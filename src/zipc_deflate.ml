(*---------------------------------------------------------------------------
   Copyright (c) 2023 The zipc programmers. All rights reserved.
   SPDX-License-Identifier: ISC
  ---------------------------------------------------------------------------*)

let[@inline] min x y : int = if x < y then x else y
let[@inline] clear_array a = Array.fill a 0 (Array.length a) 0
let failwithf fmt = Printf.ksprintf failwith fmt
let default_length ?len s start = match len with
| None -> String.length s - start | Some size -> size

(* Extensible byte buffer. Stdlib.Buffer falls short for [recopy]: we
   can't self-blit. Also on decode it's nice to be able to access the
   buffer directly for CRCs. *)

module Buf = struct
  type t = { mutable b : bytes; mutable len : int; fixed : bool; }
  let make ?(fixed = false) sz =
    let b = Bytes.create (if sz = 0 && not fixed then 1024 else sz) in
    { b; len = 0; fixed }

  let length buf = buf.len
  let contents buf =
    if buf.len = Bytes.length buf.b
    then Bytes.unsafe_to_string buf.b else Bytes.sub_string buf.b 0 buf.len

  let grow ~ensure buf =
    if buf.fixed (* Note, buf.fixed is only possibly [true] for decompression *)
    then failwith "Expected decompression size exceeded";
    let newlen = ref (Bytes.length buf.b) in
    let () = while !newlen < ensure do newlen := 2 * !newlen done in
    let () =
      if !newlen <= Sys.max_string_length then () else
      if ensure <= Sys.max_string_length then newlen := ensure else
      failwith "OCaml string size exceeded"
    in
    let b = Bytes.create !newlen in
    Bytes.blit buf.b 0 b 0 buf.len; buf.b <- b

  external unsafe_get_uint8 : bytes -> int -> int = "%bytes_unsafe_get"
  external unsafe_set_uint8 : bytes -> int -> int -> unit = "%bytes_unsafe_set"

  let[@inline] add_uint8 buf byte =
    let len' = buf.len + 1 in
    (if len' > Bytes.length buf.b then grow ~ensure:len' buf);
    unsafe_set_uint8 buf.b buf.len byte; buf.len <- len'

  let[@inline] add_uint16_le buf u16 =
    let len' = buf.len + 2 in
    (if len' > Bytes.length buf.b then grow ~ensure:len' buf);
    Bytes.set_uint16_le buf.b buf.len u16; buf.len <- len'

  let[@inline] add_uint32_be buf u32 =
    let len' = buf.len + 4 in
    (if len' > Bytes.length buf.b then grow ~ensure:len' buf);
    Bytes.set_int32_be buf.b buf.len u32; buf.len <- len'

  let add_string buf s ~start ~len =
    let len' = buf.len + len in
    (if len' > Bytes.length buf.b then grow ~ensure:len' buf);
    Bytes.blit_string s start buf.b buf.len len; buf.len <- len'

  let rec recopy buf ~start ~len =
    let len' = buf.len + len in
    (if len' > Bytes.length buf.b then grow ~ensure:len' buf);
    if start + len <= buf.len
    then (Bytes.blit buf.b start buf.b buf.len len; buf.len <- len')
    else begin (* overlapping, work bytewise *)
      let b = buf.b and dst_start = buf.len in
      for i = 0 to len - 1 do
        let byte = unsafe_get_uint8 b (start + i) in
        unsafe_set_uint8 b (dst_start + i) byte
      done;
      buf.len <- len';
    end
end

(* Unsigned numbers *)

type uint16 = int
type uint32 = int32

module Uint32 = struct
  type t = uint32
  let of_int = Int32.of_int
  let to_int = Int32.to_int
  let to_int32 = Fun.id
  let max_int = 0xFFFF_FFFFl
  module Syntax = struct
    let (land) = Int32.logand
    let (lor) = Int32.logor
    let (lxor) = Int32.logxor
    let (lsr) = Int32.shift_right_logical
    let (lsl) = Int32.shift_left
    let (mod) = Int32.rem
    let ( #+ ) = Int32.add
    let ( #- ) = Int32.sub
  end
end

(* CRCs. *)

let crc_error e f =
  Error (Printf.sprintf "Checksum mismatch, expected %lx found %lx)" e f)

module Crc_32 = struct
  (* Slice-by-4 technique from https://create.stephan-brumme.com/crc32 *)
  type t = uint32
  type update = uint32
  let equal = Int32.equal
  let pp ppf crc = Format.fprintf ppf "%lx" crc
  let check ~expect:e ~found:f = if equal e f then Ok () else crc_error e f
  let poly = 0xedb88320l
  let table =
    let open Uint32.Syntax in
    let t = Bigarray.Array2.create Bigarray.int32 Bigarray.c_layout 256 256 in
    for i = 0 to 0xFF do
      let c = ref (Uint32.of_int i) in
      for k = 0 to 7 do
        c := if !c land 1l <> 0l then (poly lxor (!c lsr 1)) else (!c lsr 1);
      done;
      Bigarray.Array2.set t 0 i !c
    done;
    for i = 0 to 0xFF do
      let value t k i =
        Bigarray.Array2.((get t k i lsr 8) lxor
                         (get t 0 (Uint32.to_int (get t k i land 0xFFl))))
      in
      Bigarray.Array2.set t 1 i (value t 0 i);
      Bigarray.Array2.set t 2 i (value t 1 i);
      Bigarray.Array2.set t 3 i (value t 2 i)
    done;
    t

  let init = Uint32.max_int
  let finish u = Uint32.to_int32 Uint32.Syntax.(u lxor Uint32.max_int)
  let string_update c s ~start ~len =
    let open Uint32.Syntax in
    let c = ref c in
    let i = ref start and max = (start + len - 1) - 3 in
    while (!i <= max) do
      let u = String.get_int32_le s !i in
      let u = !c lxor u in
      c :=
        Bigarray.Array2.get table 3 (Uint32.to_int ((u       ) land 0xFFl)) lxor
        Bigarray.Array2.get table 2 (Uint32.to_int ((u lsr  8) land 0xFFl)) lxor
        Bigarray.Array2.get table 1 (Uint32.to_int ((u lsr 16) land 0xFFl)) lxor
        Bigarray.Array2.get table 0 (Uint32.to_int ((u lsr 24)           ));
        i := !i + 4;
    done;
    for j = !i to max + 3 do
      let byte = Uint32.of_int (String.get_uint8 s j) in
      let k = Uint32.to_int ((!c lxor byte) land 0xffl) in
      c := (!c lsr 8) lxor (Bigarray.Array2.get table 0 k);
    done;
    !c

  let[@inline] bytes_update u b ~start ~len =
    string_update u (Bytes.unsafe_to_string b) ~start ~len

  let string ?(start = 0) ?len s =
    let len = default_length ?len s start in
    finish (string_update init s ~start ~len)
end

module Adler_32 = struct
  type t = uint32
  type update = uint32
  let equal = Int32.equal
  let pp ppf crc = Format.fprintf ppf "%lx" crc
  let check ~expect:e ~found:f = if equal e f then Ok () else crc_error e f
  let base = 65521l
  let init = 1l
  let finish crc = Uint32.to_int32 crc
  let string_update a s ~start ~len =
    let open Uint32.Syntax in
    let[@inline] byte s k = Uint32.of_int (String.get_uint8 s k) in
    let s1 = ref (a land 0xFFFFl) and s2 = ref (a lsr 16) in
    let start = ref start and max = start + len - 1 in
    let block_len = ref Stdlib.(len mod 5552) in
    while (!start <= max) do
      let i = ref !start and block_max = !start + !block_len - 1 in
      while (!i + 7 <= block_max) do
        s1 := !s1 #+ (byte s (!i    )); s2 := !s2 #+ !s1;
        s1 := !s1 #+ (byte s (!i + 1)); s2 := !s2 #+ !s1;
        s1 := !s1 #+ (byte s (!i + 2)); s2 := !s2 #+ !s1;
        s1 := !s1 #+ (byte s (!i + 3)); s2 := !s2 #+ !s1;
        s1 := !s1 #+ (byte s (!i + 4)); s2 := !s2 #+ !s1;
        s1 := !s1 #+ (byte s (!i + 5)); s2 := !s2 #+ !s1;
        s1 := !s1 #+ (byte s (!i + 6)); s2 := !s2 #+ !s1;
        s1 := !s1 #+ (byte s (!i + 7)); s2 := !s2 #+ !s1;
        i := !i + 8;
      done;
      while (!i <= block_max)
      do s1 := !s1 #+ (byte s !i); s2 := !s2 #+ !s1; incr i; done;
      s1 := !s1 mod base; s2 := !s2 mod base; start := !i; block_len := 5552;
    done;
    (!s2 lsl 16) #+ !s1

  let[@inline] bytes_update a b ~start ~len =
    string_update a (Bytes.unsafe_to_string b) ~start ~len

  let string ?(start = 0) ?len s =
    let len = default_length ?len s start in
    finish (string_update init s ~start ~len)
end

(* CRC operations *)

type crc_op = Adler_32 | Crc_32 | Nop

let crc_op_init = function
| Adler_32 -> Adler_32.init | Crc_32 -> Crc_32.init | Nop -> 0l

let crc_op_finish op crc = match op with
| Adler_32 -> Adler_32.finish crc | Crc_32 -> Crc_32.finish crc | Nop -> 0l

(* The deflate decoding code started as a port of
   https://github.com/jibsen/tinf Copyright (c) 2003-2019 Joergen Ibsen
   SPDX-License-Identifier: Zlib

   The deflate encoding code started as a port of
   https://www.hanshq.net/zip.html Hans Wennborg
   SPDX-License-Identifier: Unlicense *)

(* Deflate format data.

   Note that RFC 1951 is confusing as far as terminology is concerned,
   there are too many codes and lengths. Some of these codes are
   actually symbols (which get attributed a Huffman code). In the
   names below we try to avoid calling symbols codes. *)

let[@inline] corrupted () = raise_notrace (Failure "Corrupted data stream")

(* Litlen symbols constants *)

let litlen_sym_max = 285
let max_litlen_sym_count = litlen_sym_max + 1
let litlen_sym_fixed_max = 287 (* Values participating in construction *)
let litlen_end_of_block_sym = 256
let litlen_first_len_sym = litlen_end_of_block_sym + 1 (* First length symbol *)
let length_value_max = 258 (* Maximal length a litlen symbol can represent *)
let[@inline] length_value_base v = v lsr 4  (* Starting value *)
let[@inline] length_value_extra_bits v = v land 0xF (* Extra bits to read *)
let length_value_of_sym_table =
  (* Table of RFC 1951 3.2.5 with rows packed into an [int]. *)
  let[@inline] v bits len = (len lsl 4) lor bits in
  [|(* 257 *) v 0 3;  (* 258 *) v 0 4;  (* 259 *) v 0 5;  (* 260 *) v 0 6;
    (* 261 *) v 0 7;  (* 262 *) v 0 8;  (* 263 *) v 0 9;  (* 264 *) v 0 10;
    (* 265 *) v 1 11; (* 266 *) v 1 13; (* 267 *) v 1 15; (* 268 *) v 1 17;
    (* 269 *) v 2 19; (* 270 *) v 2 23; (* 271 *) v 2 27; (* 272 *) v 2 31;
    (* 273 *) v 3 35; (* 274 *) v 3 43; (* 275 *) v 3 51; (* 276 *) v 3 59;
    (* 277 *) v 4 67; (* 278 *) v 4 83; (* 279 *) v 4 99; (* 280 *) v 4 115;
    (* 281 *) v 5 131;(* 282 *) v 5 163;(* 283 *) v 5 195;(* 284 *) v 5 227;
    (* 285 *) v 0 258; |]

let[@inline] length_value_of_length_sym sym =
  length_value_of_sym_table.(sym - litlen_first_len_sym)

let length_value_to_sym = Lazy.from_fun @@ fun () ->
  let t = Array.make (length_value_max + 1) 0 in
  let set_codes i v =
    let base = length_value_base v and extra_bits = length_value_extra_bits v in
    for len = base to base + (1 lsl extra_bits) - 1 do t.(len) <- (257 + i) done
  in
  (* iter order is important, higher indexes overwrite lower ones *)
  Array.iteri set_codes length_value_of_sym_table; t

(* Distance symbols constants *)

let dist_sym_max = 29
let max_dist_sym_count = dist_sym_max + 1
let dist_sym_fixed_max = 31 (* Values participating in construction *)
let dist_value_max = 32768 (* Maximal distance a dist symbol can represent *)
let [@inline] dist_value_base v = v lsr 4  (* Starting value *)
let [@inline] dist_value_extra_bits v = v land 0xF (* Extra bits to read *)
let dist_value_of_sym =
  (* Table of RFC 1951 3.2.5 with rows packed into an [int]. *)
  let[@inline] v bits len = (len lsl 4) lor bits in
  [|(* 00 *) v 0 1;     (* 01 *) v 0 2;      (* 02 *) v 0 3;   (* 03 *) v 0 4;
    (* 04 *) v 1 5;     (* 05 *) v 1 7;      (* 06 *) v 2 9;   (* 07 *) v 2 13;
    (* 08 *) v 3 17;    (* 09 *) v 3 25;     (* 10 *) v 4 33;  (* 11 *) v 4 49;
    (* 12 *) v 5 65;    (* 13 *) v 5 97;     (* 14 *) v 6 129; (* 15 *) v 6 193;
    (* 16 *) v 7 257;   (* 17 *) v 7 385;    (* 18 *) v 8 513; (* 19 *) v 8 769;
    (* 20 *) v 9 1025;  (* 21 *) v 9 1537;   (* 22 *) v 10 2049;
    (* 23 *) v 10 3073; (* 24 *) v 11 4097;  (* 25 *) v 11 6145;
    (* 26 *) v 12 8193; (* 27 *) v 12 12289; (* 28 *) v 13 16385;
    (* 29 *) v 13 24577 |]

let dist_value_to_sym_table = Lazy.from_fun @@ fun () ->
  let t = Array.make 512 0 in
  let set_codes i v =
    let base = dist_value_base v and extra_bits = dist_value_extra_bits v in
    for dist = base to base + (1 lsl extra_bits) - 1 do
      let k = if dist <= 256 then dist - 1 else 256 + ((dist - 1) lsr 7) in
      t.(k) <- i;
    done
  in
  Array.iteri set_codes dist_value_of_sym; t

let[@inline] dist_value_to_sym t dist =
  t.(if dist <= 256 then dist - 1 else 256 + ((dist - 1) lsr 7))

(* Code length symbols constants *)

type codelen_symref = int (* Packs a symbol and possible repeat count *)
let codelen_symref ~repeat_bits ~sym = (repeat_bits lsl 8) lor sym
let codelen_symref_sym sref = sref land 0xFF
let codelen_symref_repeat_bits sref = sref lsr 8 (* Only for sym 16, 17 or 18 *)
let codelen_sym_max = 18
let max_codelen_sym_count = codelen_sym_max + 1
let codelen_order_of_sym_lengths =
  [| 16; 17; 18; 0; 8; 7; 9; 6; 10; 5; 11; 4; 12; 3; 13; 2; 14; 1; 15 |]

(* Huffman coding *)

module Huffman = struct
  (* As far as deflate is concerned. *)
  let max_symbol_count = litlen_sym_fixed_max + 1
  let max_code_bit_length = 15

  (* Decoding *)

  type decoder =
    { counts : int array; (* counts.(i) number of codes of length i. *)
      symbols : int array; (* symbols sorted by code *)
      mutable max_sym : int }

  let make_decoder () =
    { counts = Array.make (max_code_bit_length + 1) 0;
      symbols = Array.make max_symbol_count 0;
      max_sym = 0; }

  let fixed_litlen_decoder = Lazy.from_fun @@ fun () ->
    (* Fixed Huffman decoder for literal/length, RFC 1951 3.2.6 *)
    let t = make_decoder () in
    t.counts.(7) <- 24; t.counts.(8) <- 152; t.counts.(9) <- 112;
    for i =   0 to  23 do t.symbols.(i) <- 256 + i done;
    for i =  24 to 167 do t.symbols.(i) <- i - 24  done;
    for i = 168 to 175 do t.symbols.(i) <- 112 + i done;
    for i = 176 to 287 do t.symbols.(i) <- i - 32  done;
    t.max_sym <- litlen_sym_max (* 286 and 287 are unused *); t

  let fixed_dist_decoder = Lazy.from_fun @@ fun () ->
    (* Fixed Huffman decoder for distances, RFC 1951 3.2.6, last paragraph  *)
    let t = make_decoder () in
    t.counts.(5) <- 32; (* All 32 symbols use 5 bits *)
    for i = 0 to 31 do t.symbols.(i) <- i done;
    t.max_sym <- dist_sym_max (* 30 and 31 are unused *); t

  let scratch_decoder_offs () = Array.make 16 0
  let scratch_decoder_lengths () =
    Array.make (max_symbol_count + dist_sym_fixed_max + 1) 0

  let init_decoder
      ?scratch_offs:(offs = scratch_decoder_offs ()) t ?(start = 0)
      lengths ~lengths_len
    =
    let counts = t.counts in
    clear_array counts; t.max_sym <- -1;
    (* Count number of codes for each non-zero length *)
    for i = 0 to lengths_len - 1 do
      let len = lengths.(start + i) in
      if len <> 0 then (t.max_sym <- i; counts.(len) <- counts.(len) + 1)
    done;
    (* Compute offset table for distribution sort *)
    let available = ref 1 in
    let num_codes = ref 0 in
    for i = 0 to 15 do
      let used = counts.(i) in
      if used > !available then corrupted ();
      available := 2 * (!available - used);
      offs.(i) <- !num_codes;
      num_codes := !num_codes + used;
    done;
    (* Check all codes are used or if only one that its length is one. *)
    if (!num_codes > 1 && !available > 0) ||
       (!num_codes = 1 && counts.(1) <> 1) then corrupted ();
    (* Fill in symbols sorted by code *)
    let symbols = t.symbols in
    for i = 0 to lengths_len - 1 do
      let leni = lengths.(start + i) in
      if leni <> 0 then
        let off = offs.(leni) in
        (symbols.(off) <- i; offs.(leni) <- off + 1)
    done;
    (* For only one code (which be 0) add a code 1 which results in a symbol
       that is too large. *)
    if !num_codes = 1
    then (counts.(1) <- 2; symbols.(1) <- t.max_sym + 1;);
    ()

  (* Encoding *)

  type sym_info = int (* Bits of code in lsb (hi) and length of code (lo) *)
  let[@inline] sym_info ~code ~code_length = (code lsl 5) lor code_length
  let[@inline] sym_code v = v lsr 5 (* Code of symbol *)
  let[@inline] sym_code_length v = v land 0x1F (* Bit length of code *)

  type encoder = sym_info array
  let make_encoder () = Array.make max_symbol_count 0

  let scratch_encoder_heap () = Array.make (max_symbol_count * 2 + 1) 0
  let rec lengths_of_freqs
      ?(freq_cap = 65535) ?scratch_heap:(heap = scratch_encoder_heap ())
      e freqs ~max_sym ~max_code_len
    =
    let rec heapdown h ~max i = (* N.B. head is at 1, indexing is one-based *)
      let[@inline] compare h i j = Int.compare h.(i) h.(j) in
      let[@inline] swap h i j = let v = h.(i) in h.(i) <- h.(j); h.(j) <- v in
      let start = 2 * i in
      let l = start in (* left child index. *)
      let r = start + 1 in (* right child index. *)
      if l > max then () (* no child, stop *) else (* find smallest child k. *)
      let k = if r > max then l else (if compare h l r < 0 then l else r) in
      if compare h i k > 0 then (swap h i k; heapdown h ~max k)
    in
    let[@inline] node_freq v = v lsr 10 in
    let[@inline] node_link v = v land 0x3FF in
    let[@inline] node ~freq ~link = (freq lsl 10) lor link in
    let create_and_sort_nodes ~max_sym freqs heap = (* sort by frequency *)
      let max = ref 0 in
      for sym = 0 to max_sym do
        let freq = freqs.(sym) in
        if freq = 0 then () else begin
          let freq = if freq > freq_cap then freq_cap else freq in
          incr max; heap.(!max) <- node ~freq ~link:(max_sym + 1 + !max);
        end
      done;
      for i = !max / 2 downto 1 do heapdown heap ~max:!max i done; !max
    in
    let rec make_huffman_tree max heap =
      if max <= 1 then () else
      let new_max = max - 1 in
      let p = heap.(1) in (* get node with least frequency *)
      heap.(1) <- heap.(max); heapdown heap ~max:new_max 1;
      let q = heap.(1) in (* get next lowest frequency node *)
      let nlink = max (* Use [max] for the new node, it's unused. *) in
      let freq = node_freq p + node_freq q in
      heap.(1) <- node ~freq ~link:nlink; (* new node with combined freq *)
      (* Make p and q point to the new node. *)
      heap.(node_link p) <- nlink; heap.(node_link q) <- nlink;
      heapdown heap ~max:new_max 1; (* sort new node *)
      make_huffman_tree new_max heap
    in
    let code_lengths_of_tree e ~max_sym ~max_code_len freqs heap =
      (* Compute the code length of symbols. *)
      let max = ref 0 in
      for sym = 0 to max_sym do
        if freqs.(sym) = 0 then e.(sym) <- 0 else begin
          incr max;
          (* Link for the symbol *)
          let p = ref (heap.(max_sym + 1 + !max)) in
          (* Follow the links until the root (p = 2) *)
          let len = ref 1 in
          while (!p <> 2) do (incr len; p := heap.(!p)) done;
          if !len > max_code_len then raise_notrace Exit;
          e.(sym) <- !len;
        end
      done
    in
    let trivial_codeword_lengths e ~max_sym freqs =
      for sym = 0 to max_sym do e.(sym) <- if freqs.(sym) = 0 then 0 else 1 done
    in
    let max = create_and_sort_nodes ~max_sym freqs heap in
    if max < 2 then trivial_codeword_lengths e ~max_sym freqs else
    try
      make_huffman_tree max heap;
      code_lengths_of_tree e ~max_sym ~max_code_len freqs heap
    with Exit -> (* Flatten distribution and retry *)
      let freq_cap = freq_cap / 2 in
      lengths_of_freqs
        ~freq_cap ~scratch_heap:heap e freqs ~max_sym ~max_code_len

  let scratch_encoder_count () = Array.make (max_code_bit_length + 1) 0
  let scratch_encoder_code () = Array.make (max_code_bit_length + 1) 0
  let init_with_lengths
      ?scratch_count:(count = scratch_encoder_count ())
      ?scratch_code:(code = scratch_encoder_code ()) e ~max_sym
    =
    let[@inline] reverse_16 b =
      let b = ((b land 0xFF00) lsr 8) lor ((b land 0x00FF) lsl 8) in
      let b = ((b land 0xF0F0) lsr 4) lor ((b land 0x0F0F) lsl 4) in
      let b = ((b land 0xCCCC) lsr 2) lor ((b land 0x3333) lsl 2) in
      let b = ((b land 0xAAAA) lsr 1) lor ((b land 0x5555) lsl 1) in
      b
    in
    clear_array count; clear_array code;
    (* Compute canonical code *)
    for sym = 0 to max_sym
    do let len = sym_code_length e.(sym) in count.(len) <- count.(len) + 1 done;
    count.(0) <- 0; (* Ignore zero-length codes *)
    (* Compute first codeword of each length *)
    code.(0) <- 0;
    for len = 1 to max_code_bit_length
    do code.(len) <- (code.(len - 1) + count.(len - 1)) lsl 1 done;
    (* Assign codeword for each symbol. *)
    for sym = 0 to max_sym do
      let len = sym_code_length e.(sym) in
      if len <> 0 then begin
        let c = code.(len) in
        let bits = (reverse_16 c) lsr (16 - len) in
        e.(sym) <- sym_info ~code:bits ~code_length:len;
        code.(len) <- c + 1;
      end
    done

  let init_with_freqs
      ?scratch_heap ?scratch_count ?scratch_code e freqs ~max_sym ~max_code_len
    =
    lengths_of_freqs ?scratch_heap e freqs ~max_sym ~max_code_len;
    init_with_lengths ?scratch_count ?scratch_code e ~max_sym

  let fixed_litlen_encoder = Lazy.from_fun @@ fun () ->
    (* Fixed Huffman decoder for litlen, RFC 1951 3.2.6  *)
    let e = make_encoder () in
    for i =   0 to 143 do e.(i) <- 8 done;
    for i = 144 to 255 do e.(i) <- 9 done;
    for i = 256 to 279 do e.(i) <- 7 done;
    for i = 280 to 287 do e.(i) <- 8 done;
    init_with_lengths e ~max_sym:litlen_sym_fixed_max; e

  let fixed_dist_encoder = Lazy.from_fun @@ fun () ->
    (* Fixed Huffman decoder for litlen, RFC 1951 3.2.6  *)
    let e = make_encoder () in
    for i = 0 to 31 do e.(i) <- 5 done;
    init_with_lengths e ~max_sym:dist_sym_fixed_max; e
end

(* Inflate *)

type decoder =
  { src : string; (* Data to decompress *)
    src_max : int;
    mutable src_pos : int; (* Next read position in [src] *)
    mutable src_bits : int; (* Buffered bits *)
    mutable src_bits_len : int; (* Unread bits in [src_bits] *)
    dst : Buf.t;
    dyn_litlen : Huffman.decoder; (* Dynamic literal/length huffman decoder *)
    dyn_dist : Huffman.decoder; (* Dynamic distance huffman decoder *)
    crc_op : crc_op;
    mutable crc : Uint32.t;
    mutable crc_next : int; (* Where to start in [Buf.t] for next update. *)
    (* Scratch space to avoid allocations *)
    scratch_lengths : int array;
    scratch_offs : int array; }

let make_decoder ?decompressed_size:dsize ?(start = 0) ?len src ~crc_op =
  let len = default_length ?len src start in
  let src_max = start + len - 1 and src_pos = start in
  let src_bits = 0 and src_bits_len = 0 in
  let dst = match dsize with
  | None -> Buf.make ~fixed:false (len * 3)
  | Some d -> Buf.make ~fixed:true d
  in
  let dyn_litlen = Huffman.make_decoder () in
  let dyn_dist = Huffman.make_decoder () in
  let crc = crc_op_init crc_op and crc_next = 0 in
  let scratch_lengths = Huffman.scratch_decoder_lengths () in
  let scratch_offs = Huffman.scratch_decoder_offs () in
  { src; src_max; src_pos; src_bits; src_bits_len; dst; dyn_litlen; dyn_dist;
    crc_op; crc; crc_next; scratch_lengths; scratch_offs }

let[@inline] read_bits d ~count = (* assert (count < 32); *)
  (* The inline and refs improve things. Unfortunately decoding speed varies
     by 2-3% by making small adjustements to this function. Also careful
     with the exception. *)
  let bits = ref d.src_bits in
  let bits_len = ref d.src_bits_len in
  while !bits_len < count do
    if d.src_pos > d.src_max then (corrupted[@inlined]) () else
    (bits := !bits lor ((String.get_uint8 d.src d.src_pos) lsl !bits_len);
     d.src_pos <- d.src_pos + 1;);
    bits_len := !bits_len + 8;
  done;
  let ret = !bits land ((1 lsl count) - 1) (* mask [count] bits *) in
  d.src_bits <- !bits lsr count;
  d.src_bits_len <- !bits_len - count;
  ret

let[@inline] read_int d ~base ~bit_count =
  base + (if bit_count = 0 then 0 else read_bits d ~count:bit_count)

let read_symbol d huff =
  let rec loop d huff len base offs =
    let offs = 2 * offs + read_bits d ~count:1 in
    let count = huff.Huffman.counts.(len) in
    if offs < count then huff.symbols.(base + offs) else
    loop d huff (len + 1) (base + count) (offs - count)
  in
  loop d huff 1 0 0

let rec read_block_symbols d hlitlen hdist =
  let sym = read_symbol d hlitlen in
  if sym < litlen_end_of_block_sym
  then (Buf.add_uint8 d.dst sym; read_block_symbols d hlitlen hdist) else
  if sym = litlen_end_of_block_sym then () else
  if sym > hlitlen.max_sym || sym > litlen_sym_max || hlitlen.max_sym = -1
  then corrupted () else
  let length =
    let len = length_value_of_length_sym sym in
    let base = (length_value_base[@inlined]) len in
    let extra_bits = (length_value_extra_bits[@inlined]) len in
    read_int d ~base ~bit_count:extra_bits
  in
  let dist =
    let sym = read_symbol d hdist in
    if sym > hdist.max_sym || sym > dist_sym_max then corrupted () else
    let dist = dist_value_of_sym.(sym) in
    let base = (dist_value_base[@inlined]) dist in
    let extra_bits = (dist_value_extra_bits[@inlined]) dist in
    read_int d ~base ~bit_count:extra_bits
  in
  if dist > Buf.length d.dst then corrupted ();
  Buf.recopy d.dst ~start:(Buf.length d.dst - dist) ~len:length;
  read_block_symbols d hlitlen hdist

let read_fixed_block d =
  let litlen = Lazy.force Huffman.fixed_litlen_decoder in
  let dist = Lazy.force Huffman.fixed_dist_decoder in
  read_block_symbols d litlen dist

let read_dynamic_block d =
  let read_codelen_code d =
    (* Read lengths for Huffman code of code length symbols *)
    let hclen = read_int d ~base:4 ~bit_count:4 in
    let lengths = d.scratch_lengths in
    Array.fill lengths 0 max_codelen_sym_count 0;
    for i = 0 to hclen - 1 do
      lengths.(codelen_order_of_sym_lengths.(i)) <- read_bits d ~count:3
    done;
    let huff = d.dyn_litlen in (* Temporarily use dyn_litlen for that code *)
    let scratch_offs = d.scratch_offs and lengths_len = max_codelen_sym_count in
    Huffman.init_decoder ~scratch_offs huff lengths ~lengths_len;
    if huff.max_sym == -1 then corrupted (); (* Check code is not empty *)
    huff
  in
  let read_dynamic_codes d = (* See format in RFC 1951 3.2.7 *)
    let hlit = read_int d ~base:257 ~bit_count:5 in
    let hdist = read_int d ~base:1 ~bit_count:5 in
    if hlit > max_litlen_sym_count || hdist > max_dist_sym_count
    then corrupted ();
    let huff = read_codelen_code d in
    (* Decode code lengths for the dynamic litlen and dist Huffman code *)
    let lengths = d.scratch_lengths in
    let num = ref 0 in
    while !num < hlit + hdist do
      let sym = read_symbol d huff in
      if sym > huff.max_sym then corrupted ();
      let repeat = ref 0 in
      let sym = match sym with
      | 16 ->
          if !num = 0 then corrupted ();
          repeat := read_int d ~base:3 ~bit_count:2; lengths.(!num - 1);
      | 17 -> repeat := read_int d ~base:3 ~bit_count:3; 0
      | 18 -> repeat := read_int d ~base:11 ~bit_count:7; 0
      | sym -> repeat := 1; sym
      in
      if !repeat > hlit + hdist - !num then corrupted ();
      while !repeat > 0 do decr repeat; lengths.(!num) <- sym; incr num done;
    done;
    if lengths.(256) = 0 then corrupted ();
    let scratch_offs = d.scratch_offs in
    Huffman.init_decoder
      ~scratch_offs d.dyn_litlen lengths ~lengths_len:hlit;
    Huffman.init_decoder
      ~scratch_offs d.dyn_dist lengths ~start:hlit ~lengths_len:hdist
  in
  read_dynamic_codes d; read_block_symbols d d.dyn_litlen d.dyn_dist

let read_uncompressed_block d =
  if d.src_max - d.src_pos + 1 < 4 then corrupted ();
  let length = String.get_uint16_le d.src d.src_pos in
  let inv_length = String.get_uint16_le d.src (d.src_pos + 2) in
  if length <> ((lnot inv_length) land 0xFFFF) then corrupted ();
  d.src_pos <- d.src_pos + 4;
  if d.src_max - d.src_pos + 1 < length then corrupted ();
  let copy_max = d.src_pos + length - 1 in
  Buf.add_string d.dst d.src ~start:d.src_pos ~len:length;
  d.src_pos <- copy_max + 1; d.src_bits <- 0; d.src_bits_len <- 0

let inflated_block_crc d =
  let crc_next = Buf.length d.dst in
  let start = d.crc_next in
  let len = crc_next - start in
  d.crc_next <- crc_next;
  match d.crc_op with
  | Adler_32 -> d.crc <- Adler_32.bytes_update d.crc d.dst.Buf.b ~start ~len
  | Crc_32 -> d.crc <- Crc_32.bytes_update d.crc d.dst.Buf.b ~start ~len
  | Nop -> ()

let inflate_and_crc ?decompressed_size ?start ?len s ~crc_op =
  let d = make_decoder ?decompressed_size ?start ?len s ~crc_op in
  let rec inflate_loop d =
    let final = read_bits d ~count:1 = 1 in
    let btype = read_bits d ~count:2 in
    begin match btype with
    | 0 -> read_uncompressed_block d
    | 1 -> read_fixed_block d
    | 2 -> read_dynamic_block d
    | _ -> corrupted ()
    end;
    inflated_block_crc d;
    if final then Ok (Buf.contents d.dst, crc_op_finish d.crc_op d.crc) else
    inflate_loop d
  in
  match inflate_loop d with
  | ret -> ret
  | exception Failure e -> Error e

let inflate_and_crc_32 ?decompressed_size ?start ?len s =
  inflate_and_crc ?decompressed_size ?start ?len s ~crc_op:Crc_32

let inflate_and_adler_32 ?decompressed_size ?start ?len s =
  inflate_and_crc ?decompressed_size ?start ?len s ~crc_op:Adler_32

let inflate ?decompressed_size ?start ?len s =
  Result.map fst (inflate_and_crc ?decompressed_size ?start ?len s ~crc_op:Nop)

let zlib_decompress ?decompressed_size ?(start = 0) ?len s =
  try
    let len = default_length ?len s start in
    if len < 6 (* header and trailer *) then corrupted () else
    let cmf = String.get_uint8 s start in
    let flg = String.get_uint8 s (start + 1) in
    if (256 * cmf + flg) mod 31 <> 0 then corrupted () else
    let cm = cmf land 0x0F in
    if cm <> 8 then failwithf "Unknown compression method (%d)" cm else
    if cmf lsr 4 > 7 then failwithf "Window size too large" else
    if flg land 0x20 <> 0 then failwith "Preset dictionary unsupported" else
    let expect = String.get_int32_be s (len - 4) in
    let start = start + 2 and len = len - 4 in
    match inflate_and_adler_32 ?decompressed_size s ~start ~len with
    | Error e -> failwith e
    | Ok (_, found) as r ->
        match Adler_32.check ~expect ~found with
        | Error e -> Error (Some (expect, found), e)
        | Ok () -> r
  with
  | Failure e -> Error (None, e)

(* Deflate *)

let lz77_no_pos = -1
let lz77_window_size = 32768
let lz77_hash_bit_size = 15
let max_block_src_len =
  (* This will fit any kind of block, a huffman with only literals plus an
     end or block symbol or an uncompressed block (max 65535) *)
  65534

type level = [ `None | `Fast | `Default | `Best ]

let level_params = function
(* Level compression parameters, from Zlib
   - good_match, reduce search above this match length
   - (unused) max_lazy, do not peform lazy search above this match length
   - (unused) nice_length, quit search above this match length
   - max_chain_len, max search length in hash table chains
   good_match, max_lazy, nice_length, max_chain_len *)
| `None    -> 0, 0, 0, 0 (* Note that `None is handled specially *)
| `Fast    -> 4, 4, 8, 4
| `Default -> 8, 16, 128, 128
| `Best    -> 32, 258, 258, 4096

type backref = int
  (* Compact encoding of type backref = { dist : int; len : int }
     Literals are bytes and, given a byte [byte] we encode it as a
     backref as: { dist = 0; len = byte }. In deflate maximum for length
     is 258 which can be stored on 9 bits.  Maximum for distance is 32768
     which is 15 bits. So the whole information can stay on an OCaml
     [int] on 32-bit platforms. *)
let[@inline] make_backref ~dist ~len = (dist lsl 9) lor len
let[@inline] backref_lit l = l (* dist = 0 *)
let[@inline] backref_dist v = v lsr 9
let[@inline] backref_len v = v land 0x1FF

type encoder =
  { level : level;
    src : string; (* Data to compress *)
    src_start : int;
    src_len : int;
    dst : Buf.t;
    mutable dst_bits : int; (* Bits waiting to be added to [dst]. *)
    mutable dst_bits_len : int; (* Number of bits in [dst_bits]. *)
    block_syms : backref array; (* Symbols describing data of the block. *)
    mutable block_syms_len : int; (* Length of [block_syms] *)
    mutable block_src_start : int; (* In [src] *)
    mutable block_src_len : int;   (* In [src] *)
    litlen_sym_freqs : int array; (* of [block_syms], indexed by symbol *)
    dist_sym_freqs : int array; (* of [block_syms] indexed by dist symbol *)
    codelen_syms : codelen_symref array; (* Dyn code length sequence. *)
    mutable codelen_syms_len : int; (* Length of [code_len_syms]. *)
    codelen_sym_freqs : int array; (* of [codelen_syms] indexed by symbol. *)
    good_match : int;
    max_lazy : int;
    nice_length : int;
    max_chain_len : int;
    hash_head : int array; (* For LZ77 Rabin-Karp matching *)
    hash_prev : int array; (* For LZ77 Rabin-Karp matching *)
    dyn_litlen : Huffman.encoder; (* Dynamic literal/length encoder *)
    dyn_dist : Huffman.encoder; (* Dynamic distance encoder *)
    dyn_codelen : Huffman.encoder; (* Dynamic encoder for code lengths *)
    mutable hlit : int; (* [dyn_litlen] codes - 257 *)
    mutable hdist : int; (* [dyn_dist] codes - 1 *)
    mutable hclen : int; (* [dyn_codelen] codes - 4 *)
    crc_op : crc_op;
    mutable crc : Uint32.t;
    (* Scratch space to avoid allocations *)
    scratch_heap : int array;
    scratch_count : int array;
    scratch_code : int array;
    (* Unbox these lazy values. *)
    length_value_to_sym : int array;
    dist_value_to_sym_table : int array; }

let make_encoder ?(level = `Best) ?len ?start:(src_start = 0) src ~crc_op =
  let src_len = default_length ?len src src_start in
  let dst = Buf.make src_len and dst_bits = 0 and dst_bits_len = 0 in
  let block_syms = Array.make (max_block_src_len + 1) 0 in
  let block_syms_len = 0 in
  let block_src_start = src_start and block_src_len = 0 in
  let litlen_sym_freqs = Array.make (litlen_sym_max + 1) 0 in
  let dist_sym_freqs = Array.make (dist_sym_max + 1) 0 in
  let codelen_syms = Array.make (litlen_sym_max + dist_sym_max + 2) 0 in
  let codelen_syms_len = 0 in
  let codelen_sym_freqs = Array.make (codelen_sym_max + 1)  0 in
  let good_match, max_lazy, nice_length, max_chain_len = level_params level in
  let hash_head = Array.make (1 lsl lz77_hash_bit_size) lz77_no_pos in
  let hash_prev = Array.make lz77_window_size 0 in
  let dyn_litlen = Huffman.make_encoder () in
  let dyn_dist = Huffman.make_encoder () in
  let dyn_codelen = Huffman.make_encoder () in
  let hlit = 0 and hdist = 0 and hclen = 0 in
  let crc = crc_op_init crc_op in
  let scratch_heap = Huffman.scratch_encoder_heap () in
  let scratch_count =  Huffman.scratch_encoder_count () in
  let scratch_code = Huffman.scratch_encoder_code () in
  let length_value_to_sym = Lazy.force length_value_to_sym in
  let dist_value_to_sym_table = Lazy.force dist_value_to_sym_table in
  { level; src; src_start; src_len; dst; dst_bits; dst_bits_len;
    block_syms; block_syms_len; block_src_start; block_src_len;
    litlen_sym_freqs; dist_sym_freqs; codelen_syms; codelen_syms_len;
    codelen_sym_freqs; good_match; max_lazy; nice_length; max_chain_len;
    hash_head; hash_prev; dyn_litlen; dyn_dist; dyn_codelen; hlit; hdist; hclen;
    crc_op; crc; scratch_heap; scratch_count; scratch_code;
    length_value_to_sym; dist_value_to_sym_table; }

let new_block e =
  e.block_syms_len <- 0;
  e.block_src_start <- e.block_src_start + e.block_src_len;
  e.block_src_len <- 0;
  clear_array e.litlen_sym_freqs;
  clear_array e.dist_sym_freqs

let flush e = (* flush pending bits. *)
  if e.dst_bits_len > 0
  then (Buf.add_uint8 e.dst e.dst_bits; e.dst_bits <- 0; e.dst_bits_len <- 0)

let write_uint8 e u8 = Buf.add_uint8 e.dst u8
let write_uint16_le e u16 = Buf.add_uint16_le e.dst u16
let write_uint32_be e u32 = Buf.add_uint32_be e.dst u32
let write_bytes e s ~start ~len = Buf.add_string e.dst s ~start ~len
let write_bits e v ~count =
  e.dst_bits <- (v lsl e.dst_bits_len) lor e.dst_bits;
  e.dst_bits_len <- e.dst_bits_len + count;
  while e.dst_bits_len >= 8 do
    Buf.add_uint8 e.dst e.dst_bits;
    e.dst_bits <- e.dst_bits lsr 8;
    e.dst_bits_len <- e.dst_bits_len - 8;
  done

let write_non_compressed_block e ~final =
  let len = e.block_src_len in
  write_bits e (if final then 0b001 else 0b000) ~count:3; flush e;
  write_uint16_le e len; write_uint16_le e (Int.lognot len);
  write_bytes e e.src ~start:e.block_src_start ~len

let write_block_symbols e huffman_litlen huffman_dist =
  for i = 0 to e.block_syms_len - 1 do
    let bref = e.block_syms.(i) in
    let dist = backref_dist bref and len = backref_len bref in
    if dist = 0 then begin
      (* Write a literal or the end of block *)
      let sym_info = huffman_litlen.(len (* len the is literal symbol *)) in
      let code = Huffman.sym_code sym_info in
      write_bits e code ~count:(Huffman.sym_code_length sym_info)
    end else begin
      (* Write the length *)
      let litlen_sym = e.length_value_to_sym.(len) in
      let sym_info = huffman_litlen.(litlen_sym) in
      let bits = Huffman.sym_code sym_info in
      let count = Huffman.sym_code_length sym_info in
      let litlen_length = length_value_of_length_sym litlen_sym in
      let extra_bits = len - length_value_base litlen_length in
      let extra_bits_count = length_value_extra_bits litlen_length in
      let bits = (extra_bits lsl count) lor bits in
      write_bits e bits ~count:(count + extra_bits_count);
      (* Write the distance *)
      let dist_sym = dist_value_to_sym e.dist_value_to_sym_table dist in
      let sym_info = huffman_dist.(dist_sym) in
      let bits = Huffman.sym_code sym_info in
      let count = Huffman.sym_code_length sym_info in
      let dist' = dist_value_of_sym.(dist_sym) in
      let extra_bits = dist - dist_value_base dist' in
      let bits = (extra_bits lsl count) lor bits in
      let extra_bits_count = dist_value_extra_bits dist' in
      write_bits e bits ~count:(count + extra_bits_count)
    end
  done

let write_fixed_huffman_block e ~final =
  let huffman_litlen = Lazy.force Huffman.fixed_litlen_encoder in
  let huffman_dist = Lazy.force Huffman.fixed_dist_encoder in
  write_bits e (if final then 0b011 else 0b010) ~count:3;
  write_block_symbols e huffman_litlen huffman_dist

let write_dynamic_huffman_block e ~final =
  let write_dynamic_codes e =
    write_bits e e.hlit ~count:5;
    write_bits e e.hdist ~count:5;
    write_bits e e.hclen ~count:4;
    (* Write the codelen lengths *)
    for o = 0 to e.hclen + 4 - 1 do
      let sym = codelen_order_of_sym_lengths.(o) in
      let len = Huffman.sym_code_length e.dyn_codelen.(sym) in
      write_bits e len ~count:3
    done;
    (* Write the litlen and dist code lenghts *)
    for l = 0 to e.codelen_syms_len - 1 do
      let symref = e.codelen_syms.(l) in
      let sym = codelen_symref_sym symref in
      let sym_info = e.dyn_codelen.(sym) in
      let bits = Huffman.sym_code sym_info in
      let count = Huffman.sym_code_length sym_info in
      if sym <= 15 then write_bits e bits ~count else (* 16, 17, 18 *)
      let repeat_bits = codelen_symref_repeat_bits symref in
      let bits = (repeat_bits lsl count) lor bits in
      let rb = match sym with 16 -> 2 | 17 -> 3 | 18 -> 7 | _ -> assert false in
      write_bits e bits ~count:(count + rb)
    done;
  in
  write_bits e (if final then 0b101 else 0b100) ~count:3;
  write_dynamic_codes e;
  write_block_symbols e e.dyn_litlen e.dyn_dist

let huffman_init_with_freqs e huff freqs ~max_sym ~max_code_len =
  let scratch_heap = e.scratch_heap and scratch_count = e.scratch_count in
  let scratch_code = e.scratch_code in
  Huffman.init_with_freqs (* Use scratch space *)
    ~scratch_heap ~scratch_count ~scratch_code huff freqs ~max_sym ~max_code_len

let make_dynamic_huffman e = (* litlen and dist huffman codes for the block *)
  huffman_init_with_freqs
    e e.dyn_litlen e.litlen_sym_freqs ~max_sym:litlen_sym_max ~max_code_len:15;
  huffman_init_with_freqs
    e e.dyn_dist e.dist_sym_freqs ~max_sym:dist_sym_max ~max_code_len:15

let make_dynamic_huffman_encoding e =
  (* Prepare litlen and dist huffman code encoding. Lay down the huffman
     code lengths, encode them with the codelen symbols, compute the
     huffman tree for the codelen symbols. *)
  let gather_dynamic_huffman_code_lengths e =
    (* Write the litlen and dist huffman code lengths into e.codelen_syms
       and return total number of lengths. Trailing zeros are not written. *)
    let code_length_count h ~max_sym = (* Find last non-zero *)
      let sym = ref max_sym in
      while !sym >= 0 && Huffman.sym_code_length h.(!sym) = 0 do decr sym done;
      !sym + 1
    in
    let litlen_count = code_length_count e.dyn_litlen ~max_sym:litlen_sym_max in
    let dist_count =
      let c = code_length_count e.dyn_dist ~max_sym:dist_sym_max in
      if c <> 0 then c else
      (* We can't provide an empty code since in HDIST 0 means 1. We patch
         the encoder by giving symbol 0 a codeword of length 1. Note that
         zlib always adds at least two non zero length codes we don't do that.
         See Mark Adler's answer here https://stackoverflow.com/a/68672535 *)
      (e.dyn_dist.(0) <- Huffman.sym_info ~code:0 ~code_length:1; 1)
    in
    e.hlit <- litlen_count - 257; e.hdist <- dist_count - 1;
    let l = e.codelen_syms (* We can use that meanwhile *) in
    for i = 0 to litlen_count - 1
    do l.(i) <- Huffman.sym_code_length e.dyn_litlen.(i) done;
    for i = 0 to dist_count - 1
    do l.(litlen_count + i) <- Huffman.sym_code_length e.dyn_dist.(i) done;
    litlen_count + dist_count
  in
  let compute_codelen_syms e ~length_count =
    (* Compute the codelen symbols for the lengths in e.codelen_syms.
       N.B. We both read and write from e.codelen_syms but the
       encoding never expands so we always write on already seen lengths. *)
    let[@inline] occ e sym = (* Increase symbol occurence *)
      e.codelen_sym_freqs.(sym) <- e.codelen_sym_freqs.(sym) + 1
    in
    let rec loop e k lengths i len_max =
      if i > len_max then k (* Number of symbols encoded *) else
      match lengths.(i) with
      | 0 ->
          let max = min len_max (i + 138 - 1) (* Max amount of zeros *) in
          let j = ref (i + 1) in
          while (!j <= max && lengths.(!j) = 0) do incr j done;
          let zcount = !j - i in
          let next =
            if zcount < 3 then
              (let sym = 0 in
               e.codelen_syms.(k) <- sym; (occ[@inlined]) e sym; i + 1)
            else if zcount <= 10 then
              (let sym = 17 and repeat_bits = zcount - 3 in
               e.codelen_syms.(k) <- codelen_symref ~repeat_bits ~sym;
               (occ[@inlined]) e sym; !j)
            else
              (let sym = 18 and repeat_bits = zcount - 11 in
               e.codelen_syms.(k) <- codelen_symref ~repeat_bits ~sym;
               (occ[@inlined]) e sym; !j)
          in
          loop e (k + 1) lengths next len_max
      | sym ->
          e.codelen_syms.(k) <- sym; (occ[@inlined]) e sym;
          let max = min len_max (i + 6) in (* Could do better: seqs of 16 *)
          let j = ref (i + 1) in
          while (!j <= max && lengths.(!j) = sym) do incr j done;
          let scount = !j - i in
          if scount <= 3 then loop e (k + 1) lengths (i + 1) len_max else
          let sym = 16 and repeat_bits = scount - 3 - 1 in
          e.codelen_syms.(k + 1) <- codelen_symref ~repeat_bits ~sym;
          (occ[@inlined]) e sym; loop e (k + 2) lengths !j len_max
    in
    let lengths = e.codelen_syms (* has the lengths at that point *) in
    loop e 0 lengths 0 (length_count - 1)
  in
  let codelen_length_count h = (* Find last non-zero code length *)
    let o = ref codelen_sym_max in
    let order = codelen_order_of_sym_lengths in
    while !o > 0 && Huffman.sym_code_length h.(order.(!o)) = 0 do decr o done;
    !o + 1
  in
  let length_count = gather_dynamic_huffman_code_lengths e in
  e.codelen_syms_len <- compute_codelen_syms e ~length_count;
  huffman_init_with_freqs (* Here's our huffman code for codelen symbols *)
    e e.dyn_codelen e.codelen_sym_freqs ~max_sym:codelen_sym_max
    ~max_code_len:7;
  e.hclen <- codelen_length_count e.dyn_codelen - 4

let bit_length_of_non_compressed_block e =
  let alignment_loss = 8 - ((e.dst_bits_len + 3) mod 8) in
  3 + alignment_loss + (4 + e.block_src_len) * 8

let bit_length_of_block_symbols e hlitlen hdist =
  let acc = ref 0 in
  for sym = 0 to litlen_sym_max do
    let code_length = Huffman.sym_code_length hlitlen.(sym) in
    let extra_bits =
      if sym < litlen_first_len_sym then 0 else
      length_value_extra_bits (length_value_of_length_sym sym)
    in
    acc := !acc + e.litlen_sym_freqs.(sym) * (code_length + extra_bits);
  done;
  for sym = 0 to dist_sym_max do
    let code_length = Huffman.sym_code_length hdist.(sym) in
    let extra_bits = dist_value_extra_bits dist_value_of_sym.(sym) in
    acc := !acc + e.dist_sym_freqs.(sym) * (code_length + extra_bits);
  done;
  !acc

let bit_length_of_fixed_huffman_block e =
  let hlitlen = Lazy.force Huffman.fixed_litlen_encoder in
  let hdist = Lazy.force Huffman.fixed_dist_encoder in
  3 + bit_length_of_block_symbols e hlitlen hdist

let bit_length_of_dynamic_huffman_block e =
  let codelen_length_count = e.hclen + 4 in
  let acc = ref (3 + 5 + 5 + 4 + 3 * codelen_length_count) in
  for sym = 0 to codelen_sym_max do
    let len = Huffman.sym_code_length e.dyn_codelen.(sym) in
    let repeat_bits = match sym with 16 -> 2 | 17 -> 3 | 18 -> 7 | _ -> 0 in
    acc := !acc + e.codelen_sym_freqs.(sym) * (len + repeat_bits)
  done;
  !acc + bit_length_of_block_symbols e e.dyn_litlen e.dyn_dist

let deflated_block_src_crc e =
  let start = e.block_src_start and len = e.block_src_len in
  match e.crc_op with
  | Adler_32 -> e.crc <- Adler_32.string_update e.crc e.src ~start ~len
  | Crc_32 -> e.crc <- Crc_32.string_update e.crc e.src ~start ~len
  | Nop -> ()

let add_end_of_block_sym e =
  let sym = litlen_end_of_block_sym in
  e.block_syms.(e.block_syms_len) <- (backref_lit[@inlined]) sym;
  e.block_syms_len <- e.block_syms_len + 1;
  e.litlen_sym_freqs.(sym) <- 1

let write_block e ~final =
  deflated_block_src_crc e;
  add_end_of_block_sym e;
  make_dynamic_huffman e;
  make_dynamic_huffman_encoding e;
  let nlen = bit_length_of_non_compressed_block e in
  let flen = bit_length_of_fixed_huffman_block e in
  let dlen = bit_length_of_dynamic_huffman_block e in
  if nlen <= dlen && nlen <= flen then write_non_compressed_block e ~final else
  if flen <= dlen then write_fixed_huffman_block e ~final else
  write_dynamic_huffman_block e ~final

let write_all_non_compressed e = (* Fast path for `None level *)
  let rec loop e src_max =
    let start = e.block_src_start in
    let block_max = min src_max (start + max_block_src_len - 1) in
    let len = block_max - start + 1 and final = block_max = src_max in
    e.block_src_len <- len;
    deflated_block_src_crc e;
    write_non_compressed_block e ~final;
    if final then () else (e.block_src_start <- start + len; loop e src_max)
  in
  loop e (e.src_start + e.src_len - 1)

let[@inline] write_block_symbol e sym ~src_len = (* buffered write  *)
  if e.block_src_len + src_len > max_block_src_len
  then (write_block e ~final:false; new_block e);
  e.block_syms.(e.block_syms_len) <- sym;
  e.block_syms_len <- e.block_syms_len + 1;
  e.block_src_len <- e.block_src_len + src_len

let write_lit_symbol e byte =
  let lit = (backref_lit[@inlined]) byte in
  (write_block_symbol[@inlined]) e lit ~src_len:1;
  e.litlen_sym_freqs.(byte) <- e.litlen_sym_freqs.(byte) + 1

let write_backref_symbol e bref =
  let len = backref_len bref in
  (write_block_symbol[@inlined]) e bref ~src_len:len;
  let llsym = e.length_value_to_sym.(len) in
  e.litlen_sym_freqs.(llsym) <- e.litlen_sym_freqs.(llsym) + 1;
  let dsym = dist_value_to_sym e.dist_value_to_sym_table (backref_dist bref) in
  e.dist_sym_freqs.(dsym) <- e.dist_sym_freqs.(dsym) + 1

(* LZ77 compression with Rabin-Karp string matching. *)

module Lz77 = struct
  let min_match_len = 4
  let max_match_len = length_value_max
  let max_match_dist = 32768

  let[@inline] hash4 s i =
    let hmul = 0x9E3779B1l in
    let v = String.get_int32_le s i in
    Int32.(to_int (shift_right_logical (mul v hmul) (32 - lz77_hash_bit_size)))

  let[@inline] insert_hash e hash pos =
    e.hash_prev.(pos mod lz77_window_size) <- e.hash_head.(hash);
    e.hash_head.(hash) <- pos

  let[@inline] rec match_bwd s i j len =
    (* matches [len] bytes backwards starting at [i] and [j] *)
    if len >= 0 && (String.get_uint8 s i) = (String.get_uint8 s j)
    then match_bwd s (i - 1) (j - 1) (len - 1)
    else len

  let[@inline] rec match_fwd s i j len ~max_match_len =
    (* matches [len] bytes forward starting at [i] and [j] up to [max]. *)
    if len < max_match_len && (String.get_uint8 s i) = (String.get_uint8 s j)
    then match_fwd s (i + 1) (j + 1) (len + 1) ~max_match_len
    else len

  let[@inline] find_match_length s i j ~prev_match_len ~max_match_len =
    (* Compares substrings starting at [i] and [j]. Returns length
       of the common prefix no longer than [max_match_len] if strictly
       longer than [prev_match_len]. Otherwise returns 0. We start by looking
       at position [prev_match_len] and backwards and then forward. *)
    let newi = i + prev_match_len and newj = j + prev_match_len in
    if match_bwd s newi newj prev_match_len < 0
    then match_fwd s (newi + 1) (newj + 1) (prev_match_len + 1) ~max_match_len
    else 0

  let find_backref e pos hash ~prev_match_len ~max_match_len =
    let prev_match_len = match prev_match_len with
    | 0 -> min_match_len - 1 (* we want at least min_match_len matches *)
    | n -> n
    in
    if prev_match_len >= max_match_len then 0 else
    let chain_steps =
      let chain_steps = e.max_chain_len in
      if prev_match_len >= e.good_match then chain_steps / 4 else chain_steps
    in
    let rec loop chain_steps i ~match_pos prev_match_len =
      if i = lz77_no_pos || chain_steps = 0 || pos - i > max_match_dist
      then
        (if match_pos = lz77_no_pos then 0 else
         make_backref ~dist:(pos - match_pos) ~len:prev_match_len)
      else
      let chain_steps = chain_steps - 1 in
      let len = find_match_length e.src i pos ~prev_match_len ~max_match_len in
      if len = max_match_len then make_backref ~dist:(pos - i) ~len else
      if len <> 0 then
        loop chain_steps e.hash_prev.(i mod lz77_window_size) ~match_pos:i len
      else
        loop chain_steps e.hash_prev.(i mod lz77_window_size) ~match_pos
          prev_match_len
    in
    loop chain_steps e.hash_head.(hash) ~match_pos:lz77_no_pos prev_match_len

  let compress e = (* This function drives the block writing.  *)
    if e.level = `None then write_all_non_compressed e else
    let byte = String.get_uint8 in
    let max_pos = e.src_len - min_match_len in
    let rec loop max_pos i prev_backref =
      let prev_match_len = backref_len prev_backref in
      if i > max_pos then begin
        let i =
          if prev_match_len = 0 then i else (* Write pending previous match *)
          (write_backref_symbol e prev_backref; max_pos + prev_match_len)
        in
        (* Write remaining literals *)
        for k = i to e.src_len - 1 do write_lit_symbol e (byte e.src k) done;
        write_block e ~final:true; flush e
      end else begin
        (* Search for a match with the hash table *)
        let hash = (hash4[@inlined]) e.src i in
        let max_match_len = min max_match_len (e.src_len - i) in
        let bref = find_backref e i hash ~prev_match_len ~max_match_len in
        let match_len = backref_len bref in
        (insert_hash[@inlined]) e hash i;
        if prev_match_len != 0 && prev_match_len > match_len then begin
          (* If the previous match is at least as good as the current,
             write the previous back reference and move past match *)
          write_backref_symbol e prev_backref;
          let next = (i - 1) + prev_match_len in
          let last = min (next - 1) max_pos in
          for j = i + 1 to last
          do (insert_hash[@inlined]) e ((hash4[@inlined]) e.src j) j done;
          loop max_pos next 0
        end
        else if match_len = 0 (* No match and no previous match, output lit *)
        then (write_lit_symbol e (byte e.src i); loop max_pos (i + 1) 0)
        else begin
          (* The current back reference is better than the previous one, discard
             previous one and defer current one to see if the next is better *)
          if prev_match_len != 0 then write_lit_symbol e (byte e.src (i - 1));
          loop max_pos (i + 1) bref
        end
      end
    in
    loop max_pos e.src_start 0
end

let crc_and_deflate ?level ?start ?len s ~crc_op =
  let e = make_encoder ?level ?start ?len s ~crc_op in
  match Lz77.compress e with
  | exception Failure e (* only on 32-bit overflow *) -> Error e
  | () -> Ok (crc_op_finish e.crc_op  e.crc, Buf.contents e.dst)

let crc_32_and_deflate ?level ?start ?len s =
  crc_and_deflate ?level ?start ?len s ~crc_op:Crc_32

let adler_32_and_deflate ?level ?start ?len s =
  crc_and_deflate ?level ?start ?len s ~crc_op:Adler_32

let deflate ?level ?start ?len s =
  Result.map snd (crc_and_deflate ?level ?start ?len s ~crc_op:Nop)

let zlib_compress ?level ?start ?len s =
  let e = make_encoder ?level ?start ?len s ~crc_op:Adler_32 in
  let cmf = (7 lsl 4) (* 32k window *) lor 8 (* deflate *) in
  let flg =
    let flevel = match e.level with
    | `None -> 0 | `Fast -> 1 | `Default -> 2 | `Best -> 3
    in
    let header = cmf lsl 8 lor (flevel lsl 6) in
    (header + 31 - (header mod 31)) land 0xFF
  in
  write_uint8 e cmf; write_uint8 e flg;
  match Lz77.compress e with
  | exception Failure e (* only on 32-bit overflow *) -> Error e
  | () ->
      let crc = crc_op_finish e.crc_op e.crc in
      write_uint32_be e crc; Ok (crc, Buf.contents e.dst)
