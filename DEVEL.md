# Testing

Basic regression testing: 
    
    b0 -- test
  
Other than that you can try to find zip files on your hard drive,
there's likely plenty of them. List them with:

    b0 -- zipc sniff -0 -P -r / > tmp/zipfiles.txt 

On macOS (13 as of writing) many mount points seem to lead to the same
files and the above ends up with twice the files. Rather try with:

    b0 -- zipc sniff -0 -P -r \
          /Applications /Library /Users /opt /usr > tmp/zipfiles.txt
    
You can then operate on the list in various ways. For example to check
that `zipc` can decode them, or recode them in a way that the `unzip` tool 
manages to read back:

    b0 -- zipc-for-each tmp/zipfiles.txt -- unzip --skip -t
    b0 -- zipc-for-each tmp/zipfiles.txt -- recode --deflate -t
    b0 -- zipc-for-each tmp/zipfiles.txt -- \
          recode --deflate -t --check-cmd="unzip -P '' -q -q -t"

Errors will show up on stderr in your console. Note that
`zipc-for-each FILE -- ARGS` is just a shortcut for 

    time -h xargs -0 -P8 -L1 $(b0 --path -- zipc ARGS) < FILE
    
# Testing in the browser

To open the `js_of_ocaml` tests in your browser:

    b0 -- jsoo_unzip
    b0 -- jsoo_zip

# Benchmark decompression

Compare `zipc unzip -t` to `unzip -t`:

    b0 -- time-inflate [file.zip]

If the file is unspecified it tries on `tmp/silesia.zip`: 

    mkdir tmp
    curl -L https://sun.aei.polsl.pl/~sdeor/corpus/silesia.zip > tmp/silesia.zip

The [silesia] corpus is used for benchmarking compression algorithms.

[silesia]: https://sun.aei.polsl.pl/~sdeor/index.php?page=silesia

