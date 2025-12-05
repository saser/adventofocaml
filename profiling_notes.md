trying to profile the solution for year 2025 day 03

following the manual released with 5.4

created opam switch like:
```shell
opam switch create default 5.4.0+options ocaml-option-fp
```

first just build with `dune build bin/bench.exe`, then invoke `perf` like
```shell
perf record -F 99 --call-graph fp _build/default/bin/bench.exe
```
and then use inferno to generate a flame graph

seeing a pretty flat flame graph with a single unnamed function taking up the majority of the time in my program

wanted to annotate source code, which the manual mentions

ran `perf annotate` but only seeing assembly

the manual mentions `-g` needs to be passed when building the program and its dependencies, so i did `dune clean` and then `dune build bin/bench.exe` again, and could see (at least in the very last command) something like:
```
Running[43]: (cd _build/default && /home/saser/.cache/dune/toolchains/ocaml-compiler.5.4.0-1c2a4a65eeb00b53959971bc34fae591/target/bin/ocamlopt.opt -w -40 -g -o bin/bench.exe ../_private/default/.pkg/base/target/lib/base/base_internalhash_types/base_internalhash_types.cmxa [...]
```
so the `-g` flag is there

at some point i re-ran clean and build and noticed this
```
configure: the threads library is supported
checking whether the assembler supports --debug-prefix-map... yes
checking whether the assembler supports CFI directives... yes
configure: not using frame pointers       <---------------
```
this was during the configure step for the OCaml compiler itself

went searching the web a little bit and found https://github.com/ocaml/ocaml/issues/10677 from which there was a link to the `configure.ac` script; i looked at the latest version and found this: 
```
## Frame pointers

if test x"$enable_frame_pointers" = "xyes"
then :
  # bunch of code
else $as_nop
  { printf "%s\n" "$as_me:${as_lineno-$LINENO}: not using frame pointers" >&5
printf "%s\n" "$as_me: not using frame pointers" >&6;}
  frame_pointers=false
fi
```

so it looks like frame pointers are not enabled after all. hmmmmmmmmmmmm

why does perf work at all then? or is it actually not working that well but still to some degree? and does it have anything to do with why source code annotation doesn't work?
