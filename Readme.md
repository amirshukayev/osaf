# OSAF

## Installation:
Osaf uses Dune as the build tool. This requires Opam, 
the official package manager for OCaml. Verify your 
Opam installation using the following command:

> opam --version

installation guide here: https://opam.ocaml.org/doc/Install.html
I used Opam version 2.0.5 when making this project.

To install Dune, run. I also installed utop which is the 
OCaml REPL tool

> opam install dune
> opam install utop

## Useful compiler stuff

> ocamlopt

-drawflambda  Print Flambda terms after closure conversion
-dflambda-invariants  Check Flambda invariants around each pass
-dflambda-no-invariants  Do not Check Flambda invariants around each pass
-dflambda-let <stamp>  Print when the given Flambda [Let] is created
-dflambda-verbose  Print Flambda terms including around each pass

to get flambda
> opam switch create 4.08.1+flambda 
