
### Simple Regex matching engine

### Installation

To build and run, opam is required

Mac:
`brew install opam` or `bash -c "sh <(curl -fsSL https://opam.ocaml.org/install.sh)"`

Ubuntu:
`sudo apt install opam`

The dune build system is used for this project, which can be installed with:
-`opam init`
-`opam install dune`

and invoked with 

-`dune build`
-`dune test`

Nothing is currently implemented for the main entry point so `dune run` won't do anything. 
Dune test, however, will give a printout for each invoked test case. 