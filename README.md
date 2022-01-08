Base project for Ocaml project on Ford-Fulkerson. This project contains some simple configuration files to facilitate editing Ocaml in VSCode.

Features :
 - Find an available path in a graph with : findpath.ml and findpah.mli
 - Ford-fulkerson algorithm on a graph with files : fordfulk.ml and fordfulk.mli
 - A concrete example of use of the Ford-Fulkerson algorithm with money sharing with files : paymentsharing.ml and paymentsharing.mli
 - Functions to manipulate files with : gfile.ml and gfile.mli
 - Functions to manipulate graphs with : graph.ml and graph.mli
 - Additional function to manipulate graphs with : tools.ml and tools.mli
 - Testing implemented algortihms about Ford-fulkerson algorithm and money sharing with file : ftest.ml 
__**WARNING : If you want to test the Ford-fulkerson algorithm you must comment out the part of Money Sharing in the code and vice versa to test Money Sharing resolver**__


A makefile provides some useful commands:
 - `make build` to compile. This creates an ftest.native executable
 - `make demo` to run the `ftest` program with some arguments for testing Ford-fulkerson algorithm -> result in `outfile`
 - `make demo2` to run the `ftest` program with some arguments for testing Money Sharing resolver -> result in `outfile2`
 - `make format` to indent the entire project
 - `make edit` to open the project in VSCode
 - `make clean` to remove build artifacts

In case of trouble with the VSCode extension (e.g. the project does not build, there are strange mistakes), a common workaround is to (1) close vscode, (2) `make clean`, (3) `make build` and (4) reopen vscode (`make edit`).



