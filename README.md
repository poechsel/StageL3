# Internship

Software wrote during my Internship at le CRI, Mines Paristesch, under the supervision of Mr. Claude Tadonki

Under the folder rapport/ you can find a small article about the theory behind this code (in french).

This software, given a C code, automatically generate optimized data transfers to parallelize this C code with **OpenACC**.


## Compile
To compile this project, you must have OCaml installed with ocamlbuild. Just type `make` to compile

## Use
`./main.native file.c` will generate the code (data transfers + code itself) corresponding to the file `file.c`
To compile the outputed code you must link the file "library.h"

## What it can do:
This tool will generate optimize data transfer given a C code.

For example, for a file.c containing :
```
for (int i = 0; i < n; ++i) {
    A[i+m] = A[i];
}
```
this tool will generate :
    - a host to device transfer for `A[0 : n-1]` and device to host transfer for `A[m : m + n -1]` if `m >= n`
    - a host to device and a device to host transfer for `A[0 : n + m - 1]` otherwise
    

