## [imin](http://tolkiengateway.net/wiki/Imin)

A compiler for (a subset of) Racket -> x86-64, written in Racket


## Resources

- [x86-64 for the compiler writer](http://web.cecs.pdx.edu/~apt/cs491/x86-64.pdf)
- [Indiana University compiler course](https://iucompilercourse.github.io/IU-P423-P523-E313-E513-Fall-2020/)
- [Original ICFP pearl on nanopass compilers](https://legacy.cs.indiana.edu/~dyb/pubs/nano-jfp.pdf)
- [Intel x86 Manual](http://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-manual-325462.pdf?_ga=1.200286509.2020252148.1452195021)

## Misc

The `runtime.c` file needs to be compiled and linked with the assembly
code that the compiler produces. To compile `runtime.c`, do the
following
```
   gcc -c -g -std=c99 runtime.c
```
This will produce a file named `runtime.o`. The -g flag is to tell the
compiler to produce debug information.

Next, suppose the compiler has translated the Racket program in file
`foo.rkt` into the x86 assembly program in file `foo.s` (The .s filename
extension is the standard one for assembly programs.) To produce
an executable program, do
```
  gcc -g runtime.o foo.s
```
which will produce the executable program named a.out.
