## [imin](http://tolkiengateway.net/wiki/Imin)

A compiler for (a subset of) Racket -> x86-64, written in Racket

## TODO

- Add tests that have nested `if`s in then/else positions
- Add tests that have `if`s in assignments

## Resources

- [Harvard assembly tutorial](https://cs61.seas.harvard.edu/site/2019/Asm)
- [x86-64 for the compiler writer](http://web.cecs.pdx.edu/~apt/cs491/x86-64.pdf)
- [x86-64 cheatsheet](https://www.cs.cmu.edu/~fp/courses/15213-s07/misc/asm64-handout.pdf)
- [Indiana University compiler course](https://iucompilercourse.github.io/IU-P423-P523-E313-E513-Fall-2020/)
- [Original ICFP pearl on nanopass compilers](https://legacy.cs.indiana.edu/~dyb/pubs/nano-jfp.pdf)
- [Andy Keep dissertation on nanopass compilers](http://andykeep.com/pubs/dissertation.pdf)
- [An Incremental Approach to Compiler
  Construction](http://scheme2006.cs.uchicago.edu/11-ghuloum.pdf), [Nada Amin's
  implementation](https://github.com/namin/inc) (which *can* compile itself!)
- [Partial computation, Yoshihiko Futamura](https://repository.kulib.kyoto-u.ac.jp/dspace/bitstream/2433/103401/1/0482-14.pdf)
- [Interference graphs of programs in SSA-form](https://compilers.cs.uni-saarland.de/papers/ifg_ssa.pdf)
- [Racket nanopass library](https://docs.racket-lang.org/nanopass/index.html)
- [Intel x86 Manual](http://www.intel.com/content/dam/www/public/us/en/documents/manuals/64-ia-32-architectures-software-developer-manual-325462.pdf?_ga=1.200286509.2020252148.1452195021)

## Adding compiler tests

1. Add a `.rkt` test file in `/tests`, e.g.
```
; /tests/var_test_12.rkt
(let ([x (read)])
  (+ x 2))
```
2. Add a `.in` file containing inputs to all the `read` calls, e.g.
```
; /tests/var_test_12.in
3
```
3. Add a `.res` file containingt the expected output, e.g.
```
; /test/var_test_12.res
5
```
4. Run tests
```
racket run-tests.rkt
```

## Debugging assembly with gdb

1. Compile the `.s` file with `-g` to generate debugging symbols, e.g.
```
  gcc -g var_test_11.s
```
2. Load the `.out` file into gdb
```
  gdb var_test_11.out
```
3. Set break points by passing addresses via `<label> + <offset>`, e.g.
```
  (gdb) info break
  No breakpoints or watchpoints.
  (gdb) break *main+2
  Breakpoint 1 at 0x1124: file var_test_11.s, line 8.
```
4. Use `run` to step through the program, stopping at each break point and
   `cont` to continue till the next breakpoint. Use `info registers` to inspect
   register contents, `print/<bxd> <reg>` to print contents of register `<reg>`
   in binary/hex/decimal, e.g.
   ```
     (gdb) print/d $rax
   ```
   prints the contents of register `rax` in decimal
5. Use `info frame` to show stack frame info and `x/<offset><bxd><bhwg> <addr>`
   to examine contents at that address. E.g.
   ```
     (gdb) x/4xw $sp
   ```
   prints "four words (`w`) of memory above the stack pointer (here, `$sp`) in
   hexadecimal (`x`)".

## Editing `run-tests.rkt`

- The `passes` variable ins `run-tests.rkt` runs the passes *in the very order*.

## Using the runtime for IO

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

## Interesting bugs so far

- In the shrink pass, I had implemented `<=` as
    ```
    (match e
      ...
      [(Prim '<= `(,e1 ,e2))
       (let ([e1 (shrink-exp e1)]
             [e2 (shrink-exp e2)])
         (If (Prim '< `(,e1 ,e2)) (Bool #t) (Prim 'eq? `(,e1 ,e2))))]
      ...)
    ```
    how ever, this duplicates `e1` and `e2`! A compiler must *never* duplicate
    code -- in this case, this duplication can introduce issues if either of
    them are a `(read)` call, since the compiled program may potentially have to
    read the file twice instead of once!

## Notes

- `rsp` must always be of the form `8 + 16 * n`, for some natural `n`.
    If `pushq`s/`popq`s break this alignment, it must be realigned via `sub`q.
    For instance, if `rsp` (which always gets `pushq`ed in all functions) and
    `rbx` (say function uses it locally) get `pushq`ed, then the stack is
    currently aligned to 8 + 8 = 16 bytes, which isn't 8+16*n. So
    `subq $8, %rsp` must be generated to realign to 8+16*n.
- Stuff that lazy explicate control achieves
    1. Avoids duplicate block generation
    2. Avoids dead block generation
