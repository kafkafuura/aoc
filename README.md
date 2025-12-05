# Advent of Code (AOC)

Advent of Code Solutions (2018-2025) (OCaml, F#, Rust, Python) 

This is an OCaml first repo, other language solutions were completed only after OCaml solutions were found.

General Format:

- Solutions are functions within a single file (e.g., aoc.ml) and expect inputs to be "01.txt" (and "01e.txt" for examples if the example flag is set).
- Functions names are (generally) of the form problem\_01a / problem\_01b if split, or problem\_01 if combined. Some problems have alternative / optimized solutions (e.g., problem\_25a2)
- Most functions are completely self contained (you can copy just the function to another file to compile and run).
- With a few exceptions, solutions use only the standard library of the language.

These solutions are meant to be used as reference - some are very inefficent.

Most OCaml solutions are efficent enough to be run from the REPL (ocaml -> #use "aoc.ml" ;; -> problem\_25a ();;), but will run significantly faster if compiled.

If the ocaml fails to compile, try upgrading to version 5.4! (2018 uses named tuples (5.4); 2019 uses effects (5.3); 2018, 2019, and 2024 use Dynarray (5.2))

Example:

```{.ml}
(* aoc_opt.ml *)
(* copy problem_20b from aoc.ml *)
(* double check the signature! some are unit -> unit, not unit -> int, etc. *)
let _ = print_int @@ problem_20b () ; print_newline ()
```

```{.sh}
ocamlopt -O3 -o aoc_opt.exe aoc_opt.ml && ./aoc_opt.exe
```

All F# solutions are meant to be run through the REPL. (dotnet fsi -> #load "aoc.fsx" ;; -> Aoc.problem\_25a () ;;)

**Use at your own risk!**

## 2025

- OCaml: 01-05

## 2024

- OCaml: 01-25
- F#: 01-25

## 2023

- OCaml: 01-25
- F#: 01-15, 24b, 25
- Rust: 01-15
- Python: 01-09, 11-15

- 24b and 25a require LAPACK
  + F# uses a dll/so-file directly (AOCL-LibFlame-Win-dll.dll)
  + Ocaml has lacaml solutions + hand-written ffi bindings (for ocaml msvc)

## 2022

- OCaml: 01-25
- F#: 19, 23b, 24b

## 2021

- OCaml: 01-22,25 (*23-24 were solved with pen and paper*)
- F#: 19b

- aoc.ml is my first attempt in 2021 (formatted differently), aoc2.ml is when I returned to complete the rest of the unsolved problems.

## 2020

- OCaml: 01-25

## 2019

- OCaml: 01-25

## 2018

- OCaml: 01-25
