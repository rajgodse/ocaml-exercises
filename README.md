# ocaml-exercises

The OCaml language website has included [exercises](https://ocaml.org/problems) to learn the language. This repo contains my personal solutions, including interfaces for the values to be implemented as well as test suites for the implemented functions.

## Motivation

Most of the website exercises only contain one sample implementation of each function and one test case to be used in the top level. This project creates a formal and extensible testing framework to check the correctness of solutions. The included solutions also diverge from published solutions and are often designed with a particular programming practice in mind (for example, all functions in the list operations section are tail recursive).

Readers are encouraged to attempt to develop their own responses to all exercises, but the solutions included can provide an additional reference for correct solutions. [Note: the term "correct" is used loosely to indicate that the function in question is not known to produce unexpected output on any input. No formal verification has been performed on the included solutions!]

## Usage

This project uses the [Dune build system](https://dune.build/).

The solutions to all exercises are defined in the `lib/` directory in `.ml` files. The interface for each `lib/*.ml` file is in a corresponding `lib/*.mli` file, and the corresponding tests are in `test/*.ml` files.

To run all tests in the `test/` directory, run `dune test` from the command line.

When adding a new library or test file, the corresponding `dune` file must also be updated.

## Notes

Many included functions are implemented fully tail recursively. For more on what tail recursion is and why it's useful, read [here](https://abitofocaml.weebly.com/131-tail-recursion.html).

Some comments mention the value restriction, a fairly confusing topic. For more on the mechanics of weak variables in OCaml and the need to delay type inference via eta expansion in stateful languages with polymorphism, read [here](https://ocamlverse.github.io/content/weak_type_variables.html).
