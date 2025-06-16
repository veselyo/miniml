# MiniML

## Description

This project is an interpreter for MiniML, a compact, OCaml-like functional
programming language. It delves into the core concepts of programming language
design and implementation by building multiple evaluators based on different
operational semantics.

The primary goal is to understand and demonstrate how variations in evaluation
strategies—specifically substitution, dynamic scoping, and lexical scoping—can
profoundly impact program behavior and language capabilities.

## Features

*   **Multiple Evaluation Semantics Implemented:**
    *   **Substitution Model (`eval_s`):** A foundational model where names
        are directly replaced by their values.
    *   **Dynamically-Scoped Environment Model (`eval_d`):** Variables are
        resolved based on the runtime call stack.
    *   **Lexically-Scoped Environment Model (`eval_l`):** Variables are
        resolved based on the textual structure of the code, implemented
        using **closures**. This is the standard in most modern languages.
*   **Interactive REPL (Read-Eval-Print Loop):**
    Allows users to type MiniML expressions and see immediate results.
*   **Simultaneous Semantics Comparison:** The REPL uniquely displays the
    output of an expression as evaluated by *all three* semantics
    (substitution, dynamic, and lexical) side-by-side, offering a clear
    and immediate comparison.
*   **Core Functional Language Constructs:** Supports integers, booleans,
    arithmetic and comparison operators, conditionals, first-class functions,
    `let` bindings, and recursive functions (`let rec`).
*   **Informative Error Handling:** Provides clear diagnostics for common
    issues like syntax errors, type mismatches, or unbound variables.
*   **Well-Tested Implementation:** Includes a comprehensive suite of unit
    tests ([tests.ml](tests.ml)) to verify the correctness of evaluators and
    language features.

## Demo
Here, you can see a demo of the MiniML interpreter in action:

*Insert demo here*

## Contributors
- Ondrej Vesely – Author

## Acknowledgements
- The initial project framework was provided by the CS51 Course Staff.