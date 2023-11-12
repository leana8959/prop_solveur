# About
This project is made to implement a simple solver that, given a formula $f$, finds all combinations of propositional variables that set $f$ to true.

# Inner workings
A TL;DR for the logic is that, we enumarete all possible valuations of a formula and evaluates the given formula for each valuation. We then filter out those that evaluates to false.  
This is an exponential operation, more sophisticated heuristics are planned to be implemented (I'm yet to learn them).

# Execution
1. Install `ghcup` with your favorite package manager, then install `cabal` with `ghcup`.
2. Run `cabal run prop-solveur` to execute the program.

# Read from file
Use `cabal run prop-solveur -- -f [file_name]` to run with file as input.

# Installation
If you want to use this tool anywhere, simply do `cabal install .`. Make sure that `$HOME/.cabal/bin` is in your `$PATH`.

Have fun :)
