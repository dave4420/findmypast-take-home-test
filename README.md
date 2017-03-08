# Building and Running

1.  [Install the Haskell Stack build tool][stack]. Install on
    macOS/Linux/\*BSD if possible, as the Haskell toolchain has never been
    terribly reliable on Windows.

[stack]: https://docs.haskellstack.org/en/stable/README/

2.  Install the compiler in a sandbox.

    Change directory to the project root, and run

     *  `stack setup`

3.  Compile the code.

     *  `stack build`

    Or, if you want to run the tests as well,

     *  `stack build --test`

4.  Run the code.   e.g.

     *  `stack exec findmypast-take-home-test text 3`
     *  `stack exec findmypast-take-home-test csv 5`


# What I'm pleased with

DAVE: what I'm pleased with


# What I would do if I had more time

DAVE: what I'd do if I had more time
