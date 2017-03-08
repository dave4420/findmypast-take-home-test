# Building and Running

Run this on a 64 bit computer, or it will generate incorrect answers by the
5,000th prime (when the squares of the primes no longer fit into 31 bits).

1.  [Install the Haskell Stack build tool][stack]. Install on
    macOS/Linux/\*BSD if possible, as the Haskell toolchain has never been
    terribly reliable on Windows.

[stack]: https://docs.haskellstack.org/en/stable/README/

2.  Install the compiler in a sandbox.

    Change directory to the project root, and run

     *  `stack setup`

3.  Compile the code.

     *  `stack build`

4.  Run the code.   e.g.

     *  `stack exec findmypast-take-home-test text 3`
     *  `stack exec findmypast-take-home-test csv 5`

5.  Run the tests, if you like.

     *  `stack test findmypast-take-home-test-lib`


# What I'm pleased with

 *  my Sieve of Atkin worked first time
 *  I got to use a priority queue!


# What I would do if I had more time

 *  assuming we need a faster implementation:
     *  benchmark Atkins against Eratosthenes; maybe Eratosthenes is faster
     *  maybe implement another couple of prime generation algorithms,
        and benchmark them too
     *  profile my implementations
     *  there are a couple todos in Atkins.hs that could speed up my Atkins
        implementation
 *  explicitly use `Int64` instead of `Int`
 *  use a non---floating point square root calculation (if we're interested in
    primes greater than 2\*\*26)
 *  use `Integer` (arbitrarily large integer type) instead of `Int` (if we're
    interested in primes greater than 2\*\*31)
 *  use a proper options parsing library
