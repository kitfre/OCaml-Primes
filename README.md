#Primes for OCaml

A simple library for dealing with primes.

Provides the methods:
- big_primes : returns a list of n primes with type Z.t for large primes
- primes : returns a list of n primes
- is_prime : returns the primality of input as determined by the AKS test
- prime_factors : returns a list of the prime factors of the input

Dependencies:
- zarith : for arbitary precision ints for large primes
- core : for nice standard libraries

Installation:
- install with `opam install primes`

Todo:
- make "AKS" implementation closer to actual AKS test
