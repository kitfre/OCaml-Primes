#Primes for OCaml

A simple library for dealing with primes.

Provides the methods:
- big_primes : returns a list of n primes with type Z.t for large primes
- primes : returns a list of n primes
- big_is_prime: returns primality of Zarith input
- is_prime : returns the primality of int input as determined by the AKS test
- prime_factors : returns a list of the prime factors of the input

Dependencies:
- zarith : for arbitary precision ints for large primes
- gen : lazy generators for AKS test

Installation:
- install with `opam install primes`
