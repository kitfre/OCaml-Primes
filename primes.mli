module Primes : sig
    
    (* uses Sieve of Eratosthenes to find the first n primes. Returns a Z.t list
     * in order to accomodate for large primes. *)
    val big_primes : int -> Z.t list

    (* uses Sieve of Erathosthenes to find the first n primes. Returns an int
     * list, calls big_primes then maps Z.to_int over result.
     * USE WHEN OVERFLOW IS NOT AN ISSUE
     *)
    val primes : int -> int list

    (* Uses a lazy implementation of the AKS Primality Test to determine
     * primality of input.
     * See https://en.wikipedia.org/wiki/AKS_primality_test
     * for more information
     *)
    val is_prime : int -> bool

    (* Uses AKS to find primes and repeatedly divides them out of the
     * input, collecting them along the way. Returns an int list of prime
     * factors of the input in ascending order.
     *)
    val prime_factors : int -> int list
end
