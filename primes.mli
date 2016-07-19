module Primes : sig
    
    (* uses Sieve of Eratosthenes to find the first n primes. Returns a Z.t list
     * in order to accomodate for large primes. *)
    val big_primes : int -> Z.t list

    (* uses Sieve of Erathosthenes to find the first n primes. Returns an int
     * list, calls big_primes then maps Z.to_int over result.
     * USE WHEN OVERFLOW IS NOT AN ISSUE
     *)
    val primes : int -> int list

    (* Uses the Miller-Rabin Probabalistic Primality test to decide the
     * primality of the input. Runs the test 1000 times by default in order
     * to provide a very high level of accuracy while maintaining speed.
     * See https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
     * for more information on the test itself.
     * Can configure the number of times the test is run by calling
     * Primes.miller_rabin k p where k is the number of iterations and p is the
     * input to test.
     *)
    val is_prime : int -> bool

    (* Uses Miller-Rabin to find primes and repeatedly divides them out of the
     * input, collecting them along the way. Returns an int list of prime
     * factors of the input in ascending order.
     *)
    val prime_factors : int -> int list
end
