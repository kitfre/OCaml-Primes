#require "zarith"

module Primes = sig
    val big_primes : int -> Z.t list
    val primes : int -> int list
    val isPrime : int -> bool
    val primeFactors : int -> int list
end
