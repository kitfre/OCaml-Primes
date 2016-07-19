open Core.Std

module Primes = struct
    (* Returns a list of the first `top` primes, give by the sieve of
     * erasthonenes
     * Uses zarith for big_ints
     *)
    let big_primes top =
        let rec sieve ts = match ts with
            | [] -> []
            | (tt :: tts) ->
                let non_divisor x = Z.(mod) x tt <> Z.zero in
                let non_divisors = List.filter tts ~f:non_divisor in
                if non_divisors = [] then [tt] 
                else (tt :: sieve non_divisors) 
        in
        (* tail recursive function for 
         * storing elements in a range *)
        let rec range store a b =
            if a > b then store 
            else range (a :: store) (Z.add a Z.one) b 
        in
        let p = List.rev (range [] (Z.of_int 2) (Z.of_int top)) 
        in
        sieve p
    
    (* does the same as big_primes, but returns
     * a list of ints instead of Z.t *)
    let primes top = List.map (big_primes top) ~f:Z.to_int 

    (* Implementation of Miller-Rabin primality test 
     * see https://en.wikipedia.org/wiki/Miller%E2%80%93Rabin_primality_test
     * for more information
     *)
    let rec miller_rabin k p =
        (* Helper methods for Miller Rabin 
         * get_d returns d and r in the step write (n-1) as 2^r * d with d odd
         * gen_rand handles generating a random int in [2, p-2]
         *)
        let rec get_d n r = if (n mod 2 = 0) then get_d (n/2) (r+1) else (n, r)
        in
        let gen_rand cap = let x = Random.int cap in 
            if x < 2 then 2 
            else x
        in
        (* if even, return right away *)
        match (p mod 2 = 0) with 
        | true -> if p = 2 then true else false
        | _    -> 
            (* otherwise run miller-rabin *)
            if k = 0 then true 
            else let randex = gen_rand (p-2) 
            in
            let (d,r) = get_d (p - 1) 0 
            in
            let x = (Int.of_float ((Float.of_int randex) ** (Float.of_int d))) mod p 
            in
            if (x = 1 || x = (p-1)) then miller_rabin (k-1) p 
            else inner_loop x r k p
    (* mutually recursive method which handles the inner loop
     * of Miller-Rabin
     *)
    and inner_loop x r k p = if r = 0 then false 
        else let x' = (int_of_float ((float_of_int x) ** 2.)) mod p 
        in
        if x' = 1 then false 
        else if x' = (p-1) then miller_rabin (k-1) p 
        else inner_loop x' (r-1) k p

    (* Default k-value 1000 for Miller Rabin 
     * makes it extremely unlikely that 
     * the test is incorrect, but still
     * fast
     *)
    let is_prime p = miller_rabin 1000 p

    (* Gather prime factors using Miller_Rabin *)
    let prime_factors p = 
        let rec divide_out p n = if p mod n = 0 then divide_out (p/n) n
        else p
        in
        let rec prime_factors' p n acc  = if p = 1 then List.rev acc 
        else match (p mod n = 0) with
            | true -> if is_prime n then prime_factors' (divide_out p n) (n+1) (n :: acc)
                      else prime_factors' (p / n) n acc
            | false -> prime_factors' p (n+1) acc
        in
        prime_factors' p 2 []        
end
