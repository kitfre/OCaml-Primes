
module Primes = struct
    (* Returns a list of the first `top` primes, give by the sieve of
     * erasthonenes *)
    let primes top =
        let rec sieve (tt :: tts) =
            let non_divisor x = (x mod tt <> 0) in
            let non_divisors = List.filter tts ~f:non_divisor in
            if non_divisors = [] then [tt] else
                (tt :: sieve non_divisors) in
        let rec range store a b =
            if a > b then store 
            else range (a :: store) (a + 1) b 
        in
        let p = List.rev (range [] 2 top) in
        sieve p
    
    (* Helper methods for Miller Rabin 
     * get_d returns d and r in the step write (n-1) as 2^r * d with d odd
     * gen_rand handles generating a random int in [2, p-2]
     *)
    let rec get_d n r = if (n mod 2 = 0) then get_d (n/2) (r+1) else (n, r)
    let gen_rand cap = let x = Random.int cap in 
        if x < 2 then 2 else x

    (* Implementation of Miller-Rabin primality test *)
    let rec miller_rabin k p = match (p mod 2 = 0) with 
        | true -> if p = 2 then true else false
        | _    -> 
            if k = 0 then true else
            let randex = gen_rand (p-2) in
            let (d,r) = get_d (p - 1) 0 in
            let x = (int_of_float ((float_of_int randex) ** (float_of_int d))) mod p in
            if (x = 1 || x = (p-1)) then miller_rabin (k-1) p else 
                inner_loop x r k p

    and inner_loop x r k p = if r = 0 then false else
        let x' = (int_of_float ((float_of_int x) ** 2.)) mod p in
        if x' = 1 then false else
        if x' = (p-1) then miller_rabin (k-1) p else
        inner_loop x' (r-1) k p

    (* Default k-value 1000 for Miller Rabin *)
    let isPrime p = miller_rabin 1000 p

    (* Int set for convenience *) 

    (* Gather prime factors using Miller_Rabin *)
    let rec primeFactors' p n acc  = if p = 1 then List.rev acc else
        match (p mod n = 0) with
        | true -> if isPrime n then primeFactors' (p / n) n 
            (if (List.hd acc = Some n) then acc else (n :: acc)) 
            else primeFactors' (p / n) n acc
        | false -> primeFactors' p (n+1) acc
    
    let primeFactors p = primeFactors' p 2 []
end
