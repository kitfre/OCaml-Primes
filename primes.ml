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
    
    (* tests primality of p by AKS Primality Test *)
    let is_prime p = 
    (* Defined scanl, which is like fold but it creates  
     * a list of the results *)
    let scanl = let rec scanl' acc f q ls =
        match ls with 
            | [] -> acc
            | a::aas -> scanl' (q::acc) f (f q a) aas 
        in
         scanl' []
    in
    (* Creates a list [t..f]*)
    let range t f = let rec range' t' f' acc = match (f' = t') with
        | true -> (f' :: acc)
        | _    -> range' t' (f'-1)  (f' :: acc)
        in
        range' t f []
    in
    (* calculates the pth level of pascals triangle *)
    let binom p = scanl (fun z i -> z * (p-i+1) / i) 1 (range 1 p)
    in
    (* removes last element of list *)
    let rm_last l = (List.rev (match (List.tl (List.rev l)) with | Some x -> x |
    None -> []))
    in
    (* drops leading and trailing 1 from binom p *)
    let binom' p = match (rm_last (binom p)) with 
        | (x::xs) -> xs
        | [] -> []
    in
    (* reduces a list of booleans with and *)
    let and' t = List.fold t ~init:true ~f:((&&))
    in
    (* if p < 2 then trivially not prime
     * else use the AKS test for primality *)
    match (p < 2) with
        | true  -> false
        | false -> and' (List.map (binom' p) ~f:(fun n -> n mod p = 0))
    
    (* Gather prime factors using Aks*)
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
