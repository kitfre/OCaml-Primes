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
    
    (* Lazy implementation of AKS Primality Test *)
    let is_prime p =
        let interval x y by = match (x = y) with
        | true  -> Gen.empty
        | false  -> Gen.unfold (fun z -> 
            if (compare x y) = (compare z y) 
            then Some (z, (by + z)) else None) x
        in
        let binom a b = 
            let frac = (Gen.fold ( * ) 1 (interval a (a-b) (-1))) /
            (Gen.fold ( * ) 1 (interval b 0 (-1))) 
            in 
            let res =
            (if compare  b a < 0 && (b mod 2 = 0)
            then -1
            else 1) 
            in
            frac |> ( * ) @@ res
        in
        let expansion n =
            Gen.map (binom n) (interval 0 (n+1) 1) 
        in
        (Gen.drop 1 (expansion p) |> Gen.peek |> Gen.filter_map (
        function (_, None) -> None | (x,  _) -> Some x ))
        |> Gen.for_all (fun n -> n mod p = 0)
    
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
