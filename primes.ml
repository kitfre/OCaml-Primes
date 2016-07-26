(* Returns a list of the first `top` primes, give by the sieve of
 * erasthonenes
 * Uses zarith for big_ints
*)
let big_primes top =
  let rec sieve ts = match ts with
    | [] -> []
    | (tt :: tts) ->
      let non_divisor x = Z.(mod) x tt <> Z.zero in
      let non_divisors = List.filter non_divisor tts in
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
let primes top = List.map Z.to_int (big_primes top) 

(* Lazy implementation of AKS Primality Test *)
  let big_is_prime p = let open Z 
  in
  let interval x y by = match (x = y) with
    | true  -> Gen.empty
    | false  -> Gen.unfold (fun z -> 
        if (compare x y) = (compare z y) 
        then Some (z, (add by z)) else None) x
  in
  let binom a b = 
    let frac = (Gen.fold mul one (interval a (a-b) minus_one)) /
               (Gen.fold mul one (interval b zero minus_one))  
    in
    let res = if b < a && (is_even b) then minus_one else one
    in
    frac |> mul @@ res
  in
  let expansion n =
    Gen.map (binom n) (interval n (add n one) one) 
  in  
  (Gen.drop 1 (expansion p) |> Gen.peek |> Gen.filter_map (
      function (_, None) -> None | (x,  _) -> Some x ))
  |> Gen.for_all (fun n -> rem n p = zero)

let is_prime p = big_is_prime (Z.of_int p)

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


