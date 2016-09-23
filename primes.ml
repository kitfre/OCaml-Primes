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

let two = Z.of_int 2
let int_pred i = i - 1
let int_succ i = i + 1

let miller_rabin_test ?(trials=100) n =
 let open Z in
 let rec factors_of_2 n =
   match n mod two with
   | x when x = zero -> int_succ (factors_of_2 (n / two))
   | _ -> 0 in
 let choose_d_and_r n =
   if (n mod two) = zero then None else
     let r_l = factors_of_2 (n - one) in
     let d = (n - one) / (Z.shift_left one r_l) in
     Some (d,r_l) in
 if n = one then false else choose_d_and_r n |> function
   | None -> false
   | Some (d,r) ->
     let rec loop i = match i with
       | i when i = trials -> true
       | _ ->
         let a = Random.float (Z.to_float (n-two*two))
           |> Z.of_float |> Z.add two in
         let x = a ** (Z.to_int d) mod n in
         match x with
         | x when x = n - one || x = one ->
           loop (int_succ i)
         | x ->
           let rec reduce r =
             let x = (x * x) mod n in
              match x with
               | x when x = one -> false
               | x when x = n - one -> loop (int_succ i)
               | _ -> if r = 0 then false else reduce (int_pred r) in
           reduce (int_pred r) in
      loop 0








