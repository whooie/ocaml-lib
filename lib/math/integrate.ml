module type Integrable = sig
  type 'a t
  type elt
  type x

  val zero : elt
  val add : elt -> elt -> elt
  val subx : x -> x -> x
  val mulx : x -> elt -> elt
  val mulf : float -> elt -> elt

  val length : 'a t -> int
  val get : 'a t -> int -> 'a option
  val to_seq : 'a t -> 'a Seq.t
  val to_seqi : 'a t -> (int * 'a) Seq.t
  val of_seq : 'a Seq.t -> 'a t
end

module type Trapz_sig = sig
  type 'a t
  type elt
  type x

  type err =
    | Too_short of string
    | Unequal_lengths of string

  type 'a res = ('a, err) result

  val trapz : x -> elt t -> elt res
  val trapz_nonuniform : x t -> elt t -> elt res
  val trapz_prog : x -> elt t -> elt t res
  val trapz_prog_nonuniform : x t -> elt t -> elt t res
end

module Trapz (I: Integrable) = struct
  type 'a t = 'a I.t
  type elt = I.elt
  type x = I.x

  type err =
    | Too_short of string
    | Unequal_lengths of string

  let err_too_short (len: int): err =
    let msg = "expected at least 2 points but got " ^ (string_of_int len) in
    Too_short msg

  let err_unequal_lengths (len1: int) (len2: int): err =
    let msg =
      "data collections must be of equal length; got "
      ^ (string_of_int len1)
      ^ " and "
      ^ (string_of_int len2)
    in
    Unequal_lengths msg

  type 'a res = ('a, err) result

  let trapz (dx: x) (y: elt t): elt res =
    let n = I.length y in
    if n < 2 then
      Error (err_too_short n)
    else
      let first = I.get y 0 |> Option.get in
      let last = I.get y (n - 1) |> Option.get in
      let mid_sum =
        I.to_seq y
        |> Seq.drop 1
        |> Seq.take (n - 2)
        |> Seq.fold_left I.add I.zero
      in
      let term_first = I.mulf 0.5 (I.mulx dx first) in
      let term_mid = I.mulx dx mid_sum in
      let term_last = I.mulf 0.5 (I.mulx dx last) in
      Ok (I.add term_first (I.add term_mid term_last))

  let trapz_nonuniform (x: x t) (y: elt t): elt res =
    let fold_fn acc ((ykm1, yk), dxkm1) =
      let term = I.mulf 0.5 (I.mulx dxkm1 (I.add ykm1 yk)) in
      I.add acc term
    in
    let n = I.length y in
    let nx = I.length x in
    if n != nx then
      Error (err_unequal_lengths n nx)
    else if n < 2 then
      Error (err_too_short n)
    else
      let dx_seq =
        I.to_seq x
        |> Seq.take (n - 1)
        |> Seq.zip (I.to_seq x |> Seq.drop 1)
        |> Seq.map (fun (xkp1, xk) -> I.subx xkp1 xk)
      in
      let y_seq =
        Seq.zip (I.to_seq y |> Seq.take (n - 1)) (I.to_seq y |> Seq.drop 1)
      in
      let ret = Seq.zip y_seq dx_seq |> Seq.fold_left fold_fn I.zero in
      Ok ret

  let trapz_prog (dx: x) (y: elt t): elt t res =
    let n = I.length y in
    if n < 2 then
      Error (err_too_short n)
    else
      let scan_fn acc (ykm1, yk) =
        let term = I.mulf 0.5 (I.mulx dx (I.add ykm1 yk)) in
        I.add acc term
      in
      let ret =
        Seq.zip (I.to_seq y |> Seq.take (n - 1)) (I.to_seq y |> Seq.drop 1)
        |> Seq.scan scan_fn I.zero
      in
      Ok (I.of_seq ret)

  let trapz_prog_nonuniform (x: x t) (y: elt t): elt t res =
    let scan_fn acc ((ykm1, yk), dxkm1) =
      let term = I.mulf 0.5 (I.mulx dxkm1 (I.add ykm1 yk)) in
      I.add acc term
    in
    let n = I.length y in
    let nx = I.length x in
    if n != nx then
      Error (err_unequal_lengths n nx)
    else if n < 2 then
      Error (err_too_short n)
    else
      let dx_seq =
        I.to_seq x
        |> Seq.take (n - 1)
        |> Seq.zip (I.to_seq x |> Seq.drop 1)
        |> Seq.map (fun (xkp1, xk) -> I.subx xkp1 xk)
      in
      let y_seq =
        Seq.zip (I.to_seq y |> Seq.take (n - 1)) (I.to_seq y |> Seq.drop 1)
      in
      let ret = Seq.zip y_seq dx_seq |> Seq.scan scan_fn I.zero |> I.of_seq in
      Ok ret
end

module type Simpson_sig = sig
  type 'a t
  type elt
  type x

  type err =
    | Too_short of string

  type 'a res = ('a, err) result

  val simpson : x -> elt t -> elt res
  val simpson_prog : x -> elt t -> elt t res
end

module Simpson (I: Integrable) = struct
  type 'a t = 'a I.t
  type elt = I.elt
  type x = I.x

  type err =
    | Too_short of string

  let err_too_short (len: int): err =
    let msg = "expected at least 3 points but got " ^ (string_of_int len) in
    Too_short msg

  type 'a res = ('a, err) result

  let simpson (dx: x) (y: elt t): elt res =
    let n = I.length y in
    if n < 3 then
      Error (err_too_short n)
    else
      let first = I.get y 0 |> Option.get in
      let last = I.get y (n - 1) |> Option.get in
      let mid_sum1 =
        I.to_seqi y
        |> Seq.drop 1
        |> Seq.take (n - 2)
        |> Seq.filter_map (fun (k, yk) -> if k mod 2 = 1 then Some yk else None)
        |> Seq.fold_left I.add I.zero
      in
      let mid_sum2 =
        I.to_seqi y
        |> Seq.drop 1
        |> Seq.take (n - 2)
        |> Seq.filter_map (fun (k, yk) -> if k mod 2 = 0 then Some yk else None)
        |> Seq.fold_left I.add I.zero
      in
      let term_first = I.mulf (1.0 /. 3.0) (I.mulx dx first) in
      let term_mid1 = I.mulf (4.0 /. 3.0) (I.mulx dx mid_sum1) in
      let term_mid2 = I.mulf (2.0 /. 3.0) (I.mulx dx mid_sum2) in
      let term_last = I.mulf (1.0 /. 3.0) (I.mulx dx last) in
      Ok (I.add term_first (I.add term_mid1 (I.add term_mid2 term_last)))

  let simps_factor (km1: int): float =
    if km1 mod 2 = 0 then 1.0 /. 3.0 else 1.0

  let simpson_prog (dx: x) (y: elt t): elt t res =
    let n = I.length y in
    if n < 3 then
      Error (err_too_short n)
    else
      let scan_fn acc ((km1, ykm1), yk) =
        let term_km1 = I.mulf (simps_factor km1) (I.mulx dx ykm1) in
        let term_k = I.mulf (1.0 /. 3.0) (I.mulx dx yk) in
        let term = I.add term_km1 term_k in
        I.add acc term
      in
      let ret =
        Seq.zip (I.to_seqi y |> Seq.take (n - 1)) (I.to_seq y |> Seq.drop 1)
        |> Seq.scan scan_fn I.zero
      in
      Ok (I.of_seq ret)
end

module type Boole_sig = sig
  type 'a t
  type elt
  type x

  type err =
    | Too_short of string

  type 'a res = ('a, err) result

  val boole : x -> elt t -> elt res
  val boole_prog : x -> elt t -> elt t res
end

module Boole (I: Integrable) = struct
  type 'a t = 'a I.t
  type elt = I.elt
  type x = I.x

  type err =
    | Too_short of string

  let err_too_short (len: int): err =
    let msg = "expected at least 5 points but got " ^ (string_of_int len) in
    Too_short msg

  type 'a res = ('a, err) result

  let boole (dx: x) (y: elt t): elt res =
    let n = I.length y in
    if n < 5 then
      Error (err_too_short n)
    else
      let mid_sum m =
        I.to_seqi y
        |> Seq.drop 1
        |> Seq.take (n - 2)
        |> Seq.filter_map (fun (k, yk) -> if k mod 4 = m then Some yk else None)
        |> Seq.fold_left I.add I.zero
      in
      let first = I.get y 0 |> Option.get in
      let last = I.get y (n - 1) |> Option.get in
      let mid_sum1 = mid_sum 1 in
      let mid_sum2 = mid_sum 2 in
      let mid_sum3 = mid_sum 3 in
      let mid_sum4 = mid_sum 0 in
      let term_first = I.mulf (14.0 /. 45.0) (I.mulx dx first) in
      let term_mid1 = I.mulf (64.0 /. 45.0) (I.mulx dx mid_sum1) in
      let term_mid2 = I.mulf (8.0 /. 15.0) (I.mulx dx mid_sum2) in
      let term_mid3 = I.mulf (64.0 /. 45.0) (I.mulx dx mid_sum3) in
      let term_mid4 = I.mulf (28.0 /. 45.0) (I.mulx dx mid_sum4) in
      let term_last = I.mulf (14.0 /. 45.0) (I.mulx dx last) in
      let ret =
        [term_first; term_mid1; term_mid2; term_mid3; term_mid4; term_last]
        |> List.fold_left I.add I.zero
      in
      Ok ret

  let boole_factor (km1: int): float =
    match km1 mod 4 with
    | 0 -> 14.0 /. 45.0
    | 1 -> 50.0 /. 45.0
    | 2 -> 10.0 /. 45.0
    | 3 -> 50.0 /. 45.0
    | _ -> assert false

  let boole_prog (dx: x) (y: elt t): elt t res =
    let n = I.length y in
    if n < 5 then
      Error (err_too_short n)
    else
      let scan_fn acc ((km1, ykm1), yk) =
        let term_km1 = I.mulf (boole_factor km1) (I.mulx dx ykm1) in
        let term_k = I.mulf (14.0 /. 45.0) (I.mulx dx yk) in
        let term = I.add term_km1 term_k in
        I.add acc term
      in
      let ret =
        Seq.zip (I.to_seqi y |> Seq.take (n - 1)) (I.to_seq y |> Seq.drop 1)
        |> Seq.scan scan_fn I.zero
      in
      Ok (I.of_seq ret)
end

module type I = sig
  type 'a t
  type elt
  type x

  type err =
    | Too_short of string
    | Unequal_lengths of string

  type 'a res = ('a, err) result

  val trapz : x -> elt t -> elt res
  val trapz_nonuniform : x t -> elt t -> elt res
  val trapz_prog : x -> elt t -> elt t res
  val trapz_prog_nonuniform : x t -> elt t -> elt t res

  val simpson : x -> elt t -> elt res
  val simpson_prog : x -> elt t -> elt t res

  val boole : x -> elt t -> elt res
  val boole_prog : x -> elt t -> elt t res

  val integrate : x -> elt t -> elt res
  val integrate_prog : x -> elt t -> elt t res
end

module Make (I: Integrable) = struct
  module Trapz = Trapz (I)
  module Simpson = Simpson (I)
  module Boole = Boole (I)

  type 'a t = 'a I.t
  type elt = I.elt
  type x = I.x

  type err =
    | Too_short of string
    | Unequal_lengths of string

  type 'a res = ('a, err) result

  let trapz (dx: x) (y: elt t): elt res =
    match Trapz.trapz dx y with
    | Ok res -> Ok res
    | Error (Trapz.Too_short msg) -> Error (Too_short msg)
    | _ -> assert false

  let trapz_nonuniform (x: x t) (y: elt t): elt res =
    match Trapz.trapz_nonuniform x y with
    | Ok res -> Ok res
    | Error (Trapz.Too_short msg) -> Error (Too_short msg)
    | Error (Trapz.Unequal_lengths msg) -> Error (Unequal_lengths msg)

  let trapz_prog (dx: x) (y: elt t): elt t res =
    match Trapz.trapz_prog dx y with
    | Ok res -> Ok res
    | Error (Trapz.Too_short msg) -> Error (Too_short msg)
    | _ -> assert false

  let trapz_prog_nonuniform (x: x t) (y: elt t): elt t res =
    match Trapz.trapz_prog_nonuniform x y with
    | Ok res -> Ok res
    | Error (Trapz.Too_short msg) -> Error (Too_short msg)
    | Error (Trapz.Unequal_lengths msg) -> Error (Unequal_lengths msg)

  let simpson (dx: x) (y: elt t): elt res =
    match Simpson.simpson dx y with
    | Ok res -> Ok res
    | Error (Simpson.Too_short msg) -> Error (Too_short msg)

  let simpson_prog (dx: x) (y: elt t): elt t res =
    match Simpson.simpson_prog dx y with
    | Ok res -> Ok res
    | Error (Simpson.Too_short msg) -> Error (Too_short msg)

  let boole (dx: x) (y: elt t): elt res =
    match Boole.boole dx y with
    | Ok res -> Ok res
    | Error (Boole.Too_short msg) -> Error (Too_short msg)

  let boole_prog (dx: x) (y: elt t): elt t res =
    match Boole.boole_prog dx y with
    | Ok res -> Ok res
    | Error (Boole.Too_short msg) -> Error (Too_short msg)

  let integrate (dx: x) (y: elt t): elt res =
    let n = I.length y in
    if n mod 4 = 1 then
      boole dx y
    else if n mod 2 = 1 then
      simpson dx y
    else
      trapz dx y

  let integrate_prog (dx: x) (y: elt t): elt t res =
    let n = I.length y in
    if n mod 4 = 1 then
      boole_prog dx y
    else if n mod 2 = 1 then
      simpson_prog dx y
    else
      trapz_prog dx y
end

