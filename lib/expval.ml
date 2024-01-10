type t = { v: float; e: float }

let create (v: float) (e: float): t = { v; e = Float.abs e }

type value_str_sci = No | Upper | Lower

type err =
  | Malformed_input of string
  | Unmatched_dollar
  | Parse_float_error of string

type 'a res = ('a, err) result

let to_string
  ?(trunc: bool = true)
  ?(sign: bool = false)
  ?(sci: value_str_sci = No)
  ?(latex: bool = false)
  ?(dec: int option = None)
  (ev: t)
  : string
=
  let { v = x; e = err } = ev in
  let ord_x = Float.(x |> abs |> log10 |> floor) in
  let ord_err =
    if Float.(not (is_infinite err || is_nan err) && err != 0.0) then
      Some Float.(err |> abs |> log10 |> floor)
    else
      None
  in
  let (x', err', z) =
    let ord_err_def = Option.value ord_err ~default:0.0 in
    match sci with
    | No ->
        let x' =
          Float.(
            (round (x /. (10.0 ** ord_err_def)))
            *. (10.0 ** (ord_err_def -. ord_x))
          )
        in
        let err' =
          let map o =
            Float.((round (err /. (10.0 ** o))) *. (10.0 ** (o -. ord_x)))
          in
          Option.map map ord_err
        in
        let z = Float.((max (ord_x -. ord_err_def) 0.0) |> to_int) in
        (x', err', z)
    | Upper | Lower ->
        let x' =
          Float.((round (x/. (10.0 ** ord_err_def))) *. (10.0 ** ord_err_def))
        in
        let err' =
          let map o = Float.((round (err /. (10.0 ** o))) *. (10.0 ** o)) in
          Option.map map ord_err
        in
        let z = Float.((max (neg ord_err_def) 0.0) |> to_int) in
        (x', err', z)
  in
  let z =
    match dec with
    | Some d -> Int.min d z
    | None -> z
  in
  let outstr =
    let ex =
      match sci with
      | No -> ""
      | Upper ->
          Printf.sprintf "E%s%02.0f"
          (if ord_x < 0.0 then "-" else "+")
          (Float.abs ord_x)
      | Lower ->
          Printf.sprintf "e%s%02.0f"
          (if ord_x < 0.0 then "-" else "+")
          (Float.abs ord_x)
    in
    if trunc then
      Printf.(
        sprintf "%s(%s)%s"
        (sprintf (if sign then "%+.*f" else "%.*f") z x')
        (
          match err' with
          | Some e -> sprintf "%.0f" (e *. (10.0 ** (Float.of_int z)))
          | None -> "nan"
        )
        ex
      )
    else
      Printf.(
        sprintf "%s%s %s %s%s"
        (sprintf (if sign then "%+.*f" else "%.*f") z x')
        ex
        (if latex then "\\pm" else "+/-")
        (
          match err' with
          | Some e -> sprintf "%.*f" z e
          | None -> "nan"
        )
        ex
      )
  in
  if latex then
    "$" ^ outstr ^ "$"
  else
    outstr

let of_float (v: float): t = { v; e = 0.0 }

let of_pair (ev: float * float): t = { v = fst ev; e = snd ev }

let eq (ev: t) (ev': t): bool = ev.v = ev'.v

let neq (ev: t) (ev': t): bool = ev.v != ev'.v

let compare (ev: t) (ev': t): int = Float.compare ev.v ev'.v

let ge (ev: t) (ev': t): bool = compare ev ev' > 0

let le (ev: t) (ev': t): bool = compare ev ev' < 0

let geq (ev: t) (ev': t): bool = compare ev ev' >= 0

let leq (ev: t) (ev': t): bool = compare ev ev' <= 0

let neg (ev: t): t = { v = (Float.neg ev.v); e = ev.e }

let add (l: t) (r: t): t =
  {
    v = l.v +. r.v;
    e = Float.sqrt ((Float.pow l.e 2.0) +. (Float.pow r.e 2.0));
  }

let ( <+> ) = add

let addf (l: t) (r: float): t =
  {
    v = l.v +. r;
    e = l.e;
  }

let ( <+>$ ) = addf

let fadd (l: float) (r: t): t =
  {
    v = l +. r.v;
    e = r.e;
  }

let ( $<+> ) = fadd

let sub (l: t) (r: t): t =
  {
    v = l.v -. r.v;
    e = Float.sqrt ((Float.pow l.e 2.0) +. (Float.pow r.e 2.0));
  }

let ( <-> ) = sub

let subf (l: t) (r: float): t =
  {
    v = l.v -. r;
    e = l.e;
  }

let ( <->$ ) = subf

let fsub (l: float) (r: t): t =
  {
    v = l -. r.v;
    e = r.e;
  }

let ( $<-> ) = fsub

let mul (l: t) (r: t): t =
  {
    v = l.v *. r.v;
    e =
      Float.sqrt (
        (Float.pow (l.e *. r.v) 2.0)
        +. (Float.pow (l.v *. r.e) 2.0)
      );
  }

let ( <*> ) = mul

let mulf (l: t) (r: float): t =
  {
    v = l.v *. r;
    e = l.e *. (Float.abs r);
  }

let ( <*>$ ) = mulf

let fmul (l: float) (r: t): t =
  {
    v = l *. r.v;
    e = (Float.abs l) *. r.e;
  }

let ( $<*> ) = fmul

let div (l: t) (r: t): t =
  {
    v = l.v /. r.v;
    e =
      Float.sqrt (
        (Float.pow (l.e /. r.v) 2.0)
        +. (Float.pow (r.e *. l.v /. (Float.pow r.v 2.0)) 2.0)
      );
  }

let ( </> ) = div

let divf (l: t) (r: float): t =
  {
    v = l.v /. r;
    e = l.e /. (Float.abs r);
  }

let ( </>$ ) = divf

let fdiv (l: float) (r: t): t =
  {
    v = l /. r.v;
    e = r.e *. (Float.abs l) /. (Float.pow r.v 2.0);
  }

let ( $</> ) = fdiv

let abs (ev: t): t = { v = Float.abs ev.v; e = ev.e }

let abs_sub (ev: t) (ev': t): t =
  {
    v = Float.abs (ev.v -. ev'.v);
    e = Float.sqrt ((Float.pow ev.e 2.0) +. (Float.pow ev'.e 2.0));
  }

let acos (ev: t): t =
  {
    v = Float.acos ev.v;
    e = ev.e /. Float.sqrt (1.0 -. (Float.pow ev.v 2.0));
  }

let acosh (ev: t): t =
  {
    v = Float.acosh ev.v;
    e = ev.e /. Float.sqrt ((Float.pow ev.v 2.0) -. 1.0);
  }

let asin (ev: t): t =
  {
    v = Float.asin ev.v;
    e = ev.e /. Float.sqrt (1.0 -. (Float.pow ev.v 2.0));
  }

let asinh (ev: t): t =
  {
    v = Float.asinh ev.v;
    e = ev.e /. Float.sqrt ((Float.pow ev.v 2.0) +. 1.0);
  }

let atan (ev: t): t =
  {
    v = Float.atan ev.v;
    e = ev.e /. ((Float.pow ev.v 2.0) +. 1.0);
  }

let atan2 (y: t) (x: t): t =
  {
    v = Float.atan2 y.v x.v;
    e = (
      Float.sqrt ((Float.pow (y.v *. x.e) 2.0) +. (Float.pow (y.e *. x.v) 2.0))
    ) /. ((Float.pow y.v 2.0) +. (Float.pow x.v 2.0));
  }

let atanh (ev: t): t =
  {
    v = Float.atanh ev.v;
    e = ev.e /. Float.abs ((Float.pow ev.v 2.0) -. 1.0);
  }

let cbrt (ev: t): t =
  {
    v = Float.cbrt ev.v;
    e = Float.abs (ev.e /. (Float.pow ev.v (2.0 /. 3.0)) /. 3.0);
  }

let ceil (ev: t): t =
  {
    v = Float.ceil ev.v;
    e = 0.0;
  }

let classify (ev: t): Float.fpclass = Float.classify_float ev.v

let cos (ev: t): t =
  {
    v = Float.cos ev.v;
    e = ev.e  *. (Float.abs (Float.sin ev.v));
  }

let cosh (ev: t): t =
  {
    v = Float.cosh ev.v;
    e = ev.e *. (Float.abs (Float.sinh ev.v));
  }

let exp (ev: t): t =
  let ex = Float.exp ev.v in
  {
    v = ex;
    e = ev.e *. ex;
  }

let exp2 (ev: t): t =
  let ex2 = Float.exp2 ev.v in
  {
    v = ex2;
    e = ev.e *. (Float.log 2.0) *. ex2;
  }

let expm1 (ev: t): t =
  {
    v = Float.expm1 ev.v;
    e = ev.e *. (Float.exp ev.v);
  }

let floor (ev: t): t =
  {
    v = Float.floor ev.v;
    e = 0.0;
  }

let fract (ev: t): t =
  let (_, fract) = Float.modf ev.v in
  {
    v = fract;
    e = ev.e;
  }

let hypot (x: t) (y: t): t =
  let h = Float.hypot x.v y.v in
  {
    v = h;
    e = (
      Float.sqrt ((Float.pow (x.v *. x.e) 2.0) +. (Float.pow (y.v *. y.e) 2.0))
    ) /. h;
  }

let infinity = { v = Float.infinity; e = Float.nan }

let is_finite (ev: t): bool = Float.is_finite ev.v

let is_infinite (ev: t): bool = Float.is_infinite ev.v

let is_nan (ev: t): bool = Float.is_nan ev.v

let is_sign_negative (ev: t): bool = not (Float.sign_bit ev.v)

let is_sign_positive (ev: t): bool = Float.sign_bit ev.v

let ln (ev: t): t =
  {
    v = Float.log ev.v;
    e = ev.e /. (Float.abs ev.v);
  }

let ln1p (ev: t): t =
  {
    v = Float.log1p ev.v;
    e = ev.e /. (Float.abs (ev.v +. 1.0));
  }

let log (base: t) (ev: t): t =
  let v = (Float.log ev.v) /. (Float.log base.v) in
  {
    v;
    e = (Float.sqrt
      (
        (Float.pow (ev.e /. ev.v) 2.0)
        +. (Float.pow (base.e *. v /. base.v) 2.0)
      ) /. (Float.pow (Float.log base.v) 2.0)
    );
  }

let log10 (ev: t): t =
  {
    v = Float.log10 ev.v;
    e = ev.e /. (Float.log 10.0) /. (Float.abs ev.v);
  }

let log2 (ev: t): t =
  {
    v = Float.log2 ev.v;
    e = ev.e /. (Float.log 2.0) /. (Float.abs ev.v);
  }

let max (ev: t) (ev': t): t = if Float.compare ev.v ev'.v >= 0 then ev else ev'

let max_value = { v = Float.max_float; e = 0.0 }

let min (ev: t) (ev': t): t = if Float.compare ev.v ev'.v <= 0 then ev else ev'

let min_positive_value = { v = Float.epsilon; e = 0.0 }

let min_value = { v = Float.min_float; e = 0.0 }

let nan = { v = Float.nan; e = Float.nan }

let neg_infinity = { v = Float.neg_infinity; e = Float.nan }

let pow (base: t) (exp: t): t =
  {
    v = Float.pow base.v exp.v;
    e =
      (Float.pow base.v (exp.v -. 1.0))
      *. (Float.sqrt
        (Float.pow (base.e *. exp.v) 2.0)
        +. (Float.pow (exp.e *. base.v *. (Float.log base.v)) 2.0)
      );
  }

let round (ev: t): t =
  {
    v = Float.round ev.v;
    e = 0.0;
  }

let sin (ev: t): t =
  {
    v = (Float.sin ev.v);
    e = ev.e *. (Float.abs (Float.cos ev.v));
  }

let sin_cos (ev: t): t * t = (sin ev, cos ev)

let sinh (ev: t): t =
  {
    v = Float.sinh ev.v;
    e = ev.e *. (Float.cosh ev.v);
  }

let sqrt (ev: t): t =
  let sq = Float.sqrt ev.v in
  {
    v = sq;
    e = ev.e /. sq /. 2.0;
  }

let tan (ev: t): t =
  {
    v = Float.tan ev.v;
    e = ev.e /. (Float.pow (Float.cos ev.v) 2.0);
  }

let tanh (ev: t): t =
  {
    v = Float.tanh ev.v;
    e = ev.e /. (Float.pow (Float.cosh ev.v) 2.0);
  }

let trunc (ev: t): t =
  {
    v = Float.trunc ev.v;
    e = 0.0;
  }

let one = { v = 1.0; e = 0.0 }

let zero = { v = 0.0; e = 0.0 }

let is_zero (ev: t): bool = ev.v = 0.0

