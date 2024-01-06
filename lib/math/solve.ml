module type Integrable = sig
  type 'a t
  type elt
  type x

  val add : elt -> elt -> elt
  val addx : x -> x -> x
  val subx : x -> x -> x
  val mulx : x -> elt -> elt
  val mulf : float -> elt -> elt
  val mulfx : float -> x -> x

  val length : 'a t -> int
  val to_seq : 'a t -> 'a Seq.t
  val of_seq : 'a Seq.t -> 'a t
end

module type RK4_sig = sig
  type 'a t
  type elt
  type x

  val rk4_step : (x -> elt -> elt) -> x -> x -> elt -> elt
  val rk4 : (x -> elt -> elt) -> x t -> elt -> elt t
end

module RK4 (I: Integrable) = struct
  type 'a t = 'a I.t
  type elt = I.elt
  type x = I.x

  let rk4_step (rhs: x -> elt -> elt) (dx: x) (x: x) (y: elt): elt =
    let x_half = I.addx x (I.mulfx 0.5 dx) in
    let k1 = rhs x y in
    let k2 = rhs x_half (I.add y (I.mulf 0.5 (I.mulx dx k1))) in
    let k3 = rhs x_half (I.add y (I.mulf 0.5 (I.mulx dx k2))) in
    let k4 = rhs (I.addx x dx) (I.add y (I.mulx dx k3)) in
    let k = I.add k1 (I.add (I.mulf 2.0 (I.add k2 k3)) k4) in
    I.add y (I.mulf (1.0 /. 6.0) (I.mulx dx k))

  let rk4 (rhs: x -> elt -> elt) (x: x t) (y0: elt): elt t =
    let scan_fn (y0: elt) ((x: x), (dx: x)): elt = rk4_step rhs dx x y0 in
    let nx = I.length x in
    Seq.zip (I.to_seq x |> Seq.drop 1) (I.to_seq x |> Seq.take (nx - 1))
    |> Seq.map (fun (xn, xp) -> (I.subx xn xp, xp))
    |> Seq.scan scan_fn y0
    |> I.of_seq
end

module type Integrable_adaptive = sig
  type 'a t
  type elt
  type x

  val stepsize_maxiters : int

  val norm : elt -> float
  val add : elt -> elt -> elt
  val addx : x -> x -> x
  val sub : elt -> elt -> elt
  val subx : x -> x -> x
  val mulx : x -> elt -> elt
  val mulf : float -> elt -> elt
  val mulfx : float -> x -> x

  val comparex : x -> x -> int

  val length : 'a t -> int
  val to_seq : 'a t -> 'a Seq.t
  val of_seq : 'a Seq.t -> 'a t
end

module type RK4_adaptive_sig = sig
  type 'a t
  type elt
  type x

  type err =
    | No_converge of string

  type 'a res = ('a, err) result

  val rka_step
    : (x -> elt -> elt)
    -> float
    -> x
    -> x
    -> elt
    -> (x * elt * x) res

  val rka
    : (x -> elt -> elt)
    -> float
    -> x
    -> (x * x)
    -> elt
    -> ((x t) * (elt t)) res
end

module RK4_adaptive (I: Integrable_adaptive) = struct
  module R = RK4 (I)

  type 'a t = 'a I.t
  type elt = I.elt
  type x = I.x

  type err =
    | No_converge of string

  let no_converge =
    No_converge "failed to converge to desired error bound"

  type 'a res = ('a, err) result

  (* safety numbers -- particular to rk4 *) 
  let (safe1, safe2, safe2_inv) = (0.9, 4.0, 0.25)

  let error_ratio (y0: elt) (y1: elt) (err0: float): float =
    let scale = err0 *. ((I.norm y0) +. (I.norm y1)) /. 2.0 in
    let diff = I.norm (I.sub y0 y1) in
    diff /. (scale +. Float.epsilon)

  let rka_step
    (rhs: x -> elt -> elt)
    (err: float)
    (dx: x)
    (x: x)
    (y: elt)
    : (x * elt * x) res
  =
    let rec doit (iter_count: int) (dx0: x): (x * elt * x) res =
      if iter_count > I.stepsize_maxiters then
        Error no_converge
      else
        (* take two half-sized steps *)
        let dx_half = I.mulfx 0.5 dx0 in
        let x_half = I.addx x dx_half in
        let y_half = R.rk4_step rhs dx_half x y in
        let y_half2 = R.rk4_step rhs dx_half x_half y_half in
        (* take one full-sized step *)
        let x_full = I.addx x dx0 in
        let y_full = R.rk4_step rhs dx0 x y in
        (* compute the estimated local truncation error *)
        let e = error_ratio y_half2 y_full err in
        (* estimate new step size (with safety factors) *)
        if e = 0.0 then
          doit (iter_count + 1) (I.mulfx safe2_inv dx0)
        else
          let dx' = I.mulfx (safe1 *. (Float.pow e (-0.2))) dx0 in
          let dx_cond1 = I.mulfx safe2_inv dx0 in
          let dx_cond2 = I.mulfx safe2 dx0 in
          let dx' = if dx_cond1 > dx' then dx_cond1 else dx' in
          let dx' = if dx_cond2 < dx' then dx_cond2 else dx' in
          if e < 1.0 then
            Ok (x_full, y_half2, dx')
          else
            doit (iter_count + 1) dx'
    in
    doit 0 dx

  let rka
    (rhs: x -> elt -> elt)
    (err: float)
    (dx_init: x)
    (x_bounds: x * x)
    (y: elt)
    : ((x t) * (elt t)) res
  =
    let (x_start, x_stop) = x_bounds in
    let rec doit (dx: x) (x: x) (y: elt): (x * elt) Seq.t res =
      if I.comparex x x_stop >= 0 then
        Ok (fun () -> Seq.Cons ((x, y), fun () -> Seq.Nil))
      else
        let dx' =
          let x_rem = I.subx x_stop x in
          if I.comparex dx x_rem < 0 then dx else x_rem
        in
        match rka_step rhs err dx' x y with
        | Error e -> Error e
        | Ok (x_next, y_next, dx_next) ->
            match doit dx_next x_next y_next with
            | Error e -> Error e
            | Ok points ->
                Ok (fun () -> Seq.Cons ((x_next, y_next), points))
    in
    match doit dx_init x_start y with
    | Error e -> Error e
    | Ok points ->
        let (xs, ys) = Seq.unzip points in
        Ok (I.of_seq xs, I.of_seq ys)
end

