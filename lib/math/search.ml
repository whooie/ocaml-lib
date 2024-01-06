module type NR_domain = sig
  type t
  val sub : t -> t -> t
  val div : t -> t -> t
  val clamp : t -> t
  val lt_eps : t -> float -> bool
end

module type NR = sig
  type t
  type err = No_converge of t
  val find_root : t -> (t -> t) -> (t -> t) -> float -> int -> (t, err) result
end

module Newton_raphson (D: NR_domain) = struct
  type t = D.t

  type err = No_converge of t

  let find_root
    (x0: t)
    (f: t -> t)
    (df: t -> t)
    (eps: float)
    (maxsteps: int)
    : (t, err) result
  =
    let rec doit (cur: t) (step_count: int): (t, err) result =
      if step_count = maxsteps then
        Error (No_converge cur)
      else
        let step = D.div (f cur) (df cur) in
        if D.lt_eps step eps then
          Ok cur
        else
          let next = D.clamp (D.sub cur step) in
          doit next (step_count + 1)
    in
    doit x0 0
end

let invphi = 0.618_033_988_749_894_9
let invphi2 = 0.381_966_011_250_105_1

module type GS_domain = sig
  type t
  val gen_point : (t * t) -> float -> t
  val lt_eps : (t * t) -> float -> bool
end

module type GS = sig
  type t
  type 'a err = No_converge of ((t * 'a) * (t * 'a))
  type dir = Left | Right
  val find_extremum
    : (t * t)
    -> (t -> 'a)
    -> ('a -> 'a -> dir)
    -> float
    -> int
    -> (t * 'a, 'a err) result
end
module Golden_section (D: GS_domain) = struct
  type t = D.t

  type 'a err = No_converge of ((t * 'a) * (t * 'a))

  type dir = Left | Right

  let find_extremum
    (bracket: t * t)
    (f: t -> 'a)
    (cmp: 'a -> 'a -> dir)
    (eps: float)
    (maxsteps: int)
    : (t * 'a, 'a err) result
  =
    let rec doit
      (((x0: t), (f0: 'a)),
       ((x1: t), (f1: 'a)),
       ((x2: t), (f2: 'a)),
       ((x3: t), (f3: 'a)))
      (step_count: int)
      : (t * 'a, 'a err) result
    =
      if step_count = maxsteps then
        Error (No_converge ((x0, f0), (x3, f3)))
      else if D.lt_eps (x0, x3) eps then
        match cmp f1 f2 with
        | Left -> Ok (x1, f1)
        | Right -> Ok (x2, f2)
      else
        match cmp f1 f2 with
        | Left ->
            let x1' = D.gen_point (x0, x2) invphi2 in
            let f1' = f x1' in
            doit ((x0, f0), (x1', f1'), (x1, f1), (x2, f2)) (step_count + 1)
        | Right ->
            let x2' = D.gen_point (x1, x3) invphi in
            let f2' = f x2' in
            doit ((x1, f1), (x2, f2), (x2', f2'), (x3, f3)) (step_count + 1)
    in
    let (x0, x3) = bracket in
    let x1 = D.gen_point bracket invphi2 in
    let x2 = D.gen_point bracket invphi in
    let f0 = f x0 in
    let f1 = f x1 in
    let f2 = f x2 in
    let f3 = f x3 in
    doit ((x0, f0), (x1, f1), (x2, f2), (x3, f3)) 0
end

