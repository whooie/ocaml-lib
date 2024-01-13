module Const = struct
  let h = 6.626070040e-34         (* +/- 0 (exact) *)

  let hbar = h /. 2.0 /. Float.pi (* +/- 0 (exact) *)

  let c = 2.99792458e+8           (* +/- 0 (exact) *)

  let nA = 6.022140857e+23        (* +/- 0 (exact) *)

  let kB = 1.38064852e-23         (* +/- 0 (exact) *)

  let e0 = 8.8541878128e-12       (* +/- 0.0000000013e-12 *)

  let u0 = 1.25663706212e-6       (* +/- 0.00000000019e-6 *)

  let gG = 6.67430e-11             (* +/- 0.00015e-11 *)

  let g = 9.80665                 (* +/- 0 (exact) *)

  let e = 1.602176634e-19         (* +/- 0 (exact) *)

  let me = 9.1093837015e-31       (* +/- 0.0000000028e-31 *)

  let mp = 1.67262192369e-27      (* +/- 0.00000000051e-27 *)

  let mn = 1.67492749804e-27      (* +/- 0.00000000095e-27 *)

  let mu = 1.66053906660e-27      (* +/- 0.0000000005e-27 *)

  let rinf = 10973731.568160   (* +/- 0.000021 *)

  let alpha = 7.2973525693e-3     (* +/- 0.0000000011e-3 *)

  let r = 8.314462618             (* +/- 0 (exact) *)

  let sB = (Float.pi ** 2.0 *. kB ** 4.0) /. (60.0 *. hbar ** 3.0 *. c ** 2.0) (* +/- 0 (exact) *)

  let a0 = 5.29177210903e-11      (* +/- 0.00000000080e-11 *)

  let uB = 9.2740100783e-24       (* +/- 0.0000000028e-24 *)

  let uN = 5.050783699e-27        (* +/- 0.000000031e-27 *)

  let eh = 4.3597447222071e-18    (* +/- 0.0000000000085e-18 *)

  let amu = 1.66053906660e-27     (* +/- 0.00000000050e-27 *)
end

module Beam = struct
  let rayleigh
    ?(refr_index: float = 1.0)
    (wavelength: float)
    (waist_radius: float)
    : float
  =
    Float.pi *. waist_radius ** 2.0 *. refr_index /. wavelength

  let radius
    ?(refr_index: float = 1.0)
    (wavelength: float)
    (waist_radius: float)
    (z: float)
    : float
  =
    let zR = rayleigh ~refr_index:refr_index wavelength waist_radius in
    waist_radius *. (Float.sqrt (1.0 +. (z /. zR) ** 2.0))

  let peak_intensity (radius: float) (power: float): float =
    2.0 *. power /. (Float.pi *. radius ** 2.0)

  let power (radius: float) (peak_intensity: float): float =
    Float.pi *. radius ** 2.0 *. peak_intensity /. 2.0

  let radial_weight (radius: float) (r: float): float =
    Float.exp ((Float.neg 2.0) *. (r /. radius) ** 2.0)

  let axial_weight (rayl: float) (z: float): float =
    1.0 /. (1.0 +. (z /. rayl) ** 2.0)

  let dist
    ?(refr_index: float = 1.0)
    (wavelength: float)
    (waist_radius: float)
    (z: float)
    (r: float)
    : float
  =
    let zR = rayleigh ~refr_index:refr_index wavelength waist_radius in
    let rad = radius ~refr_index:refr_index wavelength waist_radius z in
    let w_rad = radial_weight rad r in
    let w_ax = axial_weight zR z in
    w_rad *. w_ax
end

module Rabi = struct
  let freq_to_saturation (linewidth: float) (rabi_freq: float): float =
    (rabi_freq /. linewidth) ** 2.0

  let saturation_to_freq (linewidth: float) (saturation: float): float =
    (Float.sqrt (saturation /. 2.0)) *. linewidth

  let saturation_intensity (wavelength: float) (linewidth: float): float =
    Float.pi /. 2.0 *. Const.h *. Const.c /. wavelength ** 3.0 *. linewidth
end

module SpinProj = struct
  type t = int

  let create (m: int): t = m

  let refl (m: t): t = -m

  let raise (m: t): t = m + 2

  let lower (m: t): t = m - 2

  let halves (m: t): int = m

  let to_float (m: t): float = (float_of_int m) /. 2.0

  let to_string (m: t): string =
    if m mod 2 = 0 then
      Printf.sprintf "proj:%i" (m / 2)
    else
      Printf.sprintf "proj:%i/2" m

  let compare (l: t) (r: t): int = compare l r
end

module SpinTotal = struct
  type t = int

  let create (s: int): t = Int.abs s

  let halves (s: t): int = s

  let to_float (s: t): float = (float_of_int s) /. 2.0

  let to_string (s: t): string =
    if s mod 2 = 0 then
      Printf.sprintf "tot:%i" (s / 2)
    else
      Printf.sprintf "tot:%i/2" s

  let compare (l: t) (r: t): int = compare l r
end

module Spin = struct
  type t = { tot: SpinTotal.t; proj: SpinProj.t }

  type err =
    | Invalid_proj of string
    | Invalid_raise of string
    | Invalid_lower of string

  type 'a res = ('a, err) result

  let create (tot: SpinTotal.t) (proj: SpinProj.t): t res =
    let s = SpinTotal.halves tot in
    let m = SpinProj.halves proj in
    if s mod 2 != Int.abs (m mod 2) then
      let msg =
        Printf.sprintf "spin numbers (%s, %s) must have equal parity"
          (SpinTotal.to_string tot)
          (SpinProj.to_string proj)
      in
      Error (Invalid_proj msg)
    else if m < -s || m > s then
      let msg =
        Printf.sprintf
          "spin projection %s must not exceed total %s in magnitude"
          (SpinTotal.to_string tot)
          (SpinProj.to_string proj)
      in
      Error (Invalid_proj msg)
    else
      Ok { tot = tot; proj = proj }

  let of_ints (tot: int) (proj: int): t res =
    let s = SpinTotal.create tot in
    let m = SpinProj.create proj in
    if tot mod 2 != Int.abs (proj mod 2) then
      let msg =
        Printf.sprintf "spin numbers (%s, %s) must have equal parity"
          (SpinTotal.to_string s)
          (SpinProj.to_string m)
      in
      Error (Invalid_proj msg)
    else if proj < -tot || proj > tot then
      let msg =
        Printf.sprintf
          "spin projection %s must not exceed total %s in magnitude"
          (SpinTotal.to_string s)
          (SpinProj.to_string m)
      in
      Error (Invalid_proj msg)
    else
      Ok { tot = SpinTotal.create tot; proj = SpinProj.create proj }

  let tot_of_int (tot: int): SpinTotal.t = SpinTotal.create tot

  let proj_of_int (proj: int): SpinProj.t = SpinProj.create proj

  let to_floats (s: t): float * float =
    (SpinTotal.to_float s.tot, SpinProj.to_float s.proj)

  let to_string (s: t): string =
    Printf.sprintf "(%s, %s)"
      (SpinTotal.to_string s.tot)
      (SpinProj.to_string s.proj)

  let tot (s: t): SpinTotal.t = s.tot

  let proj (s: t): SpinProj.t = s.proj

  let is_stretched_neg (s: t): bool =
    SpinProj.halves s.proj = -(SpinTotal.halves s.tot)

  let is_stretched_pos (s: t): bool =
    SpinProj.halves s.proj = SpinTotal.halves s.tot

  let is_stretched (s: t): bool =
    (Int.abs (SpinProj.halves s.proj)) = SpinTotal.halves s.tot

  let refl (s: t): t = { tot = s.tot; proj = SpinProj.refl s.proj }

  let raise (s: t): t res =
    if is_stretched_pos s then
      let msg =
        Printf.sprintf "cannot raise stretched state %s" (to_string s)
      in
      Error (Invalid_raise msg)
    else
      Ok { tot = s.tot; proj = SpinProj.raise s.proj }

  let lower (s: t): t res =
    if is_stretched_neg s then
      let msg =
        Printf.sprintf "cannot lower stretched state %s" (to_string s)
      in
      Error (Invalid_lower msg)
    else
      Ok { tot = s.tot; proj = SpinProj.lower s.proj }

  let halves (s: t): int * int =
    (SpinTotal.halves s.tot, SpinProj.halves s.proj)

  let projections (tot: SpinTotal.t): t Seq.t =
    let rec doit (m: SpinProj.t): t Seq.t =
      if m > tot then
        fun () -> Seq.Nil
      else
        fun () -> Seq.Cons ({ tot; proj = m }, doit (SpinProj.raise m))
    in
    doit (SpinProj.create (SpinTotal.halves tot) |> SpinProj.refl)

  let projections_rev (tot: SpinTotal.t): t Seq.t =
    let bound = -(SpinTotal.halves tot) in
    let rec doit (m: SpinProj.t): t Seq.t =
      if m < bound then
        fun () -> Seq.Nil
      else
        fun () -> Seq.Cons ({ tot; proj = m }, doit (SpinProj.lower m))
    in
    doit (SpinProj.create (SpinTotal.halves tot))

  let compare (l: t) (r: t): int option =
    if SpinTotal.compare l.tot r.tot = 0 then
      Some (SpinProj.compare l.proj r.proj)
    else
      None

  let factorial_float (n: int): float =
    let rec doit (n: int) (prod: int): int =
      if n <= 0 then
        prod
      else
        doit (n - 1) (n * prod)
    in
    doit n 1 |> float_of_int

  let cg (jm1: t) (jm2: t) (jm12: t): float =
    let j1 = SpinTotal.halves jm1.tot in
    let m1 = SpinProj.halves jm1.proj in
    let j2 = SpinTotal.halves jm2.tot in
    let m2 = SpinProj.halves jm2.proj in
    let j12 = SpinTotal.halves jm12.tot in
    let m12 = SpinProj.halves jm12.proj in
    if m1 + m2 != m12 || (j1 + j2) mod 2 != j12 mod 2 then
      0.0
    else
      let kmin =
        0
        |> Int.max (-(j12 - j2 + m1) / 2)
        |> Int.max (-(j12 - j1 - m2) / 2)
      in
      let kmax =
        ((j1 + j2 - j12) / 2)
        |> Int.min ((j1 - m1) / 2)
        |> Int.min ((j2 + m2) / 2)
      in
      if kmax < kmin then
        0.0
      else
        let summand (k: int): float =
          let sign = if k mod 2 = 0 then 1.0 else -1.0 in
          let f_k = factorial_float k in
          let f_j1_pj2_mj12_mk = factorial_float ((j1 + j2 - j12) / 2 - k) in
          let f_j1_mm1_mk = factorial_float ((j1 - m1) / 2 - k) in
          let f_j2_pm2_mk = factorial_float ((j2 + m2) / 2 - k) in
          let f_j12_mj2_pm1_pk = factorial_float ((j12 - j2 + m1) / 2 + k) in
          let f_j12_mj1_mm2_pk = factorial_float ((j12 - j1 - m2) / 2 + k) in
          sign
            /. f_k
            /. f_j1_pj2_mj12_mk
            /. f_j1_mm1_mk
            /. f_j2_pm2_mk
            /. f_j12_mj2_pm1_pk
            /. f_j12_mj1_mm2_pk
        in
        let sum =
          Seq.ints kmin
          |> Seq.take (kmax - kmin + 1)
          |> Seq.map summand
          |> Seq.fold_left (+.) 0.0
        in
        let j12t2_p1 = j12 + 1 |> float_of_int in
        let f_j12_pj1_mj2 = factorial_float ((j12 + j1 - j2) / 2) in
        let f_j12_mj1_pj2 = factorial_float ((j12 - j1 + j2) / 2) in
        let f_j1_pj2_mj12 = factorial_float ((j1 + j2 - j12) / 2) in
        let f_j1_pj2_pj12_p1 = factorial_float ((j1 + j2 + j12) / 2 + 1) in
        let f_j12_pm12 = factorial_float ((j12 + m12) / 2) in
        let f_j12_mm12 = factorial_float ((j12 - m12) / 2) in
        let f_j1_mm1 = factorial_float ((j1 - m1) / 2) in
        let f_j1_pm1 = factorial_float ((j1 + m1) / 2) in
        let f_j2_mm2 = factorial_float ((j2 - m2) / 2) in
        let f_j2_pm2 = factorial_float ((j2 + m2) / 2) in
        Float.sqrt (
          j12t2_p1
          *. f_j12_pj1_mj2
          *. f_j12_mj1_pj2
          *. f_j1_pj2_mj12
          *. f_j12_pm12
          *. f_j12_mm12
          *. f_j1_mm1
          *. f_j1_pm1
          *. f_j2_mm2
          *. f_j2_pm2
          /. f_j1_pj2_pj12_p1
        ) *. sum

  let w3j_sel (jm1: t) (jm2: t) (jm3: t): bool =
    let j1 = SpinTotal.halves jm1.tot in
    let m1 = SpinProj.halves jm1.proj in
    let j2 = SpinTotal.halves jm2.tot in
    let m2 = SpinProj.halves jm2.proj in
    let j3 = SpinTotal.halves jm3.tot in
    let m3 = SpinProj.halves jm3.proj in
    m1 + m2 + m3 = 0
    && (Int.abs (j1 - j2)) <= j3
    && j3 <= j1 + j2
    && (not (m1 = 0 && m2 = 0 && m3 = 0) && ((j1 + j2 + j3) / 2) mod 2 = 0)

  let w3j (jm1: t) (jm2: t) (jm3: t): float =
    if not (w3j_sel jm1 jm2 jm3) then
      0.0
    else
      let j1 = SpinTotal.halves jm1.tot in
      let j2 = SpinTotal.halves jm2.tot in
      let j3 = SpinTotal.halves jm3.tot in
      let m3 = SpinProj.halves jm3.proj in
      let sign = if ((j1 - j2 - m3) / 2) mod 2 = 0 then 1.0 else -1.0 in
      let denom = Float.sqrt ((j3 |> float_of_int) +. 1.0) in
      let cg = cg jm1 jm2 (refl jm3) in
      sign *. cg /. denom

  let w6j
    (j1: SpinTotal.t)
    (j2: SpinTotal.t)
    (j3: SpinTotal.t)
    (j4: SpinTotal.t)
    (j5: SpinTotal.t)
    (j6: SpinTotal.t)
    : float
  =
    let term_filter (jm1, (jm2, (jm3, (jm4, (jm5, jm6))))): bool =
      w3j_sel (refl jm1) (refl jm2) (refl jm3)
      && w3j_sel jm1 (refl jm5) jm6
      && w3j_sel jm4 jm2 (refl jm6)
      && w3j_sel (refl jm4) jm5 jm3
    in
    let sign_fn (jm1, (jm2, (jm3, (jm4, (jm5, jm6))))): float =
      let j1 = SpinTotal.halves jm1.tot in
      let m1 = SpinProj.halves jm1.proj in
      let j2 = SpinTotal.halves jm2.tot in
      let m2 = SpinProj.halves jm2.proj in
      let j3 = SpinTotal.halves jm3.tot in
      let m3 = SpinProj.halves jm3.proj in
      let j4 = SpinTotal.halves jm4.tot in
      let m4 = SpinProj.halves jm4.proj in
      let j5 = SpinTotal.halves jm5.tot in
      let m5 = SpinProj.halves jm5.proj in
      let j6 = SpinTotal.halves jm6.tot in
      let m6 = SpinProj.halves jm6.proj in
      let k = (j1 - m1 + j2 - m2 + j3 - m3 + j4 - m4 + j5 - m5 + j6 - m6) / 2 in
      if k mod 2 = 0 then 1.0 else -1.0
    in
    let term_map (jm1, (jm2, (jm3, (jm4, (jm5, jm6))))): float =
      (sign_fn (jm1, (jm2, (jm3, (jm4, (jm5, jm6))))))
      *. (w3j (refl jm1) (refl jm2) (refl jm3))
      *. (w3j jm1 (refl jm5) jm6)
      *. (w3j jm4 jm2 (refl jm6))
      *. (w3j (refl jm4) jm5 jm3)
    in
    projections j6
    |> Seq.product (projections j5)
    |> Seq.product (projections j4)
    |> Seq.product (projections j3)
    |> Seq.product (projections j2)
    |> Seq.product (projections j1)
    |> Seq.filter term_filter
    |> Seq.map term_map
    |> Seq.fold_left (+.) 0.0
end


