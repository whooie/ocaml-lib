(** Collection of useful constants, functions, and constructs relevant to
    various areas of physics. *)

(** Various physical constants. *)
module Const: sig
  (** planck constant [kg m^2 s^-1] *)
  val h : float

  (** reduced planck constant [kg m^2 s^-1] *)
  val hbar : float

  (** speed of light in vacuum [m s^-1] *)
  val c : float

  (** Avogadro's number *)
  val nA : float

  (** Boltzmann's constant [J K^-1] *)
  val kB : float

  (** electric permittivity in vacuum [F m^-1] *)
  val e0 : float

  (** magnetic permeability in vacuum [N A^-2] *)
  val u0 : float

  (** Newtonian gravitational constant [m^3 kg^-1 s^-2] *)
  val gG : float

  (** gravitational acceleration near Earth's surface [m s^-2] *)
  val g : float

  (** elementary charge [C] *)
  val e : float

  (** electron mass [kg] *)
  val me : float

  (** proton mass [kg] *)
  val mp : float

  (** neutron mass [kg] *)
  val mn : float

  (** unified atomic mass unit [kg] *)
  val mu : float

  (** Rydberg constant [m^-1] *)
  val rinf : float

  (** fine structure constant *)
  val alpha : float

  (** molar gas constant *)
  val r : float

  (** Stefan-Boltzmann constant *)
  val sB : float

  (** Bohr radius [m] *)
  val a0 : float

  (** Bohr magneton [J T^-1] *)
  val uB : float

  (** nuclear magneton [J T^-1] *)
  val uN : float

  (** Hartree energy [J] = 2*Rinf*h*c *)
  val eh : float

  (** unified atomic mass unit [kg] = 1/NA/1000 *)
  val amu : float
end

(** Basic properties of a Gaussian beam. *)
module Beam: sig
  (** [rayleigh ~refr_index wavelength waist_radius] is the Rayleigh range. *)
  val rayleigh : ?refr_index:float -> float -> float -> float

  (** [radius ~refr_index wavelength waist_radius z] is the local radius of the
      beam at axial position [z], where [z = 0] corresponds to the waist of the
      beam. *)
  val radius : ?refr_index:float -> float -> float -> float -> float

  (** [peak_intensity radius power] is the maximum intensity of the beam over an
      axial cross section of 1/e^2 radius [radius]. *)
  val peak_intensity : float -> float -> float

  (** [power radius peak_intensity] is the total power through a plane at fixed
      axial position for a given peak intensity over the plane. *)
  val power : float -> float -> float

  (** [radial_weight radius r] is the zero-to-one weighting factor on the local
      intensity due to only the radial (Gaussian) power distribution in the
      beam. *)
  val radial_weight : float -> float -> float

  (** [axial_weight rayl z] is the zero-to-one weighting factor on the local
      intensity due to only the axial (Lorentzian) power distribution in the
      beam. *)
  val axial_weight : float -> float -> float

  (** [dist ~refr_index wavelength waist_radius z r] is the total zero-to-one
      power distribution in the beam, normalized to a maximum value of [1]. *)
  val dist : ?refr_index:float -> float -> float -> float -> float -> float
end

(** Quantities related to Rabi oscillations. *)
module Rabi: sig
  (** [freq_to_saturation linewidth rabi_freq] is the saturation parameter
      needed for a particular Rabi frequency. *)
  val freq_to_saturation : float -> float -> float

  (** [saturation_to_freq linewidth saturation] is the Rabi frequency produced
      by a particular saturation parameter. *)
  val saturation_to_freq : float -> float -> float

  (** [saturation_intensity wavelength linewidth] is the saturation intensity of
      a transition. *)
  val saturation_intensity : float -> float -> float
end

(** Manipulations of a spin-projection quantum number. *)
module SpinProj: sig
  (** Main type. *)
  type t

  (** Create a new [t] from a number of half-spin quanta. *)
  val create : int -> t

  (** [refl m] flips the sign of a projection number. *)
  val refl : t -> t

  (** [raise m] increases a projection number by [1]. *)
  val raise : t -> t

  (** [lower m] decreases a projection number by [1]. *)
  val lower : t -> t

  (** [halves m] is a projection number as a number of halves. *)
  val halves : t -> int

  (** [to_float m] is a projection number as an ordinary floating-point
      number. *)
  val to_float : t -> float

  (** [to_string m] is a string representation of a projection number. *)
  val to_string : t -> string

  (** [compare m1 m2] compares two projection numbers. *)
  val compare : t -> t -> int
end

(** Manipulations of a total-spin quantum number. *)
module SpinTotal: sig
  (** Main type. *)
  type t

  (** Create a new [t]. *)
  val create : int -> t

  (** [halves s] is a total spin number as a number of halves. *)
  val halves : t -> int

  (** [to_float s] is a total spin number as an ordinary floating-point number.
      *)
  val to_float : t -> float

  (** [to_string s] is a string representation of a total spin number. *)
  val to_string : t -> string

  (** [compare s1 s2] compares two total spin numbers. *)
  val compare : t -> t -> int
end

(** Manipulations of a total spin-spin projection pair. *)
module Spin: sig
  (** Main type. *)
  type t

  (** Error type. *)
  type err =
    | Invalid_proj of string
    | Invalid_raise of string
    | Invalid_lower of string

  (** Result wrapper around {!err}s. *)
  type 'a res = ('a, err) result

  (** Create a new [t]. *)
  val create : SpinTotal.t -> SpinProj.t -> t res

  (** Create a new [t] from a pair of [int]s. *)
  val of_ints : int -> int -> t res

  (** Create a new [SpinTotal.t] from an [int]. *)
  val tot_of_int : int -> SpinTotal.t

  (** Create a new [SpinProj.t] from an [int]. *)
  val proj_of_int : int -> SpinProj.t

  (** [to_floats s] is the pair of spin numbers as ordinary floating-point
      numbers. *)
  val to_floats : t -> float * float

  (** [to_string s] is a string representation of the pair of spin numbers. *)
  val to_string : t -> string

  (** [tot s] is the total-spin part of [s]. *)
  val tot : t -> SpinTotal.t

  (** [proj s] is the spin-projection part of [s]. *)
  val proj : t -> SpinProj.t

  (** [is_stretched_neg s] is [true] if [s] is maximally projected in the {i -z}
      direction; i.e. if [m = -j], where [m] is the projection part and [j] is
      the total part. *)
  val is_stretched_neg : t -> bool

  (** [is_stretched_pos s] is [true] if [s] is maximally projected in the {i +z}
      direction; i.e. if [m = j], where [m] is the projection part and [j] is
      the total part. *)
  val is_stretched_pos : t -> bool

  (** [is_stretched s] is [true] if [s] is maximally projection; i.e. if
      [|m| = j], where [m] is the projection part and [j] is the total part. *)
  val is_stretched : t -> bool

  (** [refl s] flips the sign of the projection number. *)
  val refl : t -> t

  (** [raise s] increases the projection part by [1] if [s] is not already
      stretched in the {i +z} direction. *)
  val raise : t -> t res

  (** [lower s] decreases the projection part by [1] if [s] is not already
      stretched in the {i -z} direction. *)
  val lower : t -> t res

  (** [halves s] is the pair of spin numbers as numbers of halves. *)
  val halves : t -> int * int

  (** [projections s] is a sequence over all available [t]s of total spin [s],
      from most-negative to most-positive projection number. *)
  val projections : SpinTotal.t -> t Seq.t

  (** [projections_rev s] is a sequence over all available [t]s of total spin
      [s], from most-positive to most-negative projection number. *)
  val projections_rev : SpinTotal.t -> t Seq.t

  (** [compare s1 s2] compares the projection numbers of [s1] and [s2] if they
      have equal total spin. *)
  val compare : t -> t -> int option

  (** [cg jm1 jm2 jm12] computes the Clebsch-Gordan coefficient
      [⟨jm1, jm2∣jm12⟩]. *)
  val cg : t -> t -> t -> float

  (** [w3j jm1 jm2 jm3] computes the Wigner 3{i j} symbol [(jm1 jm2 jm3)]. *)
  val w3j : t -> t -> t -> float

  (** [w6j j1 j2 j3 j4 j5 j6] computes the Wigner 6{i j} symbol
      [{j1 j2 j3; j4 j5 j6}] (where [j1], ..., [j3] are in the top row). *)
  val w6j
    : SpinTotal.t
    -> SpinTotal.t
    -> SpinTotal.t
    -> SpinTotal.t
    -> SpinTotal.t
    -> SpinTotal.t
    -> float
end

