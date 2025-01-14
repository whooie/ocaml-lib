(** Provides implementations of Newton-Raphson root-finding and golden-section
    extrumum-finding. *)

(** Required functions for a Newton-Raphson root search, based on a mathematical
    field (minus unused operators) equipped with extra functions to clamp the
    search to a certain subset and to determine whether a step size is within
    a desired precision bound. *)
module type NR_domain = sig
  (** Represents the elements of the field. *)
  type t

  (** Subtraction between field elements. *)
  val sub : t -> t -> t

  (** Division between field elements. *)
  val div : t -> t -> t

  (** Ensures that a point in the field is returned to another that is within a
      certain sub-region of the field. *)
  val clamp : t -> t

  (** Returns [true] if a certain step size meets a desired precision bound. *)
  val lt_eps : t -> float -> bool
end

(** Signature of the Newton-Raphson implementation generated by the
    {!Newton_raphson} functor. *)
module type NR = sig
  (** Type of the elements of the field over which the search is to be
      performed. *)
  type t

  (** When the search fails to converge, this value is returned with the last
      search point. *)
  type err = No_converge of t

  (** Main Newton-Raphson routine.

      [find_root x0 f df eps maxsteps] is the result of a Newton-Raphson search
      beginning at [x0] for function [f] with derivative [df], targeting a
      precision bound [eps] and limited to [maxsteps] iterations. *)
  val find_root : t -> (t -> t) -> (t -> t) -> float -> int -> (t, err) result
end

(** Functor to generate an implementation of the Newton-Raphson algorithm for a
    particular field type. *)
module Newton_raphson (D: NR_domain): NR with type t = D.t

(** Required functions for a golden-section extremum search, based on a
    continuous domain equipped with functions to generate points within a
    sub-domain in a manner parameterized by a single [float] and to determine
    whether a sub-domain meets a desired precision bound. *)
module type GS_domain = sig
  (** Represents the elements of the domain. *)
  type t

  (** Generate a point within a given sub-domain according to the
      parameterization, which must map [0.0] and [1.0] to the ends of the
      sub-domain. *)
  val gen_point : (t * t) -> float -> t

  (** Returns [true] if a certain sub-domain meets a desired precision bound. *)
  val lt_eps : (t * t) -> float -> bool
end

(** Signature of the golden-section search implementation generated by the
    {!Golden_section} functor. *)
module type GS = sig
  (** Type of the elements of the domain over which the search is to be
      performed. *)
  type t

  (** When the search fails to converge, this value is returned with the last
      search bounds. *)
  type 'a err = No_converge of ((t * 'a) * (t * 'a))

  (** Represents a preference between two elements of the codomain of the
      searched space. *)
  type dir = Left | Right

  (** Main golden-section search routine.

      [find_extremum bracket f cmp eps maxsteps] is the result of a
      golden-section search in an initial sub-domain [bracket] with codomain
      given through application of [f] and comparison function [cmp] between
      codomain elements, targeting a precision bound [eps] and limited to
      [maxsteps] iterations. *)
  val find_extremum
    : (t * t)
    -> (t -> 'a)
    -> ('a -> 'a -> dir)
    -> float
    -> int
    -> (t * 'a, 'a err) result
end

(** Functor to generate an implementation of the golden-section search algorithm
    for a particular domain type. *)
module Golden_section (D: GS_domain): GS with type t = D.t

