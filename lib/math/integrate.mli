(** Provides implementations of various numerical techniques to compute
    integrals. *)

(** Required functions for computation of a one-dimensional numerical integral.

    [t] is the container type holding collections of [elt], the type of the
    integrand, as well as [x], the type of the x-coordinates. *)
module type Integrable = sig
  (** Data container type. *)
  type 'a t

  (** Integrand data type. *)
  type elt

  (** {i x}-coordinate data type. *)
  type x

  (** Additive identity element of the integrand type. *)
  val zero : elt

  (** Add two [elt]s together. *)
  val add : elt -> elt -> elt

  (** Subtract an [x] from another. *)
  val subx : x -> x -> x

  (** Multiply an [elt] by an [x]. *)
  val mulx : x -> elt -> elt

  (** Multiply an [elt] by a [float]. *)
  val mulf : float -> elt -> elt

  (** Find the length of a [t]. *)
  val length : 'a t -> int

  (** Get the element of a [t] at a particular index. Indices should start at
      zero. *)
  val get : 'a t -> int -> 'a option

  (** Convert a [t] to a sequence. *)
  val to_seq : 'a t -> 'a Seq.t

  (** Convert a [t] to an indexed sequence. *)
  val to_seqi : 'a t -> (int * 'a) Seq.t

  (** Collect a sequence into a [t]. *)
  val of_seq : 'a Seq.t -> 'a t
end

(** Signature of the trapezoidal rule implementation generated by the {!Trapz}
    functor. *)
module type Trapz_sig = sig
  (** Data container type. *)
  type 'a t

  (** Integrand data type. *)
  type elt

  (** {i x}-coordinate data type. *)
  type x

  (** Output error types. *)
  type err =
    | Too_short of string
    (** Returned if any data collections are less than 2 points. *)
    | Unequal_lengths of string
    (** Returned by {!trapz_nonuniform} if the two coordinate collections are of
        unequal lengths. *)

  (** [result] wrapper around {!err}. *)
  type 'a res = ('a, err) result

  (** Compute the integral of a data collection by the trapezoidal rule for
      fixed sampling interval. *)
  val trapz : x -> elt t -> elt res

  (** Compute the integral of a data collection by the trapezoidal rule for
      nonuniform sampling intervals. *)
  val trapz_nonuniform : x t -> elt t -> elt res

  (** Compute the progressive integral of a data collection by the trapezoidal
      rule for fixed sampling interval. *)
  val trapz_prog : x -> elt t -> elt t res

  (** Compute the progressive integral of a data collection by the trapezoidal
      rule for nonuniform sampling intervals. *)
  val trapz_prog_nonuniform : x t -> elt t -> elt t res
end

(** Functor to generate an implementation of the trapezoidal rule numerical
    integration scheme for a particular set of data types. *)
module Trapz (I: Integrable): Trapz_sig
  with type 'a t = 'a I.t and type elt = I.elt and type x = I.x

(** Signature of the Simpson's rule implementation generated by the {!Simpson}
    functor. *)
module type Simpson_sig = sig
  (** Data container type. *)
  type 'a t

  (** Integrand data type. *)
  type elt

  (** {i x}-coordinate data type. *)
  type x

  (** Output error type. *)
  type err =
    | Too_short of string
    (** Returned if any data collections are less than 3 points. *)

  (** [result] wrapper around {!err}. *)
  type 'a res = ('a, err) result

  (** Compute the integral of a data collection by Simpson's rule for fixed
      sampling interval. *)
  val simpson : x -> elt t -> elt res

  (** Compute the progressive integral of a data collection by Simpson's rule
      for fixed sampling interval. *)
  val simpson_prog : x -> elt t -> elt t res
end

(** Functor to generate an implementation of Simpson's rule numerical
    integration for a particular set of data types. *)
module Simpson (I: Integrable): Simpson_sig
  with type 'a t = 'a I.t and type elt = I.elt and type x = I.x

(** Signature of the Boole's rule implementation generated by the {!Boole}
    functor. *)
module type Boole_sig = sig
  (** Data container type. *)
  type 'a t

  (** Integrand data type. *)
  type elt

  (** {i x}-coordinate data type. *)
  type x

  (** Output error type. *)
  type err =
    | Too_short of string
    (** Returned if any data collections are less than 5 points. *)

  (** [result] wrapper around {!err}. *)
  type 'a res = ('a, err) result

  (** Compute the integral of a data collection by Boole's rule for fixed
      sampling interval. *)
  val boole : x -> elt t -> elt res

  (** Compute the progressive integral of a data collection by Boole's rule for
      fixed sampling interval. *)
  val boole_prog : x -> elt t -> elt t res
end

(** Functor to generate an implementation of Boole's rule numerical integration
    for a particular set of data types. *)
module Boole (I: Integrable): Boole_sig
  with type 'a t = 'a I.t and type elt = I.elt and type x = I.x

(** Signature of the implementations generated by the {!Make} functor. *)
module type I = sig
  (** Data container type. *)
  type 'a t

  (** Integrand data type. *)
  type elt

  (** {i x}-coordinate data type. *)
  type x

  (** Output error types. *)
  type err =
    | Too_short of string
    (** Returned if any data collections are less than 2 points. *)
    | Unequal_lengths of string
    (** Returned by {!trapz_nonuniform} if the two coordinate collections are of
        unequal lengths. *)

  (** [result] wrapper around {!err}. *)
  type 'a res = ('a, err) result

  (** Compute the integral of a data collection by the trapezoidal rule for
      fixed sampling interval. *)
  val trapz : x -> elt t -> elt res

  (** Compute the integral of a data collection by the trapezoidal rule for
      nonuniform sampling intervals. *)
  val trapz_nonuniform : x t -> elt t -> elt res

  (** Compute the progressive integral of a data collection by the trapezoidal
      rule for fixed sampling interval. *)
  val trapz_prog : x -> elt t -> elt t res

  (** Compute the progressive integral of a data collection by the trapezoidal
      rule for nonuniform sampling intervals. *)
  val trapz_prog_nonuniform : x t -> elt t -> elt t res

  (** Compute the integral of a data collection by Simpson's rule for fixed
      sampling interval. *)
  val simpson : x -> elt t -> elt res

  (** Compute the progressive integral of a data collection by Simpson's rule
      for fixed sampling interval. *)
  val simpson_prog : x -> elt t -> elt t res

  (** Compute the integral of a data collection by Boole's rule for fixed
      sampling interval. *)
  val boole : x -> elt t -> elt res

  (** Compute the progressive integral of a data collection by Boole's rule for
      fixed sampling interval. *)
  val boole_prog : x -> elt t -> elt t res

  (** Compute the integral of a data collection, minimizing truncation error
      by choosing from the above schemes based on the number of points in the
      collection. *)
  val integrate : x -> elt t -> elt res

  (** Compute the progressive integral of a data collection, minimizing
      truncation error by choosing from the above schemes based on the number of
      points in the collection. *)
  val integrate_prog : x -> elt t -> elt t res
end

(** Functor to generate implementations of all of the above numerical
    integration schemes for a particular set of data types. *)
module Make (I: Integrable): I
  with type 'a t = 'a I.t and type elt = I.elt and type x = I.x
