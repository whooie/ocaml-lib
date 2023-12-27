(** Rust-like [result] functions. *)

(** Raised when an {{!Error} [Error]} variant is unwrapped. *)
exception Unwrap

(** Raised by the [expect] and [expect_err] functions. *)
exception Expect of string

(** [get_ok res] is [Some r] if [res] is [Ok r], otherwise [None]. *)
val get_ok : ('a, 'e) result -> 'a option

(** [get_err res] is [Some e] if [res] is [Err e], otherwise [None]. *)
val get_err : ('a, 'e) result -> 'e option

(** [ok_and res_a res_b] is [res_b] if [res_a] is [Ok _], otherwise [res_a].
    *)
val ok_and : ('a, 'e) result -> ('b, 'e) result -> ('b, 'e) result

(** [ok_and_then f res] is [f r] if [res] is [Ok r], otherwise [res]. *)
val ok_and_then : ('a -> ('b, 'e) result) -> ('a, 'e) result -> ('b, 'e) result

(** [ok_then f res] is [f r] if [res] is [Ok r], otherwise [()]. *)
val ok_then : ('a -> unit) -> ('a, 'e) result -> unit

(** [err_or res_a res_b] is [res_a] if [res_a] is [Ok _], otherwise [res_b].
    *)
val err_or : ('a, 'e) result -> ('a, 'f) result -> ('a, 'f) result

(** [err_or_else f res] is [f e] if [res] is [Err e], otherwise [res]. *)
val err_or_else : ('e -> ('a, 'f) result) -> ('a, 'e) result -> ('a, 'f) result

(** [err_then f res] is [f e] if [res] is [Err e], otherwise [()]. *)
val err_then : ('e -> unit) -> ('a, 'e) result -> unit

(** [map f res] is [Ok (f r)] if [res] is [Ok r], otherwise [res]. *)
val map : ('a -> 'b) -> ('a, 'e) result -> ('b, 'e) result

(** [map_or f default res] is [f r] if [res] is [Ok r], otherwise [default].
    *)
val map_or : ('a -> 'b) -> 'b -> ('a, 'e) result -> 'b

(** [map_or_else f_ok f_err res] is [f_ok r] if [res] is [Ok r], otherwise
    [f_err e] when [res] is [Err e]. *)
val map_or_else : ('a -> 'b) -> ('e -> 'b) -> ('a, 'e) result -> 'b

(** [map_err f res] is [Err (f e)] if [res] is [Err e], otherwise [res]. *)
val map_err : ('e -> 'f) -> ('a, 'e) result -> ('a, 'f) result

(** [unwrap_or default res] is [r] if [res] is [Ok r], otherwise [default]. *)
val unwrap_or : 'a -> ('a, 'e) result -> 'a

(** [unwrap_or_else f res] is [r] if [res] is [Ok r], otherwise [f e] when
    [res] is [Err e]. *)
val unwrap_or_else : ('e -> 'a) -> ('a, 'e) result -> 'a

(** [unwrap res] is [r] if [res] is [Ok r], otherwise [Unwrap_err] is raised.
    *)
val unwrap : ('a, 'e) result -> 'a

(** [expect msg res] is [r] if [res] is [Ok r], otherwise [Expect msg] is
    raised. *)
val expect : string -> ('a, 'e) result -> 'a

(** [expect_err msg res] if [e] if [res] is [Err e], otherwise [Expect msg] is
    raised. *)
val expect_err : string -> ('a, 'e) result -> 'e

(** [collect_list items] is [Ok its] if all elements of [items] are [Ok _],
    otherwise the leftmost [Err _] encountered. *)
val collect_list : ('a, 'e) result list -> ('a list, 'e) result

(** [collect_list_of_seq items] is like [collect_list items], but for a
    sequence. *)
val collect_list_of_seq : ('a, 'e) result Seq.t -> ('a list, 'e) result

(** Result error type contained by the [Err] variant of a result. *)
module type Error = sig
  type t

  (** Convert to a string. *)
  val to_string : t -> string
end

(** Output signature of the functor {!Make}. *)
module type R = sig
  (** Error type. *)
  type err

  (** Main result type. *)
  type 'a t =
    | Ok of 'a
    | Err of err

  (** Raised when an {{!Err} [Err]} variant is unwrapped. *)
  exception Unwrap_err of string

  (** Raised by the [expect] and [expect_err] functions. *)
  exception Expect of string

  (** [get_ok res] is [Some r] if [res] is [Ok r], otherwise [None]. *)
  val get_ok : 'a t -> 'a option

  (** [get_err res] is [Some e] if [res] is [Err e], otherwise [None]. *)
  val get_err : 'a t -> err option

  (** [ok_and res_a res_b] is [res_b] if [res_a] is [Ok _], otherwise [res_a].
      *)
  val ok_and : 'a t -> 'b t -> 'b t

  (** [ok_and_then f res] is [f r] if [res] is [Ok r], otherwise [res]. *)
  val ok_and_then : ('a -> 'b t) -> 'a t -> 'b t

  (** [ok_then f res] is [f r] if [res] is [Ok r], otherwise [()]. *)
  val ok_then : ('a -> unit) -> 'a t -> unit

  (** [err_or res_a res_b] is [res_a] if [res_a] is [Ok _], otherwise [res_b].
      *)
  val err_or : 'a t -> 'a t -> 'a t

  (** [err_or_else f res] is [f e] if [res] is [Err e], otherwise [res]. *)
  val err_or_else : (err -> 'a t) -> 'a t -> 'a t

  (** [err_then f res] is [f e] if [res] is [Err e], otherwise [()]. *)
  val err_then : (err -> unit) -> 'a t -> unit

  (** [map f res] is [Ok (f r)] if [res] is [Ok r], otherwise [res]. *)
  val map : ('a -> 'b) -> 'a t -> 'b t

  (** [map_or f default res] is [f r] if [res] is [Ok r], otherwise [default].
      *)
  val map_or : ('a -> 'b) -> 'b -> 'a t -> 'b

  (** [map_or_else f_ok f_err res] is [f_ok r] if [res] is [Ok r], otherwise
      [f_err e] when [res] is [Err e]. *)
  val map_or_else : ('a -> 'b) -> (err -> 'b) -> 'a t -> 'b

  (** [map_err f res] is [Err (f e)] if [res] is [Err e], otherwise [res]. *)
  val map_err : (err -> err) -> 'a t -> 'a t

  (** [unwrap_or default res] is [r] if [res] is [Ok r], otherwise [default]. *)
  val unwrap_or : 'a -> 'a t -> 'a

  (** [unwrap_or_else f res] is [r] if [res] is [Ok r], otherwise [f e] when
      [res] is [Err e]. *)
  val unwrap_or_else : (err -> 'a) -> 'a t -> 'a

  (** [unwrap res] is [r] if [res] is [Ok r], otherwise [Unwrap_err] is raised.
      *)
  val unwrap : 'a t -> 'a

  (** [expect msg res] is [r] if [res] is [Ok r], otherwise [Expect msg] is
      raised. *)
  val expect : string -> 'a t -> 'a

  (** [expect_err msg res] if [e] if [res] is [Err e], otherwise [Expect msg] is
      raised. *)
  val expect_err : string -> 'a t -> err

  (** [collect_list items] is [Ok its] if all elements of [items] are [Ok _],
      otherwise the leftmost [Err _] encountered. *)
  val collect_list : 'a t list -> 'a list t

  (** [collect_list_of_seq items] is like [collect_list items], but for a
      sequence. *)
  val collect_list_of_seq : 'a t Seq.t -> 'a list t
end

(** Functor to create a result implementation. Each result is of fixed error
    type. *)
module Make (E: Error): R with type err = E.t

