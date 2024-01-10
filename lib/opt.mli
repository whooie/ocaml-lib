(** Rust-like [option] functions. *)

(** Raised when a [None] variant is unwrapped. *)
exception Unwrap of string

(** Raised by the [expect] function. *)
exception Expect of string

(** [is_some opt] is [true] if [opt] is [Some _], otherwise [false]. *)
val is_some : 'a option -> bool

(** [is_some_and f opt] is [f a] if [opt] is [Some a], otherwise [false]. *)
val is_some_and : ('a -> bool) -> 'a option -> bool

(** [is_none opt] is [true] if [opt] is [None], otherwise [false]. *)
val is_none : 'a option -> bool

(** [some_and opt_a opt_b] is [opt_b] if [opt_a] is [Some _], otherwise [None].
    *)
val some_and : 'a option -> 'b option -> 'b option

(** [some_and_then f opt] is [f a] if [opt_a] is [Some a], otherwise [None]. *)
val some_and_then : ('a -> 'b option) -> 'a option -> 'b option

(** [some_then f opt] is [f a] if [opt_a] is [Some a], otherwise [()]. *)
val some_then : ('a -> unit) -> 'a option -> unit

(** [some_or opt_a opt_b] is [opt_a] if [opt_a] is [Some _], otherwise [opt_b].
    *)
val some_or : 'a option -> 'a option -> 'a option

(** [some_or_else f opt] is [opt_a] if [opt_a] is [Some _], otherwise [f ()]. *)
val some_or_else : (unit -> 'a option) -> 'a option -> 'a option

(** [some_xor opt_a opt_b] is whichever of the two is [Some _] if exactly one of
    them is [Some _], otherwise [None]. *)
val some_xor : 'a option -> 'a option -> 'a option

(** [zip opt_a opt_b] is [Some (a, b)] if [opt_a] is [Some a] and [opt_b] is
    [Some b], otherwise [None]. *)
val zip : 'a option -> 'b option -> ('a * 'b) option

(** [unzip opt] is [(Some a, Some b)] if [opt] is [Some (a, b)], otherwise
    [(None, None)]. *)
val unzip : ('a * 'b) option -> ('a option) * ('b option)

(** [flatten opt] is [Some a] if [opt] is [Some (Some a)], otherwise [None]. *)
val flatten : 'a option option -> 'a option

(** [map f opt] is [Some (f a)] if [opt] is [Some a], otherwise [None]. *)
val map : ('a -> 'b) -> 'a option -> 'b option

(** [map_or f default opt] is [f a] if [opt] is [Some a], otherwise [default].
    *)
val map_or : ('a -> 'b) -> 'b -> 'a option -> 'b

(** [map_or_else f_some f_none opt] is [f_some a] if [opt] is [Some a],
    otherwise [f_none ()]. *)
val map_or_else : ('a -> 'b) -> (unit -> 'b) -> 'a option -> 'b

(** [ok_or err opt] is [Ok a] if [opt] is [Some a], otherwise [Error err]. *)
val ok_or : 'e -> 'a option -> ('a, 'e) result

(** [ok_or_else f opt] is [Ok a] if [opt] is [Some a], otherwise [Error (f ())].
    *)
val ok_or_else : (unit -> 'e) -> 'a option -> ('a, 'e) result

(** [unwrap_or default opt] is [a] if [opt] is [Some a], otherwise [default]. *)
val unwrap_or : 'a -> 'a option -> 'a

(** [unwrap_or_else f opt] is [a] if [opt] is [Some a], otherwise [f ()]. *)
val unwrap_or_else : (unit -> 'a) -> 'a option -> 'a

(** [unwrap ~msg opt] is [a] if [opt] is [Some a], otherwise {{!Unwrap} [Unwrap
    msg]} is raised. [msg] defaults to [""]. *)
val unwrap : ?msg:string -> 'a option -> 'a

(** [expect msg opt] is [a] if [opt] is [Some a], otherwise {{!Expect}
    [Expect msg]} is raised. *)
val expect : string -> 'a option -> 'a

