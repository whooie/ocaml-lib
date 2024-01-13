(** Implements a real value associated with an experimental error that is
    automatically propagated through various operations. *)

(** Main data type. Each value of this type is a number ([v]) associated with a
    positive-valued error ([e]). *)
type t = private { v: float; e: float }

(** Create a new [t]. *)
val create : float -> float -> t

(** Passed to {!to_string} to control scientific notation. *)
type value_str_sci = No | Upper | Lower

type err =
  | Malformed_input of string
  | Unmatched_dollar
  | Parse_float_error of string

type 'a res = ('a, err) result

(** Render a [t] as a string.

    Optional formatting arguments:
    - [trunc]: Use "truncated" notation; e.g. [0.12(3)] as opposed to
      [0.12 +/- 0.03]. Defaults to [true].
    - [sign]: Include a [+] for positive values. Defaults to [false].
    - [sci]: Controls the use of scientific notation. Defaults to {!No}.
    - [latex]: Output string includes surrounding [$]'s and replaces [+/-] with
      [\pm]. Defaults to [false].
    - [dec]: Print numbers with this number of decimal places. Defaults to
      [None].
    *)
val to_string
  : ?trunc:bool
  -> ?sign:bool
  -> ?sci:value_str_sci
  -> ?latex:bool
  -> ?dec:int option
  -> t
  -> string

(** Convert a [float] to a [t] with zero error. *)
val of_float : float -> t

(** Convert a [float * float] pair to a [t], with the second item corresponding
    to the error. *)
val of_pair : float * float -> t

(** Return [true] if the values of two [t]s are equal, otherwise [false]. *)
val eq : t -> t -> bool

(** Return [true] if the values of two [t]s are not equal, otherwise [false]. *)
val neq : t -> t -> bool

(** {!compare} the values of two [t]s. *)
val compare : t -> t -> int

(** Return [true] if the value of the left argument is greater than the value of
    the right argument, otherwise [false]. *)
val ge : t -> t -> bool

(** Return [true] if the value of the left argument is less than the value of
    the right argument, otherwise [false]. *)
val le : t -> t -> bool

(** Return [true] if the value of the left argument is greater than or equal to
    the value of the right argument, otherwise [false]. *)
val geq : t -> t -> bool

(** Return [true] if the value of the left argument is less than or equal to the
    value of the right argument, otherwise [false]. *)
val leq : t -> t -> bool

(** Flip the sign of a [t]'s value. *)
val neg : t -> t

(** Add two [t]s. *)
val add : t -> t -> t

(** {!add} *)
val ( <+> ) : t -> t -> t

(** Add a [t] to a [float]. *)
val addf : t -> float -> t

(** {!addf} *)
val ( <+>$ ) : t -> float -> t

(** Add a [float] to a [t]. *)
val fadd : float -> t -> t

(** {!fadd} *)
val ( $<+> ) : float -> t -> t

(** Subtract a [t] from another. *)
val sub : t -> t -> t

(** {!sub} *)
val ( <-> ) : t -> t -> t

(** Subtract a [float] from a [t]. *)
val subf : t -> float -> t

(** {!subf} *)
val ( <->$ ) : t -> float -> t

(** Subtract a [t] from a [float]. *)
val fsub : float -> t -> t

(** {!fsub} *)
val ( $<-> ) : float -> t -> t

(** Multiply two [t]s. *)
val mul : t -> t -> t

(** {!mul} *)
val ( <*> ) : t -> t -> t

(** Multiply a [t] and a [float]. *)
val mulf : t -> float -> t

(** {!mulf} *)
val ( <*>$ ) : t -> float -> t

(** Multiply a [float] and a [t]. *)
val fmul : float -> t -> t

(** {!fmul} *)
val ( $<*> ) : float -> t -> t

(** Divide a [t] by another. *)
val div : t -> t -> t

(** {!div} *)
val ( </> ) : t -> t -> t

(** Divide a [t] by a [float]. *)
val divf : t -> float -> t

(** {!divf} *)
val ( </>$ ) : t -> float -> t

(** Divide a [float] by a [t]. *)
val fdiv : float -> t -> t

(** {!fdiv} *)
val ( $</> ) : float -> t -> t

(** Absolute value. *)
val abs : t -> t

(** Absolute difference. *)
val abs_sub : t -> t -> t

(** Arc cosine. *)
val acos : t -> t

(** Hyperbolic arc cosine. *)
val acosh : t -> t

(** Arc sine. *)
val asin : t -> t

(** Hyperbolic arc sine. *)
val asinh : t -> t

(** Arc tangent. *)
val atan : t -> t

(** Arc tangent, tracking the quadrants of each to determine the final output.
    *)
val atan2 : t -> t -> t

(** Hyperbolic arc tangent. *)
val atanh : t -> t

(** Cube root. *)
val cbrt : t -> t

(** Round up to the nearest integer value. *)
val ceil : t -> t

(** Return the class of a value. *)
val classify : t -> Float.fpclass

(** Cosine. *)
val cos : t -> t

(** Hyperbolic cosine. *)
val cosh : t -> t

(** Exponential function. *)
val exp : t -> t

(** Base-2 exponential function. *)
val exp2 : t -> t

(** Exponential of the argument minus [1]. *)
val expm1 : t -> t

(** Round down to the nearest integer value. *)
val floor : t -> t

(** Reduce to the non-integer part. *)
val fract : t -> t

(** Calculate [sqrt(x^2 + y^2)]. *)
val hypot : t -> t -> t

(** Positive infinity. *)
val infinity : t

(** Returns [true] if the value is finite, otherwise [false]. *)
val is_finite : t -> bool

(** Returns [true] if the value is infinite, otherwise [false]. *)
val is_infinite : t -> bool

(** Returns [true] if the value is [nan], otherwise [false]. *)
val is_nan : t -> bool

(** Returns [true] if the value has negative sign, otherwise [false]. *)
val is_sign_negative : t -> bool

(** Returns [true] if the value has positive sign, otherwise [false]. *)
val is_sign_positive : t -> bool

(** Natural logarithm. *)
val ln : t -> t

(** Natural logarithm of [1] plus the argument. *)
val ln1p : t -> t

(** Logarithm with the first argument being an arbitrary base. *)
val log : t -> t -> t

(** Base-10 logarithm. *)
val log10 : t -> t

(** Base-2 logarithm. *)
val log2 : t -> t

(** Return the greater of two values, defaulting to the first if they are equal.
    *)
val max : t -> t -> t

(** Maximum possible value. *)
val max_value : t

(** Return the lesser of two values, defaulting to the first if they are equal.
    *)
val min : t -> t -> t

(** Minimum possible positive value. This is equivalent to machine epsilon. *)
val min_positive_value : t

(** Minimum possible value. *)
val min_value : t

(** Special value standing in for the result undefined operations. *)
val nan : t

(** Negative infinity. *)
val neg_infinity : t

(** Raise the first argument to the power of the second. *)
val pow : t -> t -> t

(** Round to the nearest integer value. *)
val round : t -> t

(** Sine. *)
val sin : t -> t

(** Return the sine and cosine of the value as a pair, with the first being the
    sine. *)
val sin_cos : t -> t * t

(** Hyperbolic sine. *)
val sinh : t -> t

(** Square root. *)
val sqrt : t -> t

(** Tangent. *)
val tan : t -> t

(** Hyperbolic tangent. *)
val tanh : t -> t

(** Discard the fractional part of the value. *)
val trunc : t -> t

(** Multiplicative identity. *)
val one : t

(** Additive identity. *)
val zero : t

(** Returns [true] if equal to {!zero}, otherwise [false]. *)
val is_zero : t -> bool

