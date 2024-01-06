exception Unwrap of string

exception Expect of string

let get_ok (res: ('a, 'e) result): 'a option =
  match res with
  | Ok r -> Some r
  | Error _ -> None

let get_err (res: ('a, 'e) result): 'e option =
  match res with
  | Ok _ -> None
  | Error e -> Some e

let ok_and (res_a: ('a, 'e) result) (res_b: ('b, 'e) result): ('b, 'e) result =
  match res_a with
  | Ok _ -> res_b
  | Error e -> Error e

let ok_and_then
  (f: 'a -> ('b, 'e) result)
  (res: ('a, 'e) result)
  : ('b, 'e) result
=
  match res with
  | Ok r -> f r
  | Error e -> Error e

let ok_then (f: 'a -> unit) (res: ('a, 'e) result): unit =
  match res with
  | Ok r -> f r
  | Error _ -> ()

let err_or (res_a: ('a, 'e) result) (res_b: ('a, 'f) result): ('a, 'f) result =
  match res_a with
  | Ok r -> Ok r
  | Error _ -> res_b

let err_or_else
  (f: 'e -> ('a, 'f) result)
  (res: ('a, 'e) result)
  : ('a, 'f) result
=
  match res with
  | Ok r -> Ok r
  | Error e -> f e

let err_then (f: 'e -> unit) (res: ('a, 'e) result): unit =
  match res with
  | Ok _ -> ()
  | Error e -> f e

let map (f: 'a -> 'b) (res: ('a, 'e) result): ('b, 'e) result =
  match res with
  | Ok a -> Ok (f a)
  | Error e -> Error e

let map_or (f: 'a -> 'b) (default: 'b) (res: ('a, 'e) result): 'b =
  match res with
  | Ok a -> f a
  | Error _ -> default

let map_or_else (f_ok: 'a -> 'b) (f_err: 'e -> 'b) (res: ('a, 'e) result): 'b =
  match res with
  | Ok a -> f_ok a
  | Error e -> f_err e

let map_err (f: 'e -> 'f) (res: ('a, 'e) result): ('a, 'f) result =
  match res with
  | Ok r -> Ok r
  | Error e -> Error (f e)

let unwrap_or (default: 'a) (res: ('a, 'e) result): 'a =
  match res with
  | Ok r -> r
  | Error _ -> default

let unwrap_or_else (f: 'e -> 'a) (res: ('a, 'e) result): 'a =
  match res with
  | Ok r -> r
  | Error e -> f e

let unwrap ?(msg: 'e -> string = (fun _ -> "")) (res: ('a, 'e) result): 'a =
  match res with
  | Ok r -> r
  | Error e -> raise (Unwrap (msg e))

let expect (msg: string) (res: ('a, 'e) result): 'a =
  match res with
  | Ok r -> r
  | Error _ -> raise (Expect msg)

let expect_err (msg: string) (res: ('a, 'e) result): 'e =
  match res with
  | Ok _ -> raise (Expect msg)
  | Error e -> e

let rec collect_list (items: ('a, 'e) result list): ('a list, 'e) result =
  match items with
  | [] -> Ok []
  | (Error e) :: _ -> Error e
  | (Ok item) :: tail ->
      match collect_list tail with
      | Error e -> Error e
      | Ok rec_res -> Ok (item :: rec_res)

let rec collect_list_of_seq
  (items: ('a, 'e) result Seq.t)
  : ('a list, 'e) result
=
  match items () with
  | Seq.Nil -> Ok []
  | Seq.Cons (Error e, _) -> Error e
  | Seq.Cons (Ok item, rest) ->
      match collect_list_of_seq rest with
      | Error e -> Error e
      | Ok rec_res -> Ok (item :: rec_res)

module type Error = sig
  type t
  val to_string : t -> string
end

module type R = sig
  type err
  type 'a t =
    | Ok of 'a
    | Err of err

  exception Unwrap_err of string
  exception Expect of string

  val get_ok : 'a t -> 'a option
  val get_err : 'a t -> err option
  val ok_and : 'a t -> 'b t -> 'b t
  val ok_and_then : ('a -> 'b t) -> 'a t -> 'b t
  val ok_then : ('a -> unit) -> 'a t -> unit
  val err_or : 'a t -> 'a t -> 'a t
  val err_or_else : (err -> 'a t) -> 'a t -> 'a t
  val err_then : (err -> unit) -> 'a t -> unit
  val map : ('a -> 'b) -> 'a t -> 'b t
  val map_or : ('a -> 'b) -> 'b -> 'a t -> 'b
  val map_or_else : ('a -> 'b) -> (err -> 'b) -> 'a t -> 'b
  val map_err : (err -> err) -> 'a t -> 'a t
  val unwrap_or : 'a -> 'a t -> 'a
  val unwrap_or_else : (err -> 'a) -> 'a t -> 'a
  val unwrap : 'a t -> 'a
  val expect : string -> 'a t -> 'a
  val expect_err : string -> 'a t -> err
  val collect_list : 'a t list -> 'a list t
  val collect_list_of_seq : 'a t Seq.t -> 'a list t
end

module Make (E: Error) = struct
  type err = E.t

  type 'a t =
    | Ok of 'a
    | Err of err

  exception Unwrap_err of string

  exception Expect of string

  let get_ok (res: 'a t): 'a option =
    match res with
    | Ok r -> Some r
    | Err _ -> None

  let get_err (res: 'a t): err option =
    match res with
    | Ok _ -> None
    | Err e -> Some e

  let ok_and (res_a: 'a t) (res_b: 'b t): 'b t =
    match res_a with
    | Ok _ -> res_b
    | Err e -> Err e

  let ok_and_then (f: 'a -> 'b t) (res: 'a t): 'b t =
    match res with
    | Ok r -> f r
    | Err e -> Err e

  let ok_then (f: 'a -> unit) (res: 'a t): unit =
    match res with
    | Ok r -> f r
    | Err _ -> ()

  let err_or (res_a: 'a t) (res_b: 'a t): 'a t =
    match res_a with
    | Ok r -> Ok r
    | Err _ -> res_b

  let err_or_else (f: err -> 'a t) (res: 'a t): 'a t =
    match res with
    | Ok r -> Ok r
    | Err e -> f e

  let err_then (f: err -> unit) (res: 'a t): unit =
    match res with
    | Ok _ -> ()
    | Err e -> f e

  let map (f: 'a -> 'b) (res: 'a t): 'b t =
    match res with
    | Ok a -> Ok (f a)
    | Err e -> Err e

  let map_or (f: 'a -> 'b) (default: 'b) (res: 'a t): 'b =
    match res with
    | Ok a -> f a
    | Err _ -> default

  let map_or_else (f_ok: 'a -> 'b) (f_err: err -> 'b) (res: 'a t): 'b =
    match res with
    | Ok a -> f_ok a
    | Err e -> f_err e

  let map_err (f: err -> err) (res: 'a t): 'a t =
    match res with
    | Ok r -> Ok r
    | Err e -> Err (f e)

  let unwrap_or (default: 'a) (res: 'a t): 'a =
    match res with
    | Ok r -> r
    | Err _ -> default

  let unwrap_or_else (f: err -> 'a) (res: 'a t): 'a =
    match res with
    | Ok r -> r
    | Err e -> f e

  let unwrap (res: 'a t): 'a =
    match res with
    | Ok r -> r
    | Err e -> raise (Unwrap_err (E.to_string e))

  let expect (msg: string) (res: 'a t): 'a =
    match res with
    | Ok r -> r
    | Err _ -> raise (Expect msg)

  let expect_err (msg: string) (res: 'a t): err =
    match res with
    | Ok _ -> raise (Expect msg)
    | Err e -> e

  let rec collect_list (items: 'a t list): 'a list t =
    match items with
    | [] -> Ok []
    | (Err e) :: _ -> Err e
    | (Ok item) :: tail ->
        match collect_list tail with
        | Err e -> Err e
        | Ok rec_res -> Ok (item :: rec_res)

  let rec collect_list_of_seq (items: 'a t Seq.t): 'a list t =
    match items () with
    | Seq.Nil -> Ok []
    | Seq.Cons (Err e, _) -> Err e
    | Seq.Cons (Ok item, rest) ->
        match collect_list_of_seq rest with
        | Err e -> Err e
        | Ok rec_res -> Ok (item :: rec_res)
end

