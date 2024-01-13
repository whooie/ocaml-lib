exception Unwrap of string

exception Expect of string

let is_some (opt: 'a option): bool =
  match opt with
  | Some _ -> true
  | None -> false

let is_some_and (f: 'a -> bool) (opt: 'a option): bool =
  match opt with
  | Some a -> f a
  | None -> false

let is_none (opt: 'a option): bool =
  match opt with
  | Some _ -> false
  | None -> true

let some_and (opt_a: 'a option) (opt_b: 'b option): 'b option =
  match opt_a with
  | Some _ -> opt_b
  | None -> None

let some_and_then (f: 'a -> 'b option) (opt: 'a option): 'b option =
  match opt with
  | Some a -> f a
  | None -> None

let some_then (f: 'a -> unit) (opt: 'a option): unit =
  match opt with
  | Some a -> f a
  | None -> ()

let some_or (opt_a: 'a option) (opt_b: 'a option): 'a option =
  match opt_a with
  | Some a -> Some a
  | None -> opt_b

let some_or_else (f: unit -> 'a option) (opt: 'a option): 'a option =
  match opt with
  | Some a -> Some a
  | None -> f ()

let some_xor (opt_a: 'a option) (opt_b: 'a option): 'a option =
  match (opt_a, opt_b) with
  | (Some a, None) -> Some a
  | (None, Some b) -> Some b
  | _ -> None

let zip (opt_a: 'a option) (opt_b: 'b option): ('a * 'b) option =
  match (opt_a, opt_b) with
  | (Some a, Some b) -> Some (a, b)
  | _ -> None

let unzip (opt: ('a * 'b) option): ('a option) * ('b option) =
  match opt with
  | Some (a, b) -> (Some a, Some b)
  | None -> (None, None)

let flatten (opt: 'a option option): 'a option =
  match opt with
  | Some (Some a) -> Some a
  | _ -> None

let map (f: 'a -> 'b) (opt: 'a option): 'b option =
  match opt with
  | Some a -> Some (f a)
  | None -> None

let map_or (f: 'a -> 'b) (default: 'b) (opt: 'a option): 'b =
  match opt with
  | Some a -> f a
  | None -> default

let map_or_else (f_some: 'a -> 'b) (f_none: unit -> 'b) (opt: 'a option): 'b =
  match opt with
  | Some a -> f_some a
  | None -> f_none ()

let ok_or (err: 'e) (opt: 'a option): ('a, 'e) result =
  match opt with
  | Some a -> Ok a
  | None -> Error err

let ok_or_else (f: unit -> 'e) (opt: 'a option): ('a, 'e) result =
  match opt with
  | Some a -> Ok a
  | None -> Error (f ())

let unwrap_or (default: 'a) (opt: 'a option): 'a =
  match opt with
  | Some a -> a
  | None -> default

let unwrap_or_else (f: unit -> 'a) (opt: 'a option): 'a =
  match opt with
  | Some a -> a
  | None -> f ()

let unwrap ?(msg: string = "") (opt: 'a option): 'a =
  match opt with
  | Some a -> a
  | None -> raise (Unwrap msg)

let expect (msg: string) (opt: 'a option): 'a =
  match opt with
  | Some a -> a
  | None -> raise (Expect msg)

let rec collect_list (items: 'a option list): 'a list option =
  match items with
  | [] -> Some []
  | None :: _ -> None
  | (Some item) :: tail ->
      match collect_list tail with
      | None -> None
      | Some rec_res -> Some (item :: rec_res)

let rec collect_list_of_seq (items: 'a option Seq.t): 'a list option =
  match items () with
  | Seq.Nil -> Some []
  | Seq.Cons (None, _) -> None
  | Seq.Cons (Some item, rest) ->
      match collect_list_of_seq rest with
      | None -> None
      | Some rec_res -> Some (item :: rec_res)

