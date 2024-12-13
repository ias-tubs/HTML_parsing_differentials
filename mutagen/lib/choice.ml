open Containers

type 'a t = { x : 'a; prob : float } [@@deriving show]

(** [normalize xs] normalizes the probability of a list of {!type:t}s *)
let normalize (xs : 'a t list) =
  let sum = List.fold_left (fun a c -> a +. c.prob) 0.0 xs in
  xs |> List.map (fun x -> { x with prob = x.prob /. sum })

(** [init_choices xs] turns a list of items into a list of {!type:t}s, all with the same probability. *)
let init_choices (xs : 'a list) = xs |> List.map (fun e -> { x = e; prob = 1.0 }) |> normalize

let normalize' xs =
  let sum = List.fold_left ( +. ) 0.0 xs in
  xs |> List.map (fun x -> x /. sum)

let cumsum xs =
  let _, b =
    xs
    |> List.fold_left
         (fun (acc, l) x ->
           let acc = acc +. x in
           (acc, acc :: l))
         (0.0, [])
  in
  b |> List.rev

let choose_int st f xs =
  let p = Random.float 1.0 st in
  let ps = xs |> List.map f in
  ps |> cumsum |> List.find_idx (fun x -> x >=. p)

let choose' st xs = choose_int st Fun.id xs

let make_choice st f xs =
  let r = choose_int st f xs in
  let i, _ = Option.get_exn_or "None" r in
  List.get_at_idx_exn i xs

let choose_maybe st f x =
  let p = Random.float 1.0 st in
  if p <. f x then Some x else None

let choose_plus st f x =
  let rs = ref [ x ] in
  let p = ref (Random.float 1.0 st) in
  while !p <. f x do
    p := Random.float 1.0 st;
    rs := x :: !rs
  done;
  !rs

let choose_until st f x =
  let rs = ref [] in
  let p = ref (Random.float 1.0 st) in
  while !p <. f x do
    p := Random.float 1.0 st;
    rs := x :: !rs
  done;
  !rs

(** Choose an element from xs *)
let choose rng xs = make_choice rng (fun c -> c.prob) xs

(** Chose an element from the list xs, they all have the same probability *)
let choose_uniform xs rng =
  let choices = init_choices xs in
  (choose rng choices).x

(** Choose an element from the list xs, each element has its probability attached *)
let choose_weighted (xs : ('a * float) list) rng =
  let choices = xs |> List.map (fun (x, p) -> { x; prob = p }) |> normalize in
  (choose rng choices).x
