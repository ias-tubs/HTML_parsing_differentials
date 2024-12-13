open Containers

type t = Mtime.span

let pr pf ts =
  let ns = Mtime.Span.to_float_ns ts in
  match ns with
  | n when n <. 1000. -> Fmt.pf pf "%f ns" n
  | n when n <. 1000000. -> Fmt.pf pf "%f us" (n /. 1000.)
  | n when n <. 1000000000. -> Fmt.pf pf "%f ms" (n /. 1000000.)
  | n when n <. 60000000000. -> Fmt.pf pf "%f s" (n /. 1000000000.)
  | n when n >=. 60000000000. -> Fmt.pf pf "%f min" (n /. 60000000000.)
  | u -> failwith (Fmt.str "Unmatched case: %f ns" u)
(*| n -> Fmt.pf pf "%f s" (n/.1000.)*)

let of_float_ns ts = ts |> Mtime.Span.of_float_ns |> Option.get_exn_or "Unable to convert"
let to_float_ns ts = Mtime.Span.to_float_ns ts
let diff t1 t2 = Mtime.Span.abs_diff t1 t2
let add t1 t2 = Mtime.Span.add t1 t2
let zero () = Mtime.Span.zero
let timestamp () = Mtime_clock.elapsed ()
