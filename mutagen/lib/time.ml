type t = Ptime.date * Ptime.time

let current_time () =
  let ct = Ptime_clock.now () in
  let d, t = Ptime.to_date_time ct in
  (d, t)

let pp pf (d, t) =
  let year, month, day = d in
  let (hh, mm, ss), _ = t in
  Fmt.pf pf "%d-%d-%d-%d-%d-%d" year month day hh mm ss
