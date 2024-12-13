module Cq = Caqti_lwt
open Lwt.Infix
open Containers

let as_int = function false -> 0 | true -> 1

let as_bool = function
  | 0 -> false
  | 1 -> true
  | n -> failwith @@ Fmt.str "We do binary, not tenary! %d is an invalid value..\n%!" n

module Generation = struct
  type t = { id : int; hash : int; payload : string; pretty : string; json : string; rng_state_id : int }
  [@@deriving show { with_path = false }, hash]
end

module Exec = struct
  type t = { result : string; serialized : string; executed : bool } [@@deriving show { with_path = false }, hash]

  let equal lhs rhs = String.equal_caseless lhs.result rhs.result && Bool.equal lhs.executed rhs.executed
end

module Browser_exec_status = struct
  type t = { error_message : string; status : int } [@@deriving show { with_path = false }, hash]

  let equal lhs rhs = Int.equal lhs.status rhs.status
end

module Browser_exec = struct
  type t = { browser : Util.Browser.t; result : string; serialized : string; executed : bool }
  [@@deriving show { with_path = false }, hash]

  let equal lhs rhs = String.equal_caseless lhs.result rhs.result && Bool.equal lhs.executed rhs.executed
end

module Q = struct
  open Caqti_request.Infix
  open Caqti_type.Std

  let generation_t =
    let open Generation in
    let encode { id; hash; payload; pretty; json; rng_state_id } =
      Ok (id, hash, payload, (pretty, json, rng_state_id))
    in
    let decode (id, hash, payload, (pretty, json, rng_state_id)) =
      Ok { id; hash; payload; pretty; json; rng_state_id }
    in
    let rep = Caqti_type.(tup4 int int string (tup3 string string int)) in
    custom ~encode ~decode rep

  let browser_t =
    let open Util.Browser in
    let encode { id; name } = Ok (id, name) in
    let decode (id, name) = Ok { id; name } in
    let rep = Caqti_type.(tup2 int string) in
    custom ~encode ~decode rep

  let sanitizer_t =
    let open Util.Sanitizer in
    let encode { id; name } = Ok (id, name) in
    let decode (id, name) = Ok { id; name } in
    let rep = Caqti_type.(tup2 int string) in
    custom ~encode ~decode rep

  let mode_t =
    let open Util.Mode in
    let encode { id; name } = Ok (id, name) in
    let decode (id, name) = Ok { id; name } in
    let rep = Caqti_type.(tup2 int string) in
    custom ~encode ~decode rep

  let browser_exec_status_t =
    let open Browser_exec_status in
    let encode { error_message; status } = Ok (error_message, status) in
    let decode (error_message, status) = Ok { error_message; status } in
    let rep = Caqti_type.(tup2 string int) in
    custom ~encode ~decode rep

  let exec_t =
    let open Exec in
    let encode { result; serialized; executed } = Ok (result, serialized, as_int executed) in
    let decode (result, serialized, executed) =
      let executed = as_bool executed in
      Ok { result; serialized; executed }
    in
    let rep = Caqti_type.(tup3 string string int) in
    custom ~encode ~decode rep

  let browser_exec_t =
    let open Browser_exec in
    let encode { browser; result; serialized; executed } = Ok (browser, result, serialized, as_int executed) in
    let decode (browser, result, serialized, executed) =
      let executed = as_bool executed in
      Ok { browser; result; serialized; executed }
    in
    let rep = Caqti_type.(tup4 browser_t string string int) in
    custom ~encode ~decode rep

  let compare_evaluations_query, compare_evaluations_limited_query, compare_evaluation_query =
    let base_query =
      "select g.id, g.payload, m.id, m.name, ec.browser_id, 'chromium', ec.result, ec.serialized, ec.executed, \
       ec.error_message, ec.status, ew.browser_id, 'webkit', ew.result, ew.serialized, ew.executed, ew.error_message, \
       ew.status, ef.browser_id, 'firefox', ef.result, ef.serialized, ef.executed, ef.error_message, ef.status from \
       evaluations ec join generations g on ec.gen_id = g.id join browsers bc on (bc.id = ec.browser_id and bc.name = \
       'chromium') join sanitizers ssc on (ec.sanitizer_id = ssc.id and ssc.name = 'no-sanitizer') join evaluations ew \
       on (ew.gen_id = ec.gen_id and ec.sanitizer_id = ew.sanitizer_id  and ew.mode_id = ec.mode_id  and ew.browser_id \
       = (select id from browsers where name ='webkit')) join evaluations ef on (ef.gen_id = ec.gen_id and \
       ec.sanitizer_id = ef.sanitizer_id  and ef.mode_id = ec.mode_id  and ef.browser_id = (select id from browsers \
       where name ='firefox')) join modes m on ec.mode_id = m.id"
    in

    ( (unit
      -->* tup3 int string
             (tup4 mode_t browser_exec_t browser_exec_status_t
                (tup3 browser_exec_t browser_exec_status_t (tup2 browser_exec_t browser_exec_status_t))))
        ~oneshot:false
      @:- base_query,
      (int
      -->* tup3 int string
             (tup4 mode_t browser_exec_t browser_exec_status_t
                (tup3 browser_exec_t browser_exec_status_t (tup2 browser_exec_t browser_exec_status_t))))
        ~oneshot:false
      @:- Fmt.str "%s limit ?" base_query,
      (int
      -->* tup3 int string
             (tup4 mode_t browser_exec_t browser_exec_status_t
                (tup3 browser_exec_t browser_exec_status_t (tup2 browser_exec_t browser_exec_status_t))))
        ~oneshot:false
      @:- Fmt.str "%s where ec.gen_id = ?" base_query )

  let get_diverging_execs, get_diverging_exec, get_diverging_execs_limit =
    let query =
      "select g.id, g.payload, e1.sanitized_id, s2.id, s2.name, b.id, b.name, e1.mode_id, 'innerHTML', e1.result, \
       e1.serialized, e1.executed, e2.mode_id, 'document_write', e2.result, e2.serialized, e2.executed, e3.mode_id, \
       'set_content', e3.result, e3.serialized, e3.executed \n\
       from evaluations e1 join sanitized s on e1.sanitized_id = s.id join generations g on e1.gen_id = g.id join \
       sanitizers\n\
       s2 on s.sanitizer_id = s2.id join browsers b on e1.browser_id = b.id join\n\
       evaluations e2 on (e1.sanitized_id = e2.sanitized_id and  e2.mode_id = (select id from modes where name = \
       'document_write') and e1.browser_id = e2.browser_id) join evaluations e3 on\n\
       (e3.sanitized_id = e1.sanitized_id and e3.mode_id =\n\
      \                                      (select id from modes where name = 'set_content')  and e1.browser_id = \
       e3.browser_id) where e1.mode_id = (select id from modes where name = 'innerHTML')\n\
      \                and (e1.executed = 1 or e2.executed = 1 or e3.executed = 1) and not (e1.executed = 1 and \
       e2.executed = 1 and e3.executed = 1) and g.pretty not like '%Script%'"
    in
    ( (unit
      -->* tup4 int string int
             (tup3 sanitizer_t browser_t (tup3 mode_t exec_t (tup3 mode_t exec_t (tup2 mode_t exec_t)))))
        ~oneshot:false
      @:- query,
      (int
      -->* tup4 int string int
             (tup3 sanitizer_t browser_t (tup3 mode_t exec_t (tup3 mode_t exec_t (tup2 mode_t exec_t)))))
        ~oneshot:false
      @:- Fmt.str "%s and g.id = ?" query,
      (int
      -->* tup4 int string int
             (tup3 sanitizer_t browser_t (tup3 mode_t exec_t (tup3 mode_t exec_t (tup2 mode_t exec_t)))))
        ~oneshot:false
      @:- Fmt.str "%s limit ?" query )

  let get_full_generation_query =
    (int -->! generation_t) ~oneshot:false
    @:- "select g.id, g.hash, g.payload, g.pretty, g.json, g.rng_state_id from generations g where g.id = ?"

  let get_modes_query = (unit -->* mode_t) ~oneshot:false @:- "select m.id, m.name from modes m"
  let get_sanitizers_query = (unit -->* sanitizer_t) ~oneshot:false @:- "select s.id, s.name from sanitizers s"

  let get_eval_sanitizers_query =
    (unit -->* sanitizer_t) ~oneshot:false
    @:- "select s.id, s.name from sanitizers s where s.name not in ('no-sanitizer', 'dompurify (current, node22)', \
         'sanitize-html (node)', 'typo3-fix', 'typo3-fix-lax', 'typo3-fix2', 'typo3-fix2-lax', 'html-rule-fix', \
         'html-rule-fix-lax', 'ganss-fix', 'ganss-fix-lax', 'antisamy-anythinggoes')"

  let get_browsers_query = (unit -->* browser_t) ~oneshot:false @:- "select b.id, b.name from browsers b"
  let get_hashes_query = (unit -->* int) ~oneshot:false @:- "SELECT hash from generations"

  let add_rng_state_query =
    (string -->! int) ~oneshot:false @:- "INSERT INTO rng_state (state) VALUES (?)  RETURNING id"

  let add_generated_query =
    (tup4 int string string (tup2 string int) -->. unit) ~oneshot:false @:- "call insert_generation(?, ?, ?, ?, ?)"

  let get_generation_query =
    (int -->! tup2 string string) ~oneshot:false @:- "select g.payload, g.pretty from generations as g WHERE g.id = ?"

  let get_reference_executed_light_query =
    (int -->* tup2 int (tup3 int int (tup3 string string (tup3 string string int)))) ~oneshot:false
    @:- "select e.gen_id, e.browser_id, e.mode_id, g.payload, g.json, e.serialized, e.result, e.executed from \
         evaluations e join generations g on e.gen_id = g.id where e.sanitizer_id = (select id from sanitizers where \
         name = 'no-sanitizer') and e.gen_id = ?"

  let get_executed_light_query =
    (int -->* tup2 int (tup4 int int int (tup3 string string (tup3 string string (tup3 string string int)))))
      ~oneshot:false
    @:- "select e.gen_id, s.sanitizer_id, e.browser_id, e.mode_id, g.payload, g.pretty, s.serialized, s.output, \
         e.serialized, e.result, e.executed from evaluations e join sanitized s on e.sanitized_id = s.id join \
         generations g on s.gen_id = g.id where s.sanitizer_id in (select id from sanitizers where name not in\n\
        \         ('no-sanitizer', 'dompurify (current, node22)', 'sanitize-html (node)', 'typo3-fix', \
         'typo3-fix-lax', 'typo3-fix2', 'typo3-fix2-lax', 'html-rule-fix', 'html-rule-fix-lax', 'ganss-fix', \
         'ganss-fix-lax', 'antisamy-anythinggoes')) and e.gen_id = ?"

  let get_reference_executed_range_light_query =
    (tup2 int int -->* tup2 int (tup3 int int (tup3 string string (tup3 string string int)))) ~oneshot:false
    @:- "select e.gen_id, e.browser_id, e.mode_id, g.payload, g.json, e.serialized, e.result, e.executed from \
         evaluations e join generations g on e.gen_id = g.id where e.sanitizer_id = (select id from sanitizers where \
         name = 'no-sanitizer') and e.gen_id >= ? and e.gen_id <= ?"

  let get_executed_range_light_query =
    (tup2 int int -->* tup2 int (tup4 int int int (tup3 string string (tup3 string string (tup3 string string int)))))
      ~oneshot:false
    @:- "select e.gen_id, s.sanitizer_id, e.browser_id, e.mode_id, g.payload, g.pretty, s.serialized, s.output, \
         e.serialized, e.result, e.executed from evaluations e join sanitized s on e.sanitized_id = s.id join \
         generations g on s.gen_id = g.id where s.sanitizer_id  in (select id from sanitizers where name not in\n\
        \         ('no-sanitizer', 'dompurify (current, node22)', 'sanitize-html (node)', 'typo3-fix', \
         'typo3-fix-lax', 'typo3-fix2', 'typo3-fix2-lax', 'html-rule-fix', 'html-rule-fix-lax', 'ganss-fix', \
         'ganss-fix-lax', 'antisamy-anythinggoes')) and e.gen_id >= ? and e.gen_id <= ? order by e.gen_id"

  let get_evaluations_query =
    (int -->* tup3 string int (tup3 string int (tup3 string string int))) ~oneshot:false
    @:- "select b.name, b.id, s.name, s.id, e.result, e.serialized, e.executed from evaluations e join sanitizers s on \
         e.sanitizer_id = s.id join browsers b on e.browser_id = b.id where e.gen_id = ?"

  let get_sanitizer_results_light_query =
    (int -->* tup3 sanitizer_t string string) ~oneshot:false
    @:- "select ss.id, ss.name, s.output, s.serialized from sanitized s join sanitizers ss on s.sanitizer_id = ss.id \
         where s.gen_id = ? and ss.name != 'no-sanitizer'"

  let get_sanitizer_results_range_light_query =
    (tup2 int int -->* tup4 int sanitizer_t string string) ~oneshot:false
    @:- "select s.gen_id, ss.id, ss.name, s.output, s.serialized from sanitized s join sanitizers ss on s.sanitizer_id \
         = ss.id where s.gen_id >= ? and s.gen_id <= ? and ss.name != 'no-sanitizer'"

  let get_sanitizer_results_query =
    (int -->* tup4 int string string (tup2 sanitizer_t (tup3 string string int))) ~oneshot:false
    @:- "select g.id, g.payload, g.json, ss.id, ss.name, s.output, s.serialized, s.done from sanitized s join \
         sanitizers ss on s.sanitizer_id = ss.id join generations g on s.gen_id = g.id  where s.gen_id = ?"

  let get_browser_results_range_light_query =
    (tup2 int int -->* tup3 int string (tup3 browser_t mode_t (tup3 string string int))) ~oneshot:false
    @:- "select g.id, g.payload, b.id, b.name, m.id, m.name, e.result, e.serialized, e.executed from evaluations e \
         join browsers b on e.browser_id = b.id join sanitizers ss on e.sanitizer_id = ss.id join modes m on e.mode_id \
         = m.id join generations g on e.gen_id = g.id  where e.status = 0 and ss.name = 'no-sanitizer' and e.gen_id >= \
         ? and e.gen_id <= ?"

  let get_browser_sanitized_results_all_light_query =
    (unit -->* tup2 int (tup4 int int int (tup2 string int))) ~oneshot:false
    @:- "with execs as (\n\
        \    select e.gen_id, count(*) as cnt from evaluations e group by e.gen_id order by cnt desc\n\
         ) select e.gen_id, b.id, m.id, ss.id, e.serialized, e.executed from evaluations e join browsers b on \
         e.browser_id = b.id join sanitizers ss on e.sanitizer_id = ss.id join modes m on e.mode_id = m.id where \
         e.status = 0 and e.gen_id in (select e.gen_id from execs e where e.cnt = 180)"

  let get_browser_sanitized_results_range_light_query =
    (tup2 int int -->* tup2 int (tup4 int int int (tup2 string int))) ~oneshot:false
    @:- "select e.gen_id, b.id, m.id, ss.id, e.serialized, e.executed from evaluations e join browsers b on \
         e.browser_id = b.id join sanitizers ss on e.sanitizer_id = ss.id join modes m on e.mode_id = m.id where \
         e.status = 0 and e.gen_id >= ? and e.gen_id <= ?"

  let get_browser_sanitized_result_light_query =
    (int -->* tup2 int (tup4 int int int (tup2 string int))) ~oneshot:false
    @:- "select e.gen_id, b.id, m.id, ss.id, e.serialized, e.executed from evaluations e join browsers b on \
         e.browser_id = b.id join sanitizers ss on e.sanitizer_id = ss.id join modes m on e.mode_id = m.id where \
         e.status = 0 and e.gen_id = ?"

  let get_browser_results_query =
    (int -->* tup4 int string string (tup3 browser_t mode_t (tup3 string string int))) ~oneshot:false
    @:- "select g.id, g.payload, g.json, b.id, b.name, m.id, m.name, e.result, e.serialized, e.executed from \
         evaluations e join browsers b on e.browser_id = b.id join sanitizers ss on e.sanitizer_id = ss.id join modes \
         m on e.mode_id = m.id join generations g on e.gen_id = g.id  where e.status = 0 and ss.name = 'no-sanitizer' \
         and e.gen_id = ?"
end

let or_die ?(info = "error") () = function
  | Ok r -> r
  | Error (num, msg) -> failwith @@ Fmt.str "%s: (#%d) %s" info num msg

let connect config =
  let module C = Config in
  let uri = Fmt.str "postgresql://%s:%s@%s:%d/" config.C.username config.C.password config.C.hostname config.C.port in
  let uri = uri |> Uri.of_string in
  Cq.connect_pool uri ~max_size:50 |> Cq.or_fail

let get_diverging_execs pool =
  let get_diverging_execs' (module C : Cq.CONNECTION) =
    C.fold Q.get_diverging_execs
      (fun (gen_id, payload, sanitized_id, (sanitizer, browser, (mode1, exec1, (mode2, exec2, (mode3, exec3))))) acc ->
        (gen_id, payload, sanitized_id, sanitizer, browser, (mode1, exec1), (mode2, exec2), (mode3, exec3)) :: acc)
      () []
  in
  Cq.Pool.use get_diverging_execs' pool >>= Cq.or_fail

let get_diverging_execs pool limit =
  let get_diverging_execs' query base (module C : Cq.CONNECTION) =
    C.fold query
      (fun (gen_id, payload, sanitized_id, (sanitizer, browser, (mode1, exec1, (mode2, exec2, (mode3, exec3))))) acc ->
        (gen_id, payload, sanitized_id, sanitizer, browser, (mode1, exec1), (mode2, exec2), (mode3, exec3)) :: acc)
      base []
  in
  match limit with
  | None -> Cq.Pool.use (get_diverging_execs' Q.get_diverging_execs ()) pool >>= Cq.or_fail
  | Some l -> Cq.Pool.use (get_diverging_execs' Q.get_diverging_execs_limit l) pool >>= Cq.or_fail

let get_diverging_exec pool id =
  let get_diverging_exec' (module C : Cq.CONNECTION) =
    C.fold Q.get_diverging_exec
      (fun (gen_id, payload, sanitized_id, (sanitizer, browser, (mode1, exec1, (mode2, exec2, (mode3, exec3))))) acc ->
        (gen_id, payload, sanitized_id, sanitizer, browser, (mode1, exec1), (mode2, exec2), (mode3, exec3)) :: acc)
      id []
  in
  Cq.Pool.use get_diverging_exec' pool >>= Cq.or_fail

let compare_evaluation pool id =
  let compare_evaluation' (module C : Cq.CONNECTION) =
    C.fold Q.compare_evaluation_query
      (fun (gen_id, payload, (mode, chrome, chrome_status, (webkit, webkit_s, (firefox, firefox_s)))) acc ->
        (gen_id, payload, mode, chrome, chrome_status, webkit, webkit_s, firefox, firefox_s) :: acc)
      id []
  in
  Cq.Pool.use compare_evaluation' pool >>= Cq.or_fail

let compare_evaluations pool limit =
  let compare_evaluations' query base (module C : Cq.CONNECTION) =
    C.fold query
      (fun (gen_id, payload, (mode, chrome, chrome_status, (webkit, webkit_s, (firefox, firefox_s)))) acc ->
        (gen_id, payload, mode, chrome, chrome_status, webkit, webkit_s, firefox, firefox_s) :: acc)
      base []
  in
  match limit with
  | None -> Cq.Pool.use (compare_evaluations' Q.compare_evaluations_query ()) pool >>= Cq.or_fail
  | Some l -> Cq.Pool.use (compare_evaluations' Q.compare_evaluations_limited_query l) pool >>= Cq.or_fail

let get_modes pool =
  let get_modes' (module C : Cq.CONNECTION) = C.collect_list Q.get_modes_query () in
  Cq.Pool.use get_modes' pool >>= Cq.or_fail

let get_browsers pool =
  let get_browsers' (module C : Cq.CONNECTION) = C.collect_list Q.get_browsers_query () in
  Cq.Pool.use get_browsers' pool >>= Cq.or_fail

let get_sanitizers pool =
  let get_sanitizers' (module C : Cq.CONNECTION) = C.collect_list Q.get_sanitizers_query () in
  Cq.Pool.use get_sanitizers' pool >>= Cq.or_fail

let get_eval_sanitizers pool =
  let get_sanitizers' (module C : Cq.CONNECTION) = C.collect_list Q.get_eval_sanitizers_query () in
  Cq.Pool.use get_sanitizers' pool >>= Cq.or_fail

let get_browser_results_range_light pool l u =
  let get_browser_results' (module C : Cq.CONNECTION) =
    C.fold Q.get_browser_results_range_light_query
      (fun (gen_id, payload, (browser, mode, (output, serialized, executed))) acc ->
        (gen_id, payload, browser, mode, output, serialized, as_bool executed) :: acc)
      (l, u) []
  in
  Cq.Pool.use get_browser_results' pool >>= Cq.or_fail

let get_browser_sanitized_results_all_light pool =
  let get_browser_results' (module C : Cq.CONNECTION) =
    C.fold Q.get_browser_sanitized_results_all_light_query
      (fun (gen_id, (browser, mode, sanitizer, (serialized, executed))) acc ->
        (gen_id, browser, mode, sanitizer, serialized, as_bool executed) :: acc)
      () []
  in
  Cq.Pool.use get_browser_results' pool >>= Cq.or_fail

let get_browser_sanitized_results_range_light pool l u =
  let get_browser_results' (module C : Cq.CONNECTION) =
    C.fold Q.get_browser_sanitized_results_range_light_query
      (fun (gen_id, (browser, mode, sanitizer, (serialized, executed))) acc ->
        (gen_id, browser, mode, sanitizer, serialized, as_bool executed) :: acc)
      (l, u) []
  in
  Cq.Pool.use get_browser_results' pool >>= Cq.or_fail

let get_browser_sanitized_result_light pool id =
  let get_browser_result' (module C : Cq.CONNECTION) =
    C.fold Q.get_browser_sanitized_result_light_query
      (fun (gen_id, (browser, mode, sanitizer, (serialized, executed))) acc ->
        (gen_id, browser, mode, sanitizer, serialized, as_bool executed) :: acc)
      id []
  in
  Cq.Pool.use get_browser_result' pool >>= Cq.or_fail

let get_browser_results pool id =
  let get_browser_results' (module C : Cq.CONNECTION) =
    C.fold Q.get_browser_results_query
      (fun (gen_id, payload, json, (browser, mode, (output, serialized, executed))) acc ->
        (gen_id, payload, json, browser, mode, output, serialized, as_bool executed) :: acc)
      id []
  in
  Cq.Pool.use get_browser_results' pool >>= Cq.or_fail

let get_sanitizer_results_range_light pool lower upper =
  let get_sanitizer_results' (module C : Cq.CONNECTION) =
    C.collect_list Q.get_sanitizer_results_range_light_query (lower, upper)
  in
  Cq.Pool.use get_sanitizer_results' pool >>= Cq.or_fail

let get_sanitizer_results_light pool id =
  let get_sanitizer_results' (module C : Cq.CONNECTION) = C.collect_list Q.get_sanitizer_results_light_query id in
  Cq.Pool.use get_sanitizer_results' pool >>= Cq.or_fail

let get_sanitizer_results pool id =
  let get_sanitizer_results' (module C : Cq.CONNECTION) =
    C.fold Q.get_sanitizer_results_query
      (fun (gen_id, payload, json, (sanitizer, (output, serialized, is_done))) acc ->
        (gen_id, payload, json, sanitizer, output, serialized, is_done) :: acc)
      id []
  in
  Cq.Pool.use get_sanitizer_results' pool >>= Cq.or_fail

let get_executed_reference_light pool id =
  let results (module C : Cq.CONNECTION) =
    C.fold Q.get_reference_executed_light_query
      (fun (gen_id, (browser_id, mode_id, (payload, json, (dom, result, exec)))) acc ->
        (gen_id, browser_id, mode_id, payload, json, dom, result, as_bool exec) :: acc)
      id []
  in
  Cq.Pool.use results pool >>= Cq.or_fail

let get_executed_light pool id =
  let results (module C : Cq.CONNECTION) =
    C.fold Q.get_executed_light_query
      (fun (gen_id, (sanitizer_id, browser_id, mode_id, (payload, pretty, (serialized, output, (dom, result, exec)))))
           acc ->
        (gen_id, sanitizer_id, browser_id, mode_id, payload, pretty, serialized, output, dom, result, as_bool exec)
        :: acc)
      id []
  in
  Cq.Pool.use results pool >>= Cq.or_fail

let get_executed_reference_range_light pool l u =
  let results (module C : Cq.CONNECTION) =
    C.fold Q.get_reference_executed_range_light_query
      (fun (gen_id, (browser_id, mode_id, (payload, json, (dom, result, exec)))) acc ->
        (gen_id, browser_id, mode_id, payload, json, dom, result, as_bool exec) :: acc)
      (l, u) []
  in
  Cq.Pool.use results pool >>= Cq.or_fail

let get_executed_range_light pool l u =
  let results (module C : Cq.CONNECTION) =
    C.fold Q.get_executed_range_light_query
      (fun (gen_id, (sanitizer_id, browser_id, mode_id, (payload, pretty, (serialized, output, (dom, result, exec)))))
           acc ->
        (gen_id, sanitizer_id, browser_id, mode_id, payload, pretty, serialized, output, dom, result, as_bool exec)
        :: acc)
      (l, u) []
  in
  Cq.Pool.use results pool >>= Cq.or_fail

let get_hashes pool =
  let get_hashes' (module C : Cq.CONNECTION) = C.fold Q.get_hashes_query (fun id acc -> id :: acc) () [] in
  Cq.Pool.use get_hashes' pool >>= Cq.or_fail

let get_generation pool id =
  let get_generation' (module C : Cq.CONNECTION) = C.find Q.get_generation_query id in
  Cq.Pool.use get_generation' pool >>= Cq.or_fail

let get_full_generation pool id =
  let get_generation' (module C : Cq.CONNECTION) = C.find Q.get_full_generation_query id in
  Cq.Pool.use get_generation' pool >>= Cq.or_fail

let get_evaluations pool id =
  let get_evaluations' (module C : Cq.CONNECTION) =
    C.fold Q.get_evaluations_query
      (fun (bname, bid, (sname, sid, (result, serialized, executed))) acc ->
        ((bname, bid), (sname, sid), (result, serialized, executed)) :: acc)
      id []
  in
  Cq.Pool.use get_evaluations' pool >>= Cq.or_fail

let add_rng_state pool rng_state =
  let add' gen (module C : Cq.CONNECTION) = C.find Q.add_rng_state_query gen in
  Cq.Pool.use (add' rng_state) pool >>= Cq.or_fail

let add_generated pool hash payload pretty json rng_state_id =
  let add' gen (module C : Cq.CONNECTION) = C.exec Q.add_generated_query gen in
  Cq.Pool.use (add' (hash, payload, pretty, (json, rng_state_id))) pool >>= Cq.or_fail

let get_sanitizers_table pool =
  let ( let* ) = Lwt.bind in
  let* sanitizers = get_sanitizers pool in
  sanitizers |> List.map (fun s -> (s.Util.Sanitizer.id, s.Util.Sanitizer.name)) |> Hashtbl.of_list |> Lwt.return

let get_eval_sanitizers_table pool =
  let ( let* ) = Lwt.bind in
  let* sanitizers = get_eval_sanitizers pool in
  sanitizers |> List.map (fun s -> (s.Util.Sanitizer.id, s.Util.Sanitizer.name)) |> Hashtbl.of_list |> Lwt.return

let get_browsers_table pool =
  let ( let* ) = Lwt.bind in
  let* browsers = get_browsers pool in
  browsers |> List.map (fun s -> (s.Util.Browser.id, s.Util.Browser.name)) |> Hashtbl.of_list |> Lwt.return

let get_modes_table pool =
  let ( let* ) = Lwt.bind in
  let* modes = get_modes pool in
  modes |> List.map (fun s -> (s.Util.Mode.id, s.Util.Mode.name)) |> Hashtbl.of_list |> Lwt.return
