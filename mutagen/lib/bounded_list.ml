type 'a t = { length : int ref; content : 'a list ref; bound : int } [@@deriving show { with_path = false }, eq]

let create b = { length = ref 0; content = ref []; bound = b }

let add (lst : 'a t) (item : 'a) =
  if !(lst.length) < lst.bound then
    let () = lst.length := !(lst.length) + 1 in
    let () = lst.content := item :: !(lst.content) in
    ()
  else ()

let content (lst : 'a t) = !(lst.content)