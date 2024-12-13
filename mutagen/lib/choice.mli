(** Functionality to chose between elements of lists *)

val choose_uniform : 'a list -> Containers.Random.state -> 'a
(** [choose_uniform xs st] chooses an element from [xs] with equal probability. *)

val choose_weighted : ('a * float) list -> Containers.Random.state -> 'a
(** [choose_weighted xs st] chooses an element from [xs].

    Each element in [xs] consists of its value and its probability. *)
