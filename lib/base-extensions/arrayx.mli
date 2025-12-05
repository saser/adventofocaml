(** [max_idx ~compare] returns the index of the first maximum element as defined
by [compare]. *)
val max_idx : ?pos:int -> ?len:int -> 'a array -> compare:('a -> 'a -> int) -> int option
