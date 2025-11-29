(** [fields s] splits [s] around each instance of one or more consecutive
whitespace characters. It returns an empty list if [s] consists only of
whitespace or is empty. Every returned element is non-empty; the whitespace
characters are discarded. Whitespace characters are ' ', '\t', '\n', and '\r'.

This was heavily inspired by Go's strings.Fields, see
https://pkg.go.dev/strings#Fields. *)
val fields : string -> string list
