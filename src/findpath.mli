open Graph
open Printf

(* A path is a list of nodes. *)
type path = id list;;

(* find_path gr forbidden id1 id2 
 *   returns None if no path can be found.
 *   returns Some p if a path p from id1 to id2 has been found. 
 *
 *  forbidden is a list of forbidden nodes (they have already been visited)
*)


val find_path_core: int graph -> id list -> id -> id -> path option

val find_path: int graph -> id list -> id -> id -> path option

val traiter_out_arcs: 'a graph -> 'a out_arcs -> int list -> int -> path option

