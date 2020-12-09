open Graph 
open Tools

(* A path is a list of nodes. *)
type path = id list


(* find_path gr forbidden id1 id2 
 *   returns None if no path can be found.
 *   returns Some p if a path p from id1 to id2 has been found. 
 *
 *  forbidden is a list of forbidden nodes (they have already been visited)
*)
val init_gr:int graph -> label graph

val graph_ecart: label graph -> int graph

val find_path_rev: int graph -> id -> id -> path option

val find_path: int graph -> id -> id -> path option

val calcul_flot_min_path : int -> int graph -> id -> path -> int

val print_path : path option -> unit

val maj_flot_path : int graph -> int -> id -> path -> int graph

val maj_graph : int graph -> label graph -> int -> (label graph * int)

val ford : label graph -> id -> id -> (label graph * int)
