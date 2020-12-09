(* Yes, we have to repeat open Graph. *)
open Graph

type label = {flow : int; capacity : int}
(* assert false is of type ∀α.α, so the type-checker is happy. *)
let clone_nodes (gr:'a graph) = let new_gr = empty_graph 
  in n_fold gr (fun g id -> new_node g id) new_gr

let gmap gr f = let new_gr = clone_nodes gr 
  in e_fold gr (fun g id1 id2 label ->  new_arc g id1 id2 (f label)) new_gr

let add_arc g id1 id2 n = let arc = find_arc g id1 id2 in
    match arc with 
      |None -> new_arc g id1 id2 n 
      |Some a -> new_arc g id1 id2 (a+n)

let string_of_label label = "{"^string_of_int label.flow^" / "^string_of_int label.capacity^"}" 
