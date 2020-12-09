open Graph
open Tools

(* A path is a list of nodes. *)
type path = id list



(*initialise chaque arc avec un label composé d'un flow (=0) et d'une capacité*)
let init_gr gr = gmap gr (fun x -> {flow = 0; capacity =x})



(*construction du graph d'ecart*)
let graph_ecart gr = 
  gmap gr (fun label -> label.capacity) 



(* find_path gr forbidden id1 id2 
 *   returns None if no path can be found.
 *   returns Some p if a path p from id1 to id2 has been found. 
 *
 *  forbidden is a list of forbidden nodes (they have already been visited)
*)

let find_path_rev gr id1 id2 = 

  let rec aux current forbidden path = 

    (*si on est au puits on termine et renvoie le path*)

    if current = id2 then Some (current::path) else 

      (*sinon on appelle aux2 avec les arcs sortants du noeud courant*)
      aux2 current forbidden path (out_arcs gr current)

  and aux2 current forbidden path = function 

    |[] -> None
    |(x, label)::rest -> 

        if List.mem x forbidden || label<=0 then 
          (*on peut pas prendre cet arc*)
          aux2 current forbidden path rest 
        else 
          (*on peut explorer cet arc*)
          begin match aux x (current::forbidden) (current::path) with
            |None -> aux2 current forbidden path rest
            |Some p -> Some p
          end

  in

    aux id1 [] []


(* remet à l'endroit le chemin que l'on construit à l'envers*)
let find_path gr id1 id2 = 

  let path_rev = find_path_rev gr id1 id2 in

    match path_rev with 
      |Some x -> Some (List.rev x) 
      |None -> None



(* calcul le flot minimum d'un chemin donné, path en paramètre*)
let rec calcul_flot_min_path min gr origine = function
  |[] -> min
  |(x::rest) -> 
      let label_option = find_arc gr origine x in

      let label = match label_option with
        |Some x -> x
        |None -> assert false in

        (*si le label de l'arc est inférieur à min on recommence avec label sinon avec min*)
        if label <min then calcul_flot_min_path label gr x rest 
        else calcul_flot_min_path min gr x rest



(*affiche un chemin*)
let rec print_path = function 
  |None -> Printf.printf "No path found \n%!"
  |Some [] -> ()
  |Some (x::rest) -> Printf.printf "-> %d" x;
      print_path (Some rest)



(*maj des flots du graph d'ecart avec la variation de flot calculée*)
let rec maj_flot_path gr_ecart var_flot origine = function
  |[] -> gr_ecart
  |x::rest -> 
      let endroit = add_arc gr_ecart origine x (-var_flot) in
      let envers = add_arc endroit x origine var_flot in
        maj_flot_path envers var_flot x rest


(*maj du graph grace au graph d'ecart, on renvoie le nouveau graph et le debit *)
let maj_graph gr_ecart gr debit = 

  let construct graph id1 id2 lbl =

    let arc = find_arc gr id1 id2 in
      match arc with 
        (*on ne trouve pas l'arc à l'envers dans le graph original*)
        |None -> graph
        (*on trouve l'arc à l'envers*)
        |Some label -> new_arc graph id1 id2 {flow = label.capacity-lbl ; capacity = label.capacity }
  in
    (e_fold gr_ecart construct (clone_nodes gr_ecart), debit)




(*algo de ford on renvoie le graph final et le debit final en appliquant ford*)
let ford gr origine dest = 

  let gr_ecart = graph_ecart gr in

  let rec aux gr_ecart origine dest debit = 
    (*on cherche des chemins tant qu'il y en a*)
    let path = find_path gr_ecart origine dest in

      match path with
        |None -> (gr_ecart, debit)
        |Some [] -> assert false
        |Some (x::rest) -> 
            assert(x=origine); 

            (*on calcule la variation de flot, on maj le graph d'écart et on reboucle*)
            let var_flot = calcul_flot_min_path max_int gr_ecart x rest in

            let gr_ecart_new = maj_flot_path gr_ecart var_flot x rest in

              aux gr_ecart_new origine dest (debit+var_flot)

  in

  let gr_ecart_fin = aux gr_ecart origine dest 0 in

  (*on recupere le graph et le debit séparément*)
  let graph_final = match gr_ecart_fin with
    |(x,y) -> x in

  let debit = match gr_ecart_fin with 
    |(x,y) -> y in

    maj_graph graph_final gr debit































