open Graph
open Option

(* assert false is of type ∀α.α, so the type-checker is happy. *)
let add_arc gr id1 id2 n = match (find_arc gr id1 id2) with 
  |None -> new_arc gr id1 id2 n
  |label -> new_arc gr id1 id2 (n+ get (label));;

let clone_nodes gr = n_fold gr new_node empty_graph ;;

let gmap gr f = let x gr_accu id1 id2 label = new_arc gr_accu id1 id2 (f label)  in
  e_fold gr x (clone_nodes gr);;

let gmapbis gr f = let x gr_accu id1 id2 label = new_arc gr_accu id2 id1 (f label)  in
  e_fold gr x (clone_nodes gr);;