open Graph
open Printf

type path = id list;;

let rec traiter_out_arcs gr out_arcs forbidden id2 = match out_arcs with
  | [] -> None
  | (x,label)::rest when List.mem x forbidden || label=0 ->   (* si le noeud est dans la liste forbidden ou qu'il est sature *)
    traiter_out_arcs gr rest forbidden id2
  | (x,label)::rest-> if x = id2 then Some [x] else
      match find_path_core gr forbidden x id2 with
      | None -> traiter_out_arcs gr rest forbidden id2
      | Some p -> Some (x::p)

and find_path_core gr forbidden id1 id2 =
  match traiter_out_arcs gr (out_arcs gr id1) forbidden id2 with
  | None -> None
  | Some [] -> assert false
  | Some path ->Some (path);;


let find_path gr id1 id2 = match (find_path_core gr [] id1 id2) with
  | Some path -> id1::path
  | None -> [];;
