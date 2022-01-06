open Graph
open Printf

type path = id list;;

let rec traiter_out_arcs gr out_arcs forbidden id2 = match out_arcs with
  | [] -> None
  | (x,0)::rest -> Printf.printf "Label pour arc et noeud : %d\n" x;traiter_out_arcs gr rest forbidden id2
  | (x,label)::rest -> Printf.printf "Noeud sortant et label : %d %d\n" x label;
    if List.mem x forbidden then   (* si le noeud est dans la liste forbidden ou qu'il est sature *)
      traiter_out_arcs gr rest forbidden id2
    else 
    if x = id2 then Some [x] 
    else
      match find_path_core gr (x::forbidden) x id2  with
      | None -> traiter_out_arcs gr rest forbidden id2 
      | Some p -> Some (x::p)

and find_path_core gr forbidden id1 id2 =
  match traiter_out_arcs gr (out_arcs gr id1) forbidden id2 with
  | None -> None
  | Some [] -> Printf.printf "Assert false find_path_core "; assert false
  | Some path ->Some (path);;


let find_path gr id1 id2 = match (find_path_core gr [] id1 id2) with
  | Some path -> id1::path
  | None -> [];;
