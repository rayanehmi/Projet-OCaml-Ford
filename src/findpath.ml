open Graph
open Printf

type path = id list;;

(* 
 *  Permet d'itérer sur la liste des arcs sortants d'un noeud sachant la liste des noeuds à ne pas aller et un label positif 
 *)
let rec traiter_out_arcs gr out_arcs forbidden id2 = match out_arcs with
  | [] -> None
  (* Si l'arc a un label de 0 alors il ne faut pas passer par cet arc *)
  | (x,0)::rest -> traiter_out_arcs gr rest forbidden id2
  (* x correspond au noeud qui va de l'origine vers le noeud x par l'arc sortant *)
  | (x,label)::rest ->
    (* Si le noeud x est dans la liste forbidden *)
    if List.mem x forbidden then  
      (* Alors on ne prend pas ce noeud en compte et on refait la fonction sur les autres arcs sortant *)
      traiter_out_arcs gr rest forbidden id2
    else 
      (* Si le noeud x est le noeud final alors c'est fini *)
    if x = id2 then Some [x] 
      (* Sinon on recherche un chemin mais en ajoutant x à la liste forbidden car on prend x comme noeud suivant dans le chemin *)
    else
      match find_path_core gr (x::forbidden) x id2  with 
      | None -> traiter_out_arcs gr rest forbidden id2 
      | Some p -> Some (x::p)

and find_path_core gr forbidden id1 id2 =
  match traiter_out_arcs gr (out_arcs gr id1) forbidden id2 with
  | None -> None
  | Some [] -> assert false
  | Some path ->Some (path);;

(* 
 *  Algorithme trouvant un chemin valide entre deux noeuds donnés
 *  @param : gr -> int graph
 *  @param : id1 -> id
 *  @param : id2 -> id
 *  @return : path
 *)
let find_path gr id1 id2 = match (find_path_core gr [] id1 id2) with
  | Some path -> id1::path
  | None -> [];;
