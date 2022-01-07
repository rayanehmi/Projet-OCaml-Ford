open Graph
open Printf
open Findpath
open Tools
open Option
open Printf

let graph_ecart gr = gmapbis gr (fun x -> 0);;

(* ########### FONCTIONS DE DEBUG  ########### *)
(* Permet de retourner la liste des labels d'un chemin donné *)
let rec getLabelPath gr path accu = match path with
  |[] -> List.rev accu
  |x1::[] -> List.rev accu
  |x1::x2::rest -> 
    match (find_arc gr x1 x2) with
    |None -> accu
    |arc -> getLabelPath gr (x2::rest) ((get arc)::accu)
(* ########### FIN FONCTIONS DE DEBUG  ########### *)

(* 
 *  Permet de retrouner la valeur minimal des labels d'un chemin donné
 *  @param : gr -> 'a Graph.graph
 *  @param : path -> Graph.id list
 *  @param : accu -> 'a option
 *  @return : 'a option
 *)
let rec minLabel gr path accu = match path with 
  |x1::[] -> accu
  |[] -> accu
  |x1::x2::rest -> match (find_arc gr x1 x2)<accu with
    |true -> minLabel gr (x2::rest) (find_arc gr x1 x2)
    |false -> minLabel gr (x2::rest) accu;;

(* 
 *  Permet de rajouter un flow à tout les labels d'un chemin donné dans le sens direct seulement : id1 -> id2 
 *  @param : gr -> int Graph.graph
 *  @param : path -> Graph.id list
 *  @param : flow -> int
 *  @return : int Graph.graph
 *)
let rec addFlowToPath gr path flow = match path with
  |[] -> gr
  |x1::[] -> gr
  |x1::x2::rest -> 
    match (find_arc gr x1 x2) with
    |None -> assert false
    |label_arc when (get label_arc) >= 0 -> addFlowToPath (add_arc gr x1 x2 (-flow)) (x2::rest) flow
    |label_arc -> assert false

(* 
 *  Permet de rajouter un flow à tout les labels d'un chemin donné dans le sens indirect seulement : id2 -> id1 
 *  @param : gr -> int Graph.graph
 *  @param : path -> Graph.id list
 *  @param : flow -> int
 *  @return : int Graph.graph
 *)
let rec addFlowToPathReturn gr path flow = match path with
  |[] -> gr
  |x1::[] -> gr
  |x1::x2::rest ->
    match (find_arc gr x2 x1) with
    |None -> addFlowToPathReturn (add_arc gr x2 x1 flow) (x2::rest) flow
    |label_arc when (get label_arc) >= 0 -> addFlowToPathReturn (add_arc gr x2 x1 (flow)) (x2::rest) flow
    |label_arc -> assert false


(* 
 *  Applique l'algorithme de fordfulkerson sur un graph entre deux noeuds donnés
 *  @param : gr -> int Graph.graph
 *  @param : source -> Graph.id
 *  @param : dest -> Graph.id
 *  @return : int Graph.graph
 *)
let rec fordfulkCore gr source dest = 
  let pathAccessible = find_path gr source dest in
  match pathAccessible with
  |[] -> gr
  |path -> 
    let flow = minLabel gr path (Some 10000) in
    let finalGraph = fordfulkCore (addFlowToPathReturn (addFlowToPath gr path (get flow)) path (get flow)) source dest in fordfulkCore (finalGraph) source dest

let fordfulk gr source dest = fordfulkCore gr source dest;;

