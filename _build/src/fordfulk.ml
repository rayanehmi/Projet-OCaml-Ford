open Graph
open Printf
open Findpath
open Tools
open Option
open Printf

let graph_ecart gr = gmapbis gr (fun x -> 0);;

let rec minLabel gr path accu = match path with 
  |x1::[] -> accu
  |[] -> accu
  |x1::x2::rest -> match (find_arc gr x1 x2)<accu with
    |true -> minLabel gr (x2::rest) (find_arc gr x1 x2)
    |false -> minLabel gr (x2::rest) accu;;


(*on cherche un path -> 0 2 4 5
  on cherche les arcs pour chaque noeuds : find_arc 0 2
  Si deja un arc prsent :
    -> add_arc 0 2 -minLabel
  find_arc 2 0
  Si il y a deja un arc :
  -> add_arc 2 0 minLabel
  Sinon:
  -> add_arc 2 0 minLabel
  -> on repete l'algo sur 2 4
  -> on repete sur 4 5
  -> on repete jusqua ce que la liste des noeuds du path soit vide
  -> on retourne le graph*)

let rec getLabelPath gr path accu = match path with
  |[] -> List.rev accu
  |x1::[] -> List.rev accu
  |x1::x2::rest -> 
    match (find_arc gr x1 x2) with
    |None -> accu
    |arc -> getLabelPath gr (x2::rest) ((get arc)::accu)

let rec addFlowToPath gr path flow = match path with
  |[] -> printf "fin";gr
  |x1::[] -> printf "On termine dans le noeud: %d\n" x1;gr
  |x1::x2::rest -> printf "On regarde arc entre %d et %d\n" x1 x2;

    match (find_arc gr x1 x2) with
    |None -> assert false
    |label_arc when (get label_arc) >= 0 -> printf "On ajoute %d%! entre %d%! et %d%!\n" (-flow) x1 x2;addFlowToPath (add_arc gr x1 x2 (-flow)) (x2::rest) flow
    |label_arc -> Printf.printf "Assert false addFlowToPath"; assert false
let rec addFlowToPathReturn gr path flow = match path with
  |[] -> printf "fin";gr
  |x1::[] -> printf "On termine dans le noeud: %d\n" x1;gr
  |x1::x2::rest -> printf "On regarde arc entre %d et %d\n" x1 x2;
    match (find_arc gr x2 x1) with
    |None -> printf "On ajoute %d%! car abscence de label\n" (flow);addFlowToPathReturn (add_arc gr x2 x1 flow) (x2::rest) flow
    |label_arc when (get label_arc) >= 0 -> printf "On ajoute %d%! entre %d%! et %d%!\n" (flow) x1 x2; addFlowToPathReturn (add_arc gr x2 x1 (flow)) (x2::rest) flow
    |label_arc -> Printf.printf "Assert false addFlowToPathReturn ";assert false

let rec fordfulkCore gr source dest = 
  let pathAccessible = find_path gr source dest in
  match pathAccessible with
  |[] -> gr
  |path -> 
    let flow = minLabel gr path (Some 10000) in
    let finalGraph = fordfulkCore (addFlowToPathReturn (addFlowToPath gr path (get flow)) path (get flow)) source dest in fordfulkCore (finalGraph) source dest

let fordfulk gr source dest = fordfulkCore gr source dest;;

