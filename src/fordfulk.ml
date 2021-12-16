open Graph
open Printf
open Findpath
open Tools

let graph_ecart gr = gmapbis gr (fun x -> 0);;

let rec minLabel gr path accu = match path with 
  |x1::[] -> accu
  |[] -> accu
  |x1::x2::rest -> match (find_arc gr x1 x2)<accu with
    |true -> minLabel gr path (find_arc gr x1 x2)
    |false -> minLabel gr path accu;;


let rec add_path gr path n = match path with
  |x1::[] -> gr
  |[] -> gr
  |x1::x2::rest -> add_arc gr x1 x2 n ; add_arc gr x2 x1 (-n);;


let rec fordfulk gr id1 id2 = 
  let graphInitial = gr in
  let graphEcart = graph_ecart gr in
  match find_path gr id1 id2 with
  |None -> 
  |Some path -> add_arc graph_ecart 