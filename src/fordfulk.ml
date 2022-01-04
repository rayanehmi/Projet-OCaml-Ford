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


let rec add_path gr path n = printf "test\n" ;match path with
  |x1::[] -> gr
  |[] -> gr
  |x1::x2::rest -> add_arc gr x1 x2 n;;



let rec fordfulkcore gr graphEcart id1 id2 = 
  printf "ffcore";
  match find_path gr id1 id2 with
  |[] -> printf "test du []"; gr;
  |path -> printf "test3";  let valeur = minLabel gr path (Some (10000000)) in fordfulkcore (add_path gr path (get valeur) ) (add_path gr path (-(get valeur)) ) id1 id2;;

let fordfulk gr id1 id2 = 
  printf "DEBUT FORDFULK\n";
  let graphEcart = graph_ecart gr in 
  fordfulkcore gr graphEcart id1 id2;;