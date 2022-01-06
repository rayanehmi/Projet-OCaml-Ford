open Gfile
open Tools
open Graph
open Findpath
open Fordfulk
open Printf
open Option



let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0      ()
    end ;


  (* Arguments are : infile(1) source-id(2) sink-id(3) outfile(4) *)

  let infile = Sys.argv.(1)
  and outfile = Sys.argv.(4)

  (* These command-line arguments are not used for the moment. *)
  and source = int_of_string Sys.argv.(2)
  and sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)


  let graph = from_file infile in

  let graph = gmap graph (fun x -> int_of_string x) in
  (*let updatedGraph = fordfulk graph 0 5 in*)

  (** FIRST STEP ALGORITHM *)
  (*let graphUpdate = addFlowToPath graph (find_path graph 0 5) (get (minLabel graph (find_path graph 0 5) (Some 10000)))  in
    let graphUpdateReturn = addFlowToPathReturn graphUpdate(find_path graph 0 5) (get (minLabel graph (find_path graph 0 5) (Some 10000))) in
    let labelPath = getLabelPath graphUpdateReturn (find_path graph 0 5) [] in
    let () = List.iter (printf "Noeuds: %d\n") (find_path graph 0 5) in 
    let () = List.iter (printf "Label: %d\n") labelPath in 

    (** SECOND STEP ALGORITHM *)
    let path = find_path graphUpdateReturn 0 5 in
    let () = List.iter (printf "Noeuds2: %d\n") (find_path graphUpdateReturn 0 5) in 
    let () = List.iter (printf "Label2: %d\n") path in
    let secondGraph = addFlowToPath graphUpdateReturn (find_path graphUpdateReturn 0 5) (get (minLabel graphUpdateReturn (find_path graphUpdateReturn 0 5) (Some 10000))) in
    let secondGraphReturn = addFlowToPathReturn secondGraph(find_path graphUpdateReturn 0 5) (get (minLabel graphUpdateReturn (find_path graphUpdateReturn 0 5) (Some 10000))) in
    let secondlabelPath = getLabelPath secondGraphReturn (find_path secondGraphReturn 0 5) [] in
    let graphUpdateReturnStringMapped = gmap secondGraphReturn (fun x -> string_of_int x) in*)

  let updateGraph = fordfulk graph 0 5 in
  let updateGraphStringMapped = gmap updateGraph (fun x -> string_of_int x) in
  let () = export outfile updateGraphStringMapped in
  ()

