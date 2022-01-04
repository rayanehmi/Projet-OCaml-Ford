open Gfile
open Tools
open Graph
open Findpath
open Fordfulk
open Printf



let () =

  (* Check the number of command-line arguments *)
  if Array.length Sys.argv <> 5 then
    begin
      Printf.printf "\nUsage: %s infile source sink outfile\n\n%!" Sys.argv.(0) ;
      exit 0
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
  printf "test";
  let graph = fordfulk graph 0 5 in

  let graphFinal = gmap graph (fun x -> string_of_int x) in

  (* Rewrite the graph that has been read. *)
  let () = export outfile graphFinal in

  ()

