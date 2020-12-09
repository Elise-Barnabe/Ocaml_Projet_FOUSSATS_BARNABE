open Gfile
open Tools
open Ford

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
  and _source = int_of_string Sys.argv.(2)
  and _sink = int_of_string Sys.argv.(3)
  in

  (* Open file *)
  let graph = from_file infile in

  let gr = gmap graph  int_of_string in

  let initGr = init_gr gr in

  (*let ecart = graph_ecart initGr in*)

  let final = ford initGr _source _sink in

  let graph_final = match final with
    |(x,y) -> x in

  let debit = match final with 
    |(x,y) -> y in

  let string_gr = gmap graph_final string_of_label in


  (* Rewrite the graph that has been read. *)
  (*let () =  Printf.printf "debit : %d\n%!" debit in*)

  let () = export outfile string_gr in

    ()

