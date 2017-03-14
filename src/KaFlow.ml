open Printf


let file = ref ""
let max_cores = ref max_int

let options = [
  ("--max-cores",
   Arg.Set_int max_cores,
   "maximal number of cores to generate")
]

let description =
  Sys.argv.(0) ^
  " trace\n computes the causal core of 'trace' " ^
  "for its first event of interest."


let log s = print_string s ; print_newline () ; flush stdout

let main () =
  let () =
    Arg.parse
      options
      (fun f -> if !file = "" then file := f else
          let () = Format.eprintf "Deals only with 1 file" in exit 2)
      description in
  if !file = "" then
    prerr_string "Please specify a trace file."
  else
    let () = log "Loading the trace file." in
    let ch = open_in !file in
    let json = Yojson.Basic.from_channel ch in
    let () = close_in ch in
    let env = Model.of_yojson (Yojson.Basic.Util.member "env" json) in
    let steps = Trace.of_yojson (Yojson.Basic.Util.member "trace" json) in
    let () = log "Computing the grid." in
    let grid, eois = Grid.build_grid env steps in
    let eois = Causal_core_util.list_take !max_cores eois in
    let n_eois = List.length eois in

    if n_eois = 0 then
      log "No event of interest detected."
    else
      begin
      printf "Detected %d events of interest.\n" n_eois ; flush stdout ;
      let counter = ref 0 in

      let process_core _eoi core =
        incr counter ;
        printf "Computing core %d/%d.\r" !counter n_eois ;
        flush stdout ;
        let prec = Precedence.compute_precedence steps grid core in
        let prec = Precedence.transitive_reduction prec in
        
        let print_with_options options filename = 
          let oc = open_out (sprintf "%s.%d.dot" filename !counter) in
          let fmt = Format.formatter_of_out_channel oc in
          Story_printer.print ~options env 
            (Array.of_list steps) grid fmt (core, prec) ;
          close_out oc in

        print_with_options Story_printer.def_options_simple "story" ;
        print_with_options Story_printer.def_options_detailed "story.full" ;

        in

      Causal_core.iter_causal_cores env grid eois process_core

      end


let () = main ()