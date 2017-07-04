open Printf



let file = ref ""
let max_cores = ref max_int
let eois_file = ref ""
let outputs_prefix = ref ""
let number_after_ev_id = ref false
let verbose = ref false
let rule_of_interest = ref ""

let options = [
  ("--max-cores", Arg.Set_int max_cores,
   "maximal number of cores to generate");
  ("--eois", Arg.Set_string eois_file, 
   "file containing custom events of interest");
  ("-o", Arg.Set_string outputs_prefix,
   "name prefix for the output files");
  ("-r", Arg.Set_string rule_of_interest,
   "rule of interest");
  ("--eoi-id-in-filename", Arg.Set number_after_ev_id, 
   "include the id of the corresponding event of interest in every causal core filename");
  ("--verbose", Arg.Set verbose,
   "print annotated dot files")
]

let description =
  Sys.argv.(0) ^
  " trace\n computes the causal core of 'trace' " ^
  "for its first event of interest."


let output_file_name cc_id ev_id = 
  let n = if !number_after_ev_id then ev_id else cc_id in
  Format.sprintf "%s%d.dot" !outputs_prefix n


let read_event_ids file = 
  let ic = open_in file in
  let eois = Queue.create () in
  begin try
    while true do
      let line = input_line ic in
      try
        Queue.push (int_of_string line) eois
      with _ -> ()
    done
  with End_of_file -> close_in ic end ;
  Causal_core_util.list_of_queue eois



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
    let steps_array = Array.of_list steps in
    let () = log "Computing the grid." in
    let rule = if !rule_of_interest <> "" then Some !rule_of_interest else None in
    let grid, eois = Grid.build_grid ~rule env steps in


    (* In case some custom eois are specified, take these instead of
       the ones generated by KaSim. *)
    let eois = 
      if !eois_file <> "" then read_event_ids !eois_file else eois in 

    let eois = Causal_core_util.list_take !max_cores eois in
    let n_eois = List.length eois in

    if n_eois = 0 then
      log "No event of interest detected."
    else
      begin
      printf "Detected %d events of interest.\n" n_eois ; flush stdout ;
      let counter = ref 0 in

      let process_core eoi core =
        incr counter ;
        printf "Computing core %d/%d.\r" !counter n_eois ;
        flush stdout ;
        let prec = Precedence.compute_precedence grid (Causal_core.core_events core) in
        let prec = Precedence.transitive_reduction prec in
        
        let oc = open_out (output_file_name !counter eoi) in
        let options = 
            if !verbose then Story_printer.def_options_detailed
            else Story_printer.def_options_simple in
        let fmt = Format.formatter_of_out_channel oc in
        Story_printer.print ~options env 
          steps_array grid fmt (core, prec) ;
        close_out oc

      in Causal_core.iter_causal_cores env grid eois process_core

      end


let () = main ()