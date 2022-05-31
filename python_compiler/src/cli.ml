open !Cmdliner

let file =
  let env =
    let doc = "Compiles and executes a python file" in
    Cmd.Env.info "PATH" ~doc
  in
  let doc = "The python file to run" in
  Arg.(required & pos ~rev:true 0 (some string) None & info [] ~doc)


(* Takes in function to run*)
let penne_cli ~f = 
  let penne_t = Term.(const f $ file) in
  let cmd = Cmd.v (Cmd.info "penne") penne_t in
  exit (Cmd.eval cmd)