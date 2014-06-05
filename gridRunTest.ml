open GridRun
open Printf

let rec readLoop () =
  let () = printf "Enter your command:\n> " in
  match lineToList (readInput ()) with
  | [] ->
    printf "Error: your line is empty or contains nothing but whitespaces." ;
    readLoop ()
  | (head :: tail) as l ->
    let myPrint command list =
      printf "Command: %7s ( " (toString command) ;
      List.iter (function s -> printf "%s, " (String.escaped s)) list ;
      printf "\027[2D )\n\n" ;
    in
    let command = keyMap head in
    myPrint command l ;
    if command = Quit then () else readLoop ()

let () =
  printf "\nTesting gridRun.\n\n" ;
  readLoop()
