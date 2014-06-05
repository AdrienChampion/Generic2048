open Value
open Grid
open Printf

exception CommandIsNotASwipe

(** Reads a line on the standard input. *)
let readInput () = read_line ()

(** Type for the commands of the game. *)
type commands =
(** Game commands. *)
| Up | Down | Right | Left
(** Program commands. *)
| Save | Load | Reset | New | Help | Quit
(** Unknown. *)
| Unknown

(** Four swipe directions, NOT the same as their command counterparts. *)
type swipe =
| SUp | SDown | SRight | SLeft

(** Command to string. *)
let toString = function
  | Up -> "[Up]"
  | Down -> "[Down]"
  | Right -> "[Right]"
  | Left -> "[Left]"
  | Save -> "[Save]"
  | Load -> "[Load]"
  | Reset -> "[Reset]"
  | New -> "[New]"
  | Help -> "[Help]"
  | Quit -> "[Quit]"
  | Unknown -> "[Unknown]"

(** Converts a line into a list of tokens (strings). *)
let lineToList s =
  let rec loop res s' =
    try
      let open String in
      let spaceIndex = index s' ' ' in
      let token = sub s' 0 spaceIndex in
      let restLength = (length s') - spaceIndex in
      let rest = trim (sub s' spaceIndex restLength) in
      loop (token :: res) rest
    with Not_found -> List.rev (s' :: res)
  in
  loop [] (String.trim s)

(** A string to append at the end of the description of unimplemented
    commands. *)
let notImplemented = "(\027[31;1mnot implemented yet\027[0m)."

(** List associating the legal commands with the corresponding label
    of the 'command' type and their description. *)
let keyList = [
  (["\027[A"; "u" ; "U" ;"up"; "Up"],
   Up,
   String.escaped
     "Slides the tiles upward.       \027[A is up arrow.");
  (["\027[B"; "d" ; "D" ;"down"; "Down"],
   Down,
   String.escaped
     "Slides the tiles downward.     \027[B is down arrow.");
  (["\027[C"; "r" ; "R" ;"right"; "Right"],
   Right,
   String.escaped
     "Slides the tiles to the right. \027[C is right arrow.");
  (["\027[D"; "l" ; "L" ;"left"; "Left"],
   Left,
   String.escaped
     "Slides the tiles to the left.  \027[D is left arrow.");
  (["save"; "Save"],
   Save,
   "Saves current game to the specified file " ^ notImplemented);
  (["load"; "Load"],
   Load,
   "Loads a game from the specified file " ^ notImplemented);
  (["r"; "R"; "reset"; "Reset"],
   Reset,
   "Resets the game to its starting configuration " ^ notImplemented);
  (["n"; "N"; "new"; "New"],
   New,
   "Starts a new game " ^ notImplemented);
  (["h"; "H"; "help"; "Help"],
   Help,
   "Prints this help message.");
  (["q"; "Q"; "quit"; "Quit"],
   Quit,
   "Exits the game.")
]

(** Maps a (string) command to the corresponding label of the
    'command' type. *)
let keyMap string =
  let rec loop = function
    | (strings,command,_) :: tail ->
      if List.mem string strings then command else loop tail
    | [] -> Unknown
  in
  loop keyList

(** Copyright and license information. *)
let disclaimer = [
  "";
  "\027[1;4mCopyright 2014 Adrien Champion.\027[0m";
  "This software is released under the \027[1;4mLGPL v3 license\027[0m.";
  ""
]

(** Text before the command explanation. *)
let helpPrefix = disclaimer @ [
  "|=================| How to play this game |=================|";
  " If you don't know the rules of the game please google \"2048\".";
  "";
  " The users shall give their commands through the standard";
  " input. To send a command simply type it and press enter.";
  "";
  " Legal commands:";
  ""
]

(** The lines of the help message, includes the commands and their
    explanation. *)
let helpList = helpPrefix @ (
  let innerFoldFun string command =
    string ^ "\027[1m" ^ (String.escaped command) ^ "\027[0m, "
  in
  let foldFun stringListRev triplet =
    let (commandList,_,description) = triplet in
    ("    " ^ description)
    :: (List.fold_left innerFoldFun "  " commandList)
    :: stringListRev
  in
  List.rev ("" :: (List.fold_left foldFun [] keyList))
)

(** Prints the help message. *)
let printHelp () = List.iter (function s -> printf "%s\n" s) helpList

(** Prints a list of strings as an error. *)
let printError stringList =
  printf "\027[31;1mError:\027[0m\n" ;
  List.iter (printf "  %s\n") stringList
  
(** Prints the input string and exits with the specified exit code. *)
let quit s i = printf "\n\027[1mExiting\027[0m %s.\n\n" s ; exit i

(** Displays a prompt and reads a line. Calles ifNothing if the line
    is empty modulo whitespaces and work otherwise. *)
let prompt ifNothing work =
  printf "\n> " ; match lineToList (readInput ()) with
  | [] -> ifNothing ()
  | head :: tail ->
    let command = keyMap head in
    let () = printf
      "\027[A\027[K> %s%s\n"
      (toString command)
      (List.fold_left
	 (fun string arg -> string ^ " " ^ (String.escaped arg)) "" tail
      )
    in
    work command tail

(** Attempts to load a game, calls ifFailed if game loading fails. *)
let loadGame args ifFailed =
  printError ["not implemented"] ; ifFailed ()

(** Attemps to save a game, calls ifFailed if game saving fails. *)
let saveGame args ifFailed =
  printError ["not implemented"] ; ifFailed ()

(** Swipes a grid. *)
let swipe grid ifSame ifDifferent direction =
  let newGrid = (
    function
    | SUp -> propagateUp grid
    | SDown -> propagateDown grid
    | SRight -> propagateRight grid
    | SLeft -> propagateLeft grid
  ) direction
  in
  if grid = newGrid then ifSame () else ifDifferent (insertRandom newGrid)

(** Creates a new grid and starts the game. *)
let newGame run =
  printf "\n\nStarting a new game.\n\n" ;
  run randomStartGrid randomStartGrid

(** Starts a game from the input grid. *)
let rec runGame startingGrid currentGrid =
  let fallbackFun () = printf "\n" ; runGame startingGrid currentGrid in
  let ifSame () = printf "This swipe did nothing.\n"  in
  let rec work () =
    let ifSame () = ifSame () ; work () in
    let ifDifferent = runGame startingGrid in
    prompt (fallbackFun) (
      fun command args -> match command with
      | Save -> saveGame args fallbackFun
      | Load -> loadGame args fallbackFun
      | Reset ->
	printf "Restarting game.\n\n" ; runGame startingGrid startingGrid
      | New -> newGame runGame
      | Help -> printHelp () ; fallbackFun ()
      | Quit -> quit "on user request" 0
      | Unknown -> printError ["unknown command."] ; work ()
      | Up -> swipe currentGrid ifSame ifDifferent SUp
      | Down -> swipe currentGrid ifSame ifDifferent SDown
      | Right -> swipe currentGrid ifSame ifDifferent SRight
      | Left -> swipe currentGrid ifSame ifDifferent SLeft
    )
  in
  printGrid currentGrid ; printf "\n" ; work ()

(** Software top loop. *)
let rec topLoop () =
  prompt topLoop (fun command args ->
    match command with
    | Save -> printError ["no game running, nothing to save."] ; topLoop ()
    | Load -> loadGame args topLoop
    | Reset -> printError ["no game running, nothing to reset."] ; topLoop ()
    | New -> newGame runGame
    | Help -> printHelp () ; topLoop()
    | Quit -> quit "on user request" 0
    | Unknown -> printError ["unknown command."] ; topLoop ()
    | Up | Down | Right | Left ->
      printError ["no game running, nothing to swipe."] ; topLoop ()
  )

(** Launches the game. *)
let run () =
  printf "\n" ; List.iter (printf "%s\n") disclaimer ; topLoop ()

(** Launching the software. *)
let () = run ()
