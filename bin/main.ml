let answer () =
  Random.self_init ();
  Random.int 100

let input_guess () =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; ANSITerminal.green ]
    "Enter guess: ";
  read_int_opt ()

let print_error =
  ANSITerminal.print_string [ ANSITerminal.red; ANSITerminal.Bold ]

let print_success =
  ANSITerminal.print_string [ ANSITerminal.blue; ANSITerminal.Bold ]

let rec game answer tries =
  match input_guess () with
  | None ->
      print_error "Invalid input\n";
      game answer tries
  | Some guess -> (
      match guess > 100 || guess < 0 with
      | true ->
          print_error "Input must be between 0 and 100\n";
          game answer tries
      | false -> (
          match compare guess answer with
          | 0 ->
              print_success
                ("Correct! You got it in "
                ^ string_of_int (tries + 1)
                ^ " tries! \n")
          | 1 ->
              print_error "Try a smaller number\n";
              game answer (tries + 1)
          | -1 ->
              print_error "Try a larger number\n";
              game answer (tries + 1)
          | _ -> ()))

let () = try game (answer ()) 0 with
  | _ -> print_error "\nSomething went wrong!\n"

