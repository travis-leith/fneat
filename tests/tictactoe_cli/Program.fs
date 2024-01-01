open tictactoe

[<TailCall>]
let rec single_game_loop playing_state =
    match Game.play_game Cli.controller playing_state with
    | Ok game_over_state -> game_over_state
    | Error new_playing_state ->
        printfn "You have entered an invalid move. Try again..."
        System.Console.ReadKey() |> ignore
        single_game_loop new_playing_state

[<TailCall>]
let rec game_loop () =
    Cli.start_game()
    |> single_game_loop
    |> Cli.end_game

    if Cli.ask_user_for_new_game() then
            game_loop ()
        else
            ()

game_loop ()