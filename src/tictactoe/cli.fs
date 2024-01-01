namespace tictactoe
open Game

module private Display =
    let private cell_to_char = function
        | Cell(Some Cross) -> "X"
        | Cell(Some Circle) -> "O"
        | _ -> "_"

    let board_to_string (gameboard: Gameboard) = 
        let tl = cell_to_char gameboard.top_lft;
        let tm = cell_to_char gameboard.top_mid;
        let tr = cell_to_char gameboard.top_rgt;
        let ml = cell_to_char gameboard.mid_lft;
        let mm = cell_to_char gameboard.mid_mid;
        let mr = cell_to_char gameboard.mid_rgt;
        let bl = cell_to_char gameboard.bot_lft;
        let bm = cell_to_char gameboard.bot_mid;
        let br = cell_to_char gameboard.bot_rgt;
        $"{tl} {tm} {tr}\n{ml} {mm} {mr}\n{bl} {bm} {br}"

module Cli =
    let private get_random_move (gameboard: Gameboard) =
        let try_move (a, b) =
            match a with
            |Cell None -> Some b
            |_ -> None

        let potential_moves = [|
            gameboard.top_lft, TopLft
            gameboard.top_mid, TopMid
            gameboard.top_rgt, TopRgt
            gameboard.mid_lft, MidLft
            gameboard.mid_mid, MidMid
            gameboard.mid_rgt, MidRgt
            gameboard.bot_lft, BotLft
            gameboard.bot_mid, BotMid
            gameboard.bot_rgt, BotRgt
        |]

        potential_moves |> Array.tryPick try_move

    let rec private get_valid_input_from_user f =
        match f() with
        |Some res -> res
        |None ->
            printfn "You did not enter a valid response. Try again."
            get_valid_input_from_user f

    let private get_yes_or_no_from_user msg () =
        printfn "%s" msg
        match System.Console.ReadLine().ToLowerInvariant() with
        |"yes"|"y" -> Some true
        |"no"|"n" -> Some false
        |_ -> None

    let private get_first_player_from_user() =
        get_yes_or_no_from_user "Do you want to play first?"
        |> get_valid_input_from_user

    let start_game() =
        let player_turn =
            if get_first_player_from_user() then
                Cross
            else
                Circle

        {gameboard = empty_game_board; player_turn = player_turn} |> Playing

    let private string_to_player_move (s:string) =
        match s.ToLowerInvariant() with
        |"q" -> Some TopLft
        |"w" -> Some TopMid
        |"e" -> Some TopRgt
        |"a" -> Some MidLft
        |"s" -> Some MidMid
        |"d" -> Some MidRgt
        |"z" -> Some BotLft
        |"x" -> Some BotMid
        |"c" -> Some BotRgt
        |_   -> None

    let private try_get_user_move gameboard () =
        System.Console.Clear()
        gameboard |> Display.board_to_string |> printfn "%s"

        printfn "\nSelect move from qweasdzxc"
        System.Console.ReadLine() |> string_to_player_move

    let private get_user_move gameboard =
        try_get_user_move gameboard |> get_valid_input_from_user

    let ask_user_for_new_game () =
        get_yes_or_no_from_user "Do you want to play again?"
        |> get_valid_input_from_user

    let end_game = function
        | Tied -> printfn "No winners - game is tied!"
        | Won Cross -> printfn "User (you) have won!"
        | Won Circle -> printfn "AI has won!"

    let controller = {
        cross_mover = get_user_move >> Some
        circle_mover = get_random_move
        retry_allowed = true
    }