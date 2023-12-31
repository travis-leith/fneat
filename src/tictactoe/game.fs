namespace tictactoe

module Game =
    type Player =
        | Cross
        | Circle

    type Cell = 
        | Cell of Player option
        member this.IsNone = 
            match this with
            |Cell player -> player.IsNone
        member this.IsSome = 
            match this with
            |Cell player -> player.IsSome

    type Gameboard = {
        top_lft: Cell
        top_mid: Cell
        top_rgt: Cell
        mid_lft: Cell
        mid_mid: Cell
        mid_rgt: Cell
        bot_lft: Cell
        bot_mid: Cell
        bot_rgt: Cell
    }

    type PlayerMove =
        | TopLft
        | TopMid
        | TopRgt
        | MidLft
        | MidMid
        | MidRgt
        | BotLft
        | BotMid
        | BotRgt

    type PlayingGameState = {
        gameboard: Gameboard
        plater_turn: Player
    }

    type GameOverState =
        | Tied
        | Won of Player

    type GameState =
        | Starting
        | Playing of PlayingGameState
        | GameOver of GameOverState

    let empty_game_board = {
        top_lft = Cell(None)
        top_mid = Cell(None)
        top_rgt = Cell(None)
        mid_lft = Cell(None)
        mid_mid = Cell(None)
        mid_rgt = Cell(None)
        bot_lft = Cell(None)
        bot_mid = Cell(None)
        bot_rgt = Cell(None)
    }

    let apply_move (gameboard : Gameboard) player_move player =
        
        let player_cell() = player |> Some |> Cell
        match player_move with
        | TopLft when gameboard.top_lft.IsNone -> {gameboard with top_lft = player_cell()} |> Some
        | TopMid when gameboard.top_mid.IsNone -> {gameboard with top_mid = player_cell()} |> Some
        | TopRgt when gameboard.top_rgt.IsNone -> {gameboard with top_rgt = player_cell()} |> Some
        | MidLft when gameboard.mid_lft.IsNone -> {gameboard with mid_lft = player_cell()} |> Some
        | MidMid when gameboard.mid_mid.IsNone -> {gameboard with mid_mid = player_cell()} |> Some
        | MidRgt when gameboard.mid_rgt.IsNone -> {gameboard with mid_rgt = player_cell()} |> Some
        | BotLft when gameboard.bot_lft.IsNone -> {gameboard with bot_lft = player_cell()} |> Some
        | BotMid when gameboard.bot_mid.IsNone -> {gameboard with bot_mid = player_cell()} |> Some
        | BotRgt when gameboard.bot_rgt.IsNone -> {gameboard with bot_rgt = player_cell()} |> Some
        | _ -> None

    let gameboard_is_full (gameboard : Gameboard) =
        gameboard.top_lft.IsSome &&
        gameboard.top_mid.IsSome &&
        gameboard.top_rgt.IsSome &&
        gameboard.mid_lft.IsSome &&
        gameboard.mid_mid.IsSome &&
        gameboard.mid_rgt.IsSome &&
        gameboard.bot_lft.IsSome &&
        gameboard.bot_mid.IsSome &&
        gameboard.bot_rgt.IsSome

    let game_winner (gameboard : Gameboard) =
        let win_line = function
            | (Cell (Some p1), Cell(Some p2), Cell(Some p3)) when p1 = p2 && p1 = p3 -> Some p1
            | _ -> None

        [|
            //horizontal win lines
            gameboard.top_lft, gameboard.top_mid, gameboard.top_rgt
            gameboard.mid_lft, gameboard.mid_mid, gameboard.mid_rgt
            gameboard.bot_lft, gameboard.bot_mid, gameboard.bot_rgt
            //vertical win lines
            gameboard.top_lft, gameboard.mid_lft, gameboard.bot_lft
            gameboard.top_mid, gameboard.mid_mid, gameboard.bot_mid
            gameboard.top_rgt, gameboard.mid_rgt, gameboard.bot_rgt
            //diagonal win lines
            gameboard.top_lft, gameboard.mid_mid, gameboard.bot_rgt
            gameboard.bot_lft, gameboard.mid_mid, gameboard.top_rgt
        |] |> Array.tryPick win_line

    let check_game_over (gameboard : Gameboard) =
        match game_winner gameboard with
        | Some player -> player |> Won |> Some
        | None when gameboard_is_full gameboard -> Tied |> Some
        | None -> None