namespace tictactoe
open Game
module Display =
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