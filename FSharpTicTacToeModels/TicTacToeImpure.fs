namespace QUT

    module FSharpImpureTicTacToeModel =
    
        type Player = Nought | Cross

        type GameState = 
            { mutable currentTurn: Player; boardSize: int; board: string [,] } 
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.currentTurn
                member this.Size with get()    = this.boardSize
                member this.getPiece(row, col) = this.board.[row, col]

        type Move = 
            { row: int; col: int }
            interface ITicTacToeMove with
                member this.Row with get() = this.row
                member this.Col with get() = this.col



        let GameOutcome game = 
            Draw

        let ApplyMove (game:GameState) move = 
            let token = if game.currentTurn = Nought then "O" else "X"
            game.board.[move.row, move.col] <- token
            game.currentTurn <- if game.currentTurn = Nought then Cross else Nought
            game

        let CreateMove row col = 
            let move = { row = row; col = col }
            move

        let FindBestMove game =
            //
            raise (System.NotImplementedException("FindBestMove"))

        let GameStart first size = 
            let createBoard = array2D [| for x in 1..size -> [|for y in 1..size -> ""|]|]
            let gamestate = {currentTurn = first; boardSize = size; board = createBoard}
            gamestate

        // plus other helper functions ...




        type WithAlphaBetaPruning() =
            override this.ToString()         = "Impure F# with Alpha Beta Pruning";
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross
                member this.Nought with get()            = Nought
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game 
                member this.ApplyMove(game, move)        = ApplyMove game  move
                member this.FindBestMove(game)           = FindBestMove game