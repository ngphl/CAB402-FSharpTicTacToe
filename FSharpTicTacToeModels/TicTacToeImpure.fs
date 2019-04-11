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
            let mutable result = Draw
            let size = game.boardSize
            let getRow i (arr:string [,]) = arr.[i..i, *] |> Seq.cast<string>
            let getCol i (arr:string [,]) = arr.[*, i..i] |> Seq.cast<string>
            let getLDiag (arr:string [,]) = seq { for i in 0..size-1 do yield arr.[i,i]}
            let getRDiag (arr:string [,]) = seq { for i in 0..size-1 do yield arr.[size-1-i,i]}

            let checkLine line coordGenerator = 
                if result = Undecided || result = Draw then
                    if line |> Seq.contains ""
                    then 
                        result <- Undecided
                    elif Seq.forall (fun x -> x = "X") line then result <- Win(Cross, coordGenerator)
                    elif Seq.forall (fun x -> x = "O") line then result <- Win(Nought, coordGenerator)
                    
            let board = game.board
            for i in 0..size-1 do
                let row = getRow i board
                let col = getCol i board
                let ldiag = getLDiag board
                let rdiag = getRDiag board
                let genRowLine = seq { for y in 0..size-1 -> (i,y)}
                let genColLine = seq { for x in 0..size-1 -> (x, i)}
                let genLDiagLine = seq { for i in 0..size-1 -> (i,i)}
                let genRDiagLine = seq { for i in 0..size-1 -> (size-1-i,i)}

                checkLine row genRowLine
                checkLine col genColLine
                checkLine rdiag genRDiagLine
                checkLine ldiag genLDiagLine

            result

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