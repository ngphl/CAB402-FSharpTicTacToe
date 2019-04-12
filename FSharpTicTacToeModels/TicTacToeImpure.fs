namespace QUT

    module FSharpImpureTicTacToeModel =
    
        type Player = Nought | Cross

        type GameState = 
            { currentTurn: Player; boardSize: int; board: string [,] } 
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

            let countByVar (elem:string) S = 
                if Seq.contains elem S then snd (S |> Seq.countBy (fun x -> x = elem) |> Seq.find (fun x -> (fst x) = true))
                else 0

            let checkLine line coordGenerator = 
                if Seq.forall (fun x -> x = "X") line then Win(Cross, coordGenerator)
                elif Seq.forall (fun x -> x = "O") line then Win(Nought, coordGenerator)
                elif (countByVar "X" line) >= 1 && (countByVar "O" line) >= 1 then Draw
                else Undecided
            
            let ldiag = getLDiag game.board
            let rdiag = getRDiag game.board
            let genLDiagLine = seq { for i in 0..size-1 -> (i,i)}
            let genRDiagLine = seq { for i in 0..size-1 -> (size-1-i,i)}
            match result with
            | Draw -> result <- (checkLine ldiag genLDiagLine) //Draw is overwritten by anything
            | Undecided -> if not((checkLine ldiag genLDiagLine) = Draw) then result <- checkLine ldiag genLDiagLine //Undecided cannnot be overwritten by draw
            | Win(_,_) -> (checkLine ldiag genLDiagLine) |> ignore //nothing matter cause a win already exists

            match result with
            | Draw -> result <- (checkLine rdiag genRDiagLine) //Draw is overwritten by anything
            | Undecided -> if not((checkLine rdiag genRDiagLine) = Draw) then result <- checkLine rdiag genRDiagLine //Undecided cannnot be overwritten by draw
            | Win(_,_) -> (checkLine rdiag genRDiagLine) |> ignore //nothing matter cause a win already exists

            let board = game.board
            for i in 0..size-1 do
                let row = getRow i board
                let col = getCol i board
                let genRowLine = seq { for y in 0..size-1 -> (i,y)}
                let genColLine = seq { for x in 0..size-1 -> (x, i)}
                match result with
                | Draw -> result <- (checkLine row genRowLine) //Draw is overwritten by anything
                | Undecided -> if not((checkLine row genRowLine) = Draw) then result <- checkLine row genRowLine //Undecided cannnot be overwritten by draw
                | Win(_,_) -> (checkLine row genRowLine) |> ignore //nothing matter cause a win already exists
                match result with
                | Draw -> result <- (checkLine col genColLine) //Draw is overwritten by anything
                | Undecided -> if not((checkLine col genColLine) = Draw) then result <- checkLine col genColLine //Undecided cannnot be overwritten by draw
                | Win(_,_) -> (checkLine col genColLine) |> ignore //nothing matter cause a win already exists
            result

        let ApplyMove (game:GameState) move = 
            let token = if game.currentTurn = Nought then "O" else "X"
            let board = Array2D.copy game.board //Else mutable state just uses a reference >:(
            board.[move.row, move.col] <- token
            let currTurn = if game.currentTurn = Nought then Cross else Nought
            { currentTurn = currTurn; boardSize = game.boardSize; board=board}

        let CreateMove row col = 
            let move = { row = row; col = col }
            move

        let Lines (size:int) : seq<seq<int*int>> = 
            let GetRows x = seq {for y in 0..size-1 do yield (x,y)}
            let GetCols x = seq {for y in 0..size-1 do yield (y,x)}
            let LDiag = seq { for i in 0..size-1 do yield (i,i)}
            let RDiag = seq { for i in 0..size-1 do yield (size-1-i,i)}
            seq { 
                for x in 0..size-1 do yield GetRows x; 
                for x in 0..size-1 do yield GetCols x; //Rebind these to !yield for readability if time later on
                yield LDiag;
                yield RDiag
                }

        let FindBestMove game =
            let heuristic state perspective = 
                match GameOutcome state with
                | Win(currentPlayer, _) -> if currentPlayer = perspective then 1 else -1
                | _ -> 0
            let getTurn game = game.currentTurn
            let gameOver game = 
                match GameOutcome game with
                | Undecided -> false
                | _ -> true
            let moveGenerator state : seq<Move> =
                let getEmptyTiles line = 
                    let newLine = line |> Seq.filter (fun tile ->
                        let row = fst tile
                        let col = snd tile
                        if state.board.[row, col] = "" then true
                        else false
                    )
                    newLine |> Seq.toList
                let getValidMoves lines = 
                    let validMoves = lines |> Seq.map (fun line -> getEmptyTiles line) 
                                           |> Seq.filter (fun move -> if move = [] then false else true)
                                           |> Seq.reduce List.append
                                           |> Seq.distinct 
                                           |> Seq.map (fun validMove -> CreateMove (fst validMove) (snd validMove))
                    validMoves
                getValidMoves (Lines game.boardSize)

            let generator = GameTheory.MiniMaxWithAlphaBetaPruningGenerator heuristic getTurn gameOver moveGenerator ApplyMove
            let best_move = generator -1 1 game game.currentTurn
            Option.get (fst best_move)

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