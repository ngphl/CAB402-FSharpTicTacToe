namespace QUT

    module FSharpImpureTicTacToeModel =
        open System
    
        type Player = Nought | Cross

        type GameState = 
            { mutable currentTurn: Player; boardSize: int; board: string [,] } 
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.currentTurn //Mutable
                member this.Size with get()    = this.boardSize
                member this.getPiece(row, col) = this.board.[row, col] //Mutable

        type Move = 
            { row: int; col: int }
            interface ITicTacToeMove with
                member this.Row with get() = this.row
                member this.Col with get() = this.col

        let Lines (size:int) : seq<seq<int*int>> = 
            let GetRows x = seq {for y in 0..size-1 do yield (x,y)}
            let GetCols x = seq {for y in 0..size-1 do yield (y,x)}
            let LDiag = seq { for i in 0..size-1 do yield (i,i)}
            let RDiag = seq { for i in 0..size-1 do yield (size-1-i,i)}
            seq { 
                for x in 0..size-1 do yield GetRows x; 
                for x in 0..size-1 do yield GetCols x;
                yield LDiag;
                yield RDiag
                }

        let CreateMove row col = 
            let move = { row = row; col = col }
            move

        let ApplyMove game move = 
            (*Altered to permit mutable state in current turn and board*)
            let token = if game.currentTurn = Nought then "O" else "X"
            game.board.[move.row, move.col] <- token
            game.currentTurn <- if game.currentTurn = Nought then Cross else Nought
            game (*Don't know if possible to overwrite the abstract func without suffering. Simply use this to satisfy constraint*)
            
        let ReverseMove game move : unit = 
            (*Undo what apply move does so as not to negatively affect layer*)
            game.board.[move.row, move.col] <- ""
            game.currentTurn <- if game.currentTurn = Nought then Cross else Nought

        let GameOutcome game = 
            let CheckLine (game:GameState) (line:seq<int*int>) : TicTacToeOutcome<Player> = 
                let lineList = [for coord in line do yield game.board.[(fst coord), (snd coord)]]
                let rec Counter L search count = 
                    match L with
                    | [] -> count
                    | head::tail -> if head=search then Counter tail search count+1 else Counter tail search count
                let XCount = Counter lineList "X" 0
                let OCount = Counter lineList "O" 0
            
                if XCount = game.boardSize then Win(Cross, line)
                elif OCount = game.boardSize then Win(Nought, line)
                elif XCount >= 1 && OCount >= 1 then Draw
                else Undecided

            let isUndecided line = Undecided = CheckLine game line
            let isWin line = 
                let result = CheckLine game line
                if Win(Nought, line) = result || Win(Cross, line) = result
                then true
                else false
            match Seq.tryFind (fun e -> isWin e) (Lines game.boardSize) with
            | Some value -> CheckLine game value
            | None -> if Seq.exists (fun e -> isUndecided e) (Lines game.boardSize) then Undecided
                      else Draw

        let MiniMaxAB heuristic getTurn gameOver moveGenerator = 
            let rec MiniMax (alpha:int) (beta:int) game perspective : Option<Move> * int =
                NodeCounter.Increment()
                //let mutable a = alpha
                //let mutable b = beta Didnt show any improvement
                let maximiser state : Option<Move> * int = 
                    let moves = moveGenerator state
                    moves |> Seq.fold (fun acc move -> 
                        let newAlpha = max alpha (snd acc)
                        if newAlpha >= beta then 
                            acc
                        else 
                            let fixedAcc = 
                                if fst acc = None then
                                    (Some(move), snd acc)
                                else acc
                            ApplyMove game move |> ignore
                            let bestMove = [fixedAcc; (Some(move), snd (MiniMax newAlpha beta game perspective))] |> List.maxBy snd
                            ReverseMove game move
                            bestMove
                    ) (None, -10000)
                let minimiser state : Option<Move> * int = 
                    let moves = moveGenerator state
                    moves |> Seq.fold (fun acc move -> 
                        let newBeta = min beta (snd acc)
                        if alpha >= newBeta then 
                            acc
                        else 
                            let fixedAcc = 
                                if fst acc = None then
                                    (Some(move), snd acc)
                                else acc
                            ApplyMove game move |> ignore
                            let bestMove = [fixedAcc; (Some(move), snd (MiniMax alpha newBeta game perspective)) ] |> List.minBy snd
                            ReverseMove game move
                            bestMove
                    ) (None, 10000)
                    
                if gameOver game then 
                    let score = (None, heuristic game perspective)
                    score
                else if getTurn game = perspective then maximiser game
                else minimiser game
            NodeCounter.Reset()
            MiniMax

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
            let generator = MiniMaxAB heuristic getTurn gameOver moveGenerator
            let best_move = generator -1 1 game game.currentTurn
            Option.get (fst best_move)

        let GameStart first size = 
            let createBoard = array2D [| for x in 1..size -> [|for y in 1..size -> ""|]|]
            let gamestate = {currentTurn = first; boardSize = size; board = createBoard}
            gamestate


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