namespace QUT

    module FSharpPureTicTacToeModel =
        open System.Drawing
        open System
    
        // type to represent the two players: Noughts and Crosses
        type Player = Nought | Cross

        // type to represent a single move specified using (row, column) coordinates of the selected square
        type Move = 
            { row: int; col: int }
            interface ITicTacToeMove with
                member this.Row with get() = this.row
                member this.Col with get() = this.col

        // type to represent the current state of the game, including the size of the game (NxN), who's turn it is and the pieces on the board
        type GameState = 
            { currentTurn: Player; boardSize: int; board: string list }
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.currentTurn
                member this.Size with get()    = this.boardSize
                member this.getPiece(row, col) = this.board.[row*this.boardSize+col]


        let CreateMove row col = 
            let move:Move = { row = row; col = col } //implied
            move

        let ApplyMove (oldState:GameState) (move: Move) : GameState =
            let rec InsertEle oldArr ele i = 
                match i, oldArr with
                | _, [] -> []
                | 0, _::tail -> ele::tail
                | i, head::tail -> head::InsertEle tail ele (i-1)
            let GetCurrentPlayerToken player = 
                match player with
                | Nought -> "O"
                | Cross -> "X"
            let CalcCoord (move: Move) size = 
                move.row*size+move.col
            let ChangePlayer player = 
                match player with
                | Nought -> Cross
                | Cross -> Nought

            let newBoard = InsertEle oldState.board (GetCurrentPlayerToken oldState.currentTurn) (CalcCoord move oldState.boardSize) 
            let newState = {currentTurn= ChangePlayer oldState.currentTurn; boardSize = oldState.boardSize; board = newBoard}
            newState

            

        // Returns a sequence containing all of the lines on the board: Horizontal, Vertical and Diagonal
        // The number of lines returned should always be (size*2+2)
        // the number of squares in each line (represented by (row,column) coordinates) should always be equal to size
        // For example, if the input size = 2, then the output would be: 
        //     seq [seq[(0,0);(0,1)];seq[(1,0);(1,1)];seq[(0,0);(1,0)];seq[(0,1);(1,1)];seq[(0,0);(1,1)];seq[(0,1);(1,0)]]
        // The order of the lines and the order of the squares within each line does not matter
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

        // Checks a single line (specified as a sequence of (row,column) coordinates) to determine if one of the players
        // has won by filling all of those squares, or a Draw if the line contains at least one Nought and one Cross
        let CheckLine (game:GameState) (line:seq<int*int>) : TicTacToeOutcome<Player> = 
            let lineList = [for coord in line do yield game.board.[(fst coord)*game.boardSize+(snd coord)]]
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

        let GameOutcome game = 
            let lines = Seq.map (fun line -> CheckLine game line) (Lines game.boardSize)
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

        let GameStart (firstPlayer:Player) size =
            let gamestate = {currentTurn = firstPlayer; boardSize = size; board = [ for i in 1..size*size -> ""]}
            gamestate

        let MiniMax game = 
            //Compare the perspective with the state to see what to be checking for. Return a score for this item based on its worth
            // return -1 0 1
            let heuristic state perspective = 
                match GameOutcome state with
                | Win(currentPlayer, _) -> if currentPlayer = perspective then 1 else -1
                | _ -> 0

            //Simply return current player
            let getTurn game = game.currentTurn

            //Check to see if draw/win exists, false if undecided
            let gameOver game = 
                match GameOutcome game with
                | Undecided -> false
                | _ -> true

            //Create all possible moves
            let moveGenerator state : seq<Move> = 
                let getEmptyTiles line = 
                    let newLine = line |> Seq.filter (fun tile -> 
                        let row = fst tile
                        let col = snd tile
                        let gameTile = state.board.[row*state.boardSize+col]
                        if gameTile = "" then true
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

            //Implement applymove func written earlier
            let applyMove game move = ApplyMove game move

            //Call minimaxi generator
            let generator = GameTheory.MiniMaxGenerator heuristic getTurn gameOver moveGenerator applyMove
            let best_move = generator game game.currentTurn
            Option.get (fst best_move)

        let MiniMaxWithPruning game = 
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
                        let gameTile = state.board.[row*state.boardSize+col]
                        if gameTile = "" then true
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
            let applyMove game move = ApplyMove game move

                

            let generator = GameTheory.MiniMaxWithAlphaBetaPruningGenerator heuristic getTurn gameOver moveGenerator applyMove
            let best_move = generator -1 1 game game.currentTurn
            Option.get (fst best_move)
        // plus other helper functions ...




        [<AbstractClass>]
        type Model() =
            abstract member FindBestMove : GameState -> Move
            interface ITicTacToeModel<GameState, Move, Player> with
                member this.Cross with get()             = Cross 
                member this.Nought with get()            = Nought 
                member this.GameStart(firstPlayer, size) = GameStart firstPlayer size
                member this.CreateMove(row, col)         = CreateMove row col
                member this.GameOutcome(game)            = GameOutcome game
                member this.ApplyMove(game, move)        = ApplyMove game move 
                member this.FindBestMove(game)           = this.FindBestMove game

        type BasicMiniMax() =
            inherit Model()
            override this.ToString()         = "Pure F# with basic MiniMax";
            override this.FindBestMove(game) = MiniMax game


        type WithAlphaBetaPruning() =
            inherit Model()
            override this.ToString()         = "Pure F# with Alpha Beta Pruning";
            override this.FindBestMove(game) = MiniMaxWithPruning game