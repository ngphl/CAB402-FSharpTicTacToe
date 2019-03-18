namespace QUT

    module FSharpPureTicTacToeModel =
        open System.Drawing
    
        // type to represent the two players: Noughts and Crosses
        type Player = Nought | Cross

        // type to represent a single move specified using (row, column) coordinates of the selected square
        type Move = 
            { row: int; col: int } //{ something: int (* TODO implement type *) }
            interface ITicTacToeMove with
                member this.Row with get() = this.row
                member this.Col with get() = this.col

        // type to represent the current state of the game, including the size of the game (NxN), who's turn it is and the pieces on the board
        type GameState = 
            { currentTurn: Player; boardSize: int; board: string array }//state: (Move*Player) list } //row, col player?
            interface ITicTacToeGame<Player> with
                member this.Turn with get()    = this.currentTurn
                member this.Size with get()    = this.boardSize
                member this.getPiece(row, col) = this.board.[row*this.boardSize+col]


        let CreateMove row col = 
            let move:Move = { row = row; col = col } //implied
            move

        let ApplyMove (oldState:GameState) (move: Move) : GameState =
            let coord = move.row*oldState.boardSize+move.col
            let addToken (oldBoard:string array) coords currentPlayer : string array= 
                match currentPlayer with
                | Nought -> oldBoard.[coords] <- "AAAAAAAA" //not letting me mutate the array, could do something where it recurses over the array filling it with previous values, stopping on the new coord and filling it witha  new value, would also well support exceptions if necesary
                | Cross -> oldBoard.[coords]
            let changePlayer (state:GameState) = 
                match state.currentTurn with
                | Nought -> Cross
                | Cross -> Nought
            
            let newState = {currentTurn = changePlayer oldState; boardSize = oldState.boardSize; board = addToken oldState.board coord oldState.currentTurn}
            newState

        // Returns a sequence containing all of the lines on the board: Horizontal, Vertical and Diagonal
        // The number of lines returned should always be (size*2+2)
        // the number of squares in each line (represented by (row,column) coordinates) should always be equal to size
        // For example, if the input size = 2, then the output would be: 
        //     seq [seq[(0,0);(0,1)];seq[(1,0);(1,1)];seq[(0,0);(1,0)];seq[(0,1);(1,1)];seq[(0,0);(1,1)];seq[(0,1);(1,0)]]
        // The order of the lines and the order of the squares within each line does not matter
        let Lines (size:int) : seq<seq<int*int>> = raise (System.NotImplementedException("Lines"))

        // Checks a single line (specified as a sequence of (row,column) coordinates) to determine if one of the players
        // has won by filling all of those squares, or a Draw if the line contains at least one Nought and one Cross
        let CheckLine (game:GameState) (line:seq<int*int>) : TicTacToeOutcome<Player> = raise (System.NotImplementedException("CheckLine"))

        let GameOutcome game = raise (System.NotImplementedException("GameOutcome"))

        let GameStart (firstPlayer:Player) size =
            let gameState = {currentTurn = firstPlayer; boardSize = size; board = [| for i in 1 .. size*size -> "" |]}
            //For observing the array
            //for x in 0..size-1 do
            //    for y in 0..size-1 do
            //        let index = x*size+y
            //        System.Console.Write("Accessing" + index.ToString() + "| ")
            //        System.Console.Write(gameState.board.[x*size+y])
            //    System.Console.WriteLine("")
                
            
            gameState

        let MiniMax game = raise (System.NotImplementedException("MiniMax"))

        let MiniMaxWithPruning game = raise (System.NotImplementedException("MiniMaxWithPruning"))

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
            override this.FindBestMove(game) = raise (System.NotImplementedException("FindBestMove"))


        type WithAlphaBetaPruning() =
            inherit Model()
            override this.ToString()         = "Pure F# with Alpha Beta Pruning";
            override this.FindBestMove(game) = raise (System.NotImplementedException("FindBestMove"))