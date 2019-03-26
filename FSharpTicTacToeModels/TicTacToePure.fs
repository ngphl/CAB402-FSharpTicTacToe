namespace QUT

    module FSharpPureTicTacToeModel =
        open System.Drawing
        open System
    
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
            { currentTurn: Player; boardSize: int; board: string list }//state: (Move*Player) list } //row, col player?
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
                | i, [] -> []
                | 0, head::tail -> ele::tail
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

            //GRAVEYARD
            //Considers i starting from tail
            //let rec InsertEle (oldarr:list<int>) ele i =
            //    match oldarr with
            //    | [] -> []
            //    | head::tail -> 
            //        System.Console.Write("Length is: " + oldarr.Length.ToString() + "& Desired pos is " + i.ToString() + " || ")
            //        System.Console.WriteLine("Total is : " + (oldarr.Length-1).ToString())
            //        if oldarr.Length-1 = i
            //        then head::ele::(InsertEle tail.Tail ele i) 
            //        else head::(InsertEle tail ele i)
            (* Whoops impure...
            let coord = move.row*oldState.boardSize + move.col 
            match oldState.currentTurn with
            | Nought-> 
                Array.set oldState.board coord "O"
                {currentTurn = Cross; boardSize=oldState.boardSize; board=oldState.board}
            | Cross -> 
                Array.set oldState.board coord "X"
                {currentTurn = Nought; boardSize=oldState.boardSize; board=oldState.board}
            *)
            

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
                for x in 0..size-1 do yield GetCols x; //Rebind these to !yield for readability if time later on
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
            //Extract relevant line from gamestate & bind to a list
            //FILTER items and return counter for N and C
            //Check against each other, N==3|C==3 = Win, N==C = Draw, else Undecided (1ofeach)

        let GameOutcome game = 
            let lines = Seq.map (fun line -> CheckLine game line) (Lines game.boardSize)
            //Seq.find (fun x -> x = Win()) lines
            Lines game.boardSize |> Seq.map (fun line -> CheckLine game line)
                                 
            //Map a new value to each line
            //See if win exists. If it does grab it
            //If win doesnt exist check if undecided exists. If it does return undecided
            // if neither return Draw
            
        let GameStart (firstPlayer:Player) size =
            let gamestate = {currentTurn = firstPlayer; boardSize = size; board = [ for i in 1..size*size -> ""]}
            gamestate
            //impure
            //let gameState = {currentTurn = firstPlayer; boardSize = size; board = [| for i in 1 .. size*size -> "" |]}
            //For observing the array
            //for x in 0..size-1 do
            //    for y in 0..size-1 do
            //        let index = x*size+y
            //        System.Console.Write("Accessing" + index.ToString() + "| ")
            //        System.Console.Write(gameState.board.[x*size+y])
            //    System.Console.WriteLine("")

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