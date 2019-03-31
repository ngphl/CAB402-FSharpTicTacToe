namespace QUT

    module GameTheory =
        open System.IO

        let MiniMaxGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : 'Game -> 'Player -> Option<'Move> * int =
            // Basic MiniMax algorithm without using alpha beta pruning
            let rec MiniMax game perspective =
                NodeCounter.Increment()
                //Try to generate all possible moves remaining
                let bestMove = 
                    if gameOver game then 
                        (None, heuristic game perspective)
                    else
                        let moveGenerator = moveGenerator game
                        //Consider it a maximising player always
                        //Apply minimax until fully recursed
                        
                        let level = moveGenerator |> Seq.map( fun move -> 
                            //System.Diagnostics.Debug.WriteLine(move)
                            //System.Console.WriteLine(move)
                            let newState = applyMove game move
                            (Some move, snd (MiniMax newState perspective))
                        ) 
                        //System.Diagnostics.Debug.WriteLine("")
                        //System.Console.WriteLine("")
                        if getTurn game = perspective then level |> Seq.maxBy (fun x -> snd x)
                        else level |> Seq.minBy (fun x -> snd x)
                bestMove
                //Apply the heurisitcc to the reutnred moveS
                //Compare every moves heuristic
                //Send it back up

                

                //Current node is game/perspective


                //foreach move in moves
                //Apply move, check gamestate to see if moves remain. Check if moves possible
                //Upon returning a move, check if it smaller/larger, if not expand the other node
                //Please read the wiki, doing freehand wont work
                //Do any moves exist? Check via gameover
                //If no moves go back up
                //If moves do exist
                //Create all possible moves in seq
                //Bind each move to a score using heuristic
                //Activate each move and apply minimax
                //Should return an option
            NodeCounter.Reset()
            MiniMax

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored            
            let rec MiniMax alpha beta oldState perspective =
                NodeCounter.Increment()
                raise (System.NotImplementedException("Alpha Beta Pruning"))
            NodeCounter.Reset()
            MiniMax
