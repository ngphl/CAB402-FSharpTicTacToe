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
                        //Apply minimax until leaf
                        
                        let level = moveGenerator |> Seq.map( fun move -> 
                            let newState = applyMove game move
                            (Some move, snd (MiniMax newState perspective))
                        ) 
                        if getTurn game = perspective then level |> Seq.maxBy (fun x -> snd x)
                        else level |> Seq.minBy (fun x -> snd x)
                bestMove
            NodeCounter.Reset()
            MiniMax

        let MiniMaxWithAlphaBetaPruningGenerator (heuristic:'Game -> 'Player -> int) (getTurn: 'Game -> 'Player) (gameOver:'Game->bool) (moveGenerator: 'Game->seq<'Move>) (applyMove: 'Game -> 'Move -> 'Game) : int -> int -> 'Game -> 'Player -> Option<'Move> * int =
            // Optimized MiniMax algorithm that uses alpha beta pruning to eliminate parts of the search tree that don't need to be explored            
            let rec MiniMax (alpha:int) (beta:int) game perspective : Option<'Move> * int =
                NodeCounter.Increment()
                
                let maximiser state = 
                    let moves = moveGenerator state
                    moves |> Seq.fold (fun acc move -> 
                        let newAlpha = max alpha (snd acc)
                        if newAlpha >= beta then 
                            acc
                        else 
                            let newState = applyMove game move
                            let fixedAcc = 
                                if fst acc = None then
                                    (Some(move), snd acc)
                                else acc
                            let bestMove = [fixedAcc; (Some(move), snd (MiniMax newAlpha beta newState perspective))] |> List.maxBy snd
                            bestMove
                    ) (None, -10000)
                let minimiser state = 
                    let moves = moveGenerator state
                    moves |> Seq.fold (fun acc move -> 
                        let newBeta = min beta (snd acc)
                        if alpha >= newBeta then 
                            acc
                        else 
                            let newState = applyMove game move
                            let fixedAcc = 
                                if fst acc = None then
                                    (Some(move), snd acc)
                                else acc

                            let bestMove = [fixedAcc; (Some(move), snd (MiniMax alpha newBeta newState perspective)) ] |> List.minBy snd
                            bestMove
                    ) (None, 10000)
                    
                if gameOver game then 
                    let score = (None, heuristic game perspective)
                    score
                else if getTurn game = perspective then maximiser game
                else minimiser game
                    
            NodeCounter.Reset()
            MiniMax