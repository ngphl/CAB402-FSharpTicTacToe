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
                            //let bestScore = max (snd (MiniMax newAlpha beta newState perspective)) (snd acc)
                            let fixedAcc = 
                                if fst acc = None then
                                    (Some(move), snd acc)
                                else acc
                            let bestMove = [fixedAcc; (Some(move), snd (MiniMax newAlpha beta newState perspective))] |> List.maxBy snd
                            bestMove
                    ) (None, -1)
                let minimiser state = 
                    let moves = moveGenerator state
                    moves |> Seq.fold (fun acc move -> 
                        let newBeta = min beta (snd acc)
                        if alpha >= newBeta then 
                            acc
                        else 
                            let newState = applyMove game move
                            //let bestScore = min (snd (MiniMax alpha newBeta newState perspective)) (snd acc)
                            let fixedAcc = 
                                if fst acc = None then
                                    (Some(move), snd acc)
                                else acc

                            let bestMove = [fixedAcc; (Some(move), snd (MiniMax alpha newBeta newState perspective)) ] |> List.minBy snd
                            bestMove
                    ) (None, 1)
                    
                if gameOver game then 
                    let score = (None, heuristic game perspective)
                    score
                else if getTurn game = perspective then maximiser game
                else minimiser game
                    
            NodeCounter.Reset()
            MiniMax

(* let rec maxer moves alpha beta = 
    match moves with
    | head::tail -> 
        let newState = applyMove game head
        let new_alpha = max alpha (snd (MiniMax alpha beta newState perspective))
        if new_alpha >= beta then
            (Some(head), new_alpha)
        else 
            (Some(head), snd (maxer tail alpha beta))
    | [] -> (None, 0)
let rec minner moves alpha beta = 
    match moves with
    | head::tail -> 
        let newState = applyMove game head
        let new_beta = min beta (snd (MiniMax alpha beta newState perspective))
        if alpha >= new_beta then
            (Some(head), new_beta)
        else 
            (Some(head), snd (minner tail alpha beta))
    | [] -> (None, 0)*)

(*let rec tryMax moves alpha beta = 
    match moves with
    | head :: tail -> 
        if List.length tail = 0 then
            let newState= applyMove game head
            (Some(head), snd (MiniMax alpha beta newState perspective))
        else tryMax tail alpha beta
    | [] -> printf "ERROR"; (None, 0)

let rec tryMin moves alpha beta : Option<'Move>*int = 
    match moves with
    | head :: tail -> 
        if List.length tail = 0 then
            let newState= applyMove game head
            (Some(head), snd (MiniMax alpha beta newState perspective))
        else tryMin tail alpha beta
    | [] -> printf "ERROR"; (None, 0)

if gameOver game then 
    (None, heuristic game perspective)
else
    if getTurn game = perspective then
        NodeCounter.Increment()
        let moves = moveGenerator game |> Seq.toList
        tryMax moves alpha beta
    else 
        NodeCounter.Increment()
        let moves = moveGenerator game |> Seq.toList
        tryMin moves alpha beta*)

(* let rec maxxer moves = 
                        match moves with
                        | [move] -> 
                            let newState = applyMove game move
                            let score = MiniMax alpha beta newState perspective
                            let bestAlpha = max (snd score) alpha
                            let final = (Some(move), bestAlpha)
                            final
                        | move::tail -> 
                            let newState = applyMove game move
                            let score = MiniMax alpha beta newState perspective
                            let bestAlpha = max (snd score) alpha
                            if bestAlpha >= beta then (Some(move), bestAlpha)
                            else 
                                let final = Some(move), snd (maxxer tail)
                                final
                        | [] -> System.Console.WriteLine("ERROR");(None, 0)
                    let rec minner moves = 
                        match moves with
                        | [move] -> 
                            let newState = applyMove game move
                            let score = MiniMax alpha beta newState perspective
                            let bestBeta = min (snd score) beta
                            let final = (Some(move), bestBeta)
                            final
                        | move::tail -> 
                            let newState = applyMove game move
                            let score = MiniMax alpha beta newState perspective
                            let bestBeta = min (snd score) beta
                            if alpha >= bestBeta then (Some(move), bestBeta)
                            else 
                                let final = Some(move), snd (minner tail)
                                final
                        | [] -> System.Console.WriteLine("ERROR");(None, 0)
                    let moves = moveGenerator game |> Seq.toList
                    if getTurn game = perspective then maxxer moves
                    else minner moves*)