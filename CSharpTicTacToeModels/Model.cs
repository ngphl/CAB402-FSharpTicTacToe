using System;
using System.Collections.Generic;

namespace QUT.CSharpTicTacToe
{
    public class WithAlphaBetaPruning : ITicTacToeModel<Game, Move, Player>
    {
        public Player Cross => Player.Cross;
        public Player Nought => Player.Nought;
        public override string ToString()
        {
            return "Impure C# with Alpha Beta Pruning";
        }

        public Game ApplyMove(Game game, Move move)
        {
            //newBoard = game.Board.Clone() as string[,];
            //newBoard[move.Row, move.Col] = token;
            game.ApplyMove(move);
            return game;
        }

        public Move CreateMove(int row, int col)
        {
            return new Move(row, col);
        }

        private List<Tuple<int, int>[]> GenerateLines(Game game)
        {
            List<Tuple<int, int>[]> lines = new List<Tuple<int, int>[]>();
            for (int x = 0; x < game.Size; x++)
            {
                List<Tuple<int, int>> row = new List<Tuple<int, int>>();
                List<Tuple<int, int>> col = new List<Tuple<int, int>>();
                for (int y = 0; y < game.Size; y++)
                {
                    row.Add(new Tuple<int, int>(x, y));
                    col.Add(new Tuple<int, int>(y, x));
                }
                lines.Add(row.ToArray());
                lines.Add(col.ToArray());
            }
            List<Tuple<int, int>> LDiag = new List<Tuple<int, int>>();
            List<Tuple<int, int>> RDiag = new List<Tuple<int, int>>();
            for (int x = 0; x < game.Size; x++)
            {
                LDiag.Add(new Tuple<int, int>(x, x));
                RDiag.Add(new Tuple<int, int>(game.Size - 1 - x, x));
            }
            lines.Add(LDiag.ToArray());
            lines.Add(RDiag.ToArray());
            return lines;
        }

        public TicTacToeOutcome<Player> GameOutcome(Game game)
        {
            TicTacToeOutcome<Player> result = TicTacToeOutcome<Player>.Draw;
            List<Tuple<int, int>[]> lines = GenerateLines(game);
            for(int i = 0; i < lines.Count; i++)
            {
                string [] line = new string [game.Size];
                for(int p = 0; p < lines[i].Length; p++)
                {
                    Tuple<int, int> coord = lines[i][p];
                    line[p] = game.Board[coord.Item1, coord.Item2];
                }
                Tuple<int, int>[] z = lines[i];
                int x_count = Array.FindAll(line, e => e=="X").Length;
                int o_count = Array.FindAll(line, e => e == "O").Length;
                int space_count = Array.FindAll(line, e => e == "").Length;
                if (result == TicTacToeOutcome<Player>.Draw || result == TicTacToeOutcome<Player>.Undecided)
                {
                    if (x_count == game.Size)
                    {
                        result = TicTacToeOutcome<Player>.NewWin(Player.Cross, lines[i]);
                    }
                    else if (o_count == game.Size)
                    {
                        result = TicTacToeOutcome<Player>.NewWin(Player.Nought, lines[i]);
                    }
                    else if (o_count >= 1 && x_count >= 1 && result == TicTacToeOutcome<Player>.Draw)
                    {
                        result = TicTacToeOutcome<Player>.Draw;
                    }
                    else
                    {
                        result = TicTacToeOutcome<Player>.Undecided;
                    }
                }
            }
            return result;
        }

        public List<Move> GetAllPossibleMoves(Game game)
        {
            List<Move> validMoves = new List<Move>();
            for (int x = 0; x < game.Size; x++)
            {
                for (int y = 0; y < game.Size; y++)
                {
                    if (game.Board[x, y] == "")
                    {
                        validMoves.Add(CreateMove(x, y));
                    }
                }
            }
            return validMoves;
            //Get indexes of all empty tiles
            //For each index create a move using x y
            //Bind it all to a list and send it up
        }

        public Move FindBestMove(Game game)
        {
            throw new System.NotImplementedException("FindBestMove");
        }

        private Move bestMove MiniMax(Game game, int alpha, int beta, Player perspective)
        {
            //Check if gameoutcome is a win or a draw then return

            if (game.Turn == perspective) {

            } else
            {

            }
        }

        public Game GameStart(Player first, int size)
        {
            string[,] board = new string[size,size];
            for(int x=0; x < size; x++)
            {
                for (int y = 0; y < size; y++)
                {
                    board[x,y] = "";
                }
            }
            return new Game(first, size, board);
        }
    }
}