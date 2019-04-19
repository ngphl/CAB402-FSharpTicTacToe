using System.Collections.Generic;
using System;

namespace QUT.CSharpTicTacToe
{
    public class Game : ITicTacToeGame<Player>
    {
        public int Size { get; }
        public Player Turn { get; set; }
        public string[,] Board { get; set; }
        public string getPiece(int row, int col)
        {
            return Board[row, col];
        }
        public Game(Player turn, int boardSize, string[,] board)
        {
            Size = boardSize;
            Turn = turn;
            Board = board;
        }
    }
}