namespace QUT.CSharpTicTacToe
{
    public class Move : ITicTacToeMove
    {
        public int Row { get; }
        public int Col { get; }

        public Move(int row, int col)
        {
            Row = row;
            Col = col;
        }
    }
}
