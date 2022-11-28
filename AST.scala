class AST {
  override def toString: String = {
    this match {
      case character : Character => character.str
      case star: Star => "STAR " + star.un.toString
      case binOp : BinOp => binOp.op + " " + binOp.left.toString + " " + binOp.right.toString
    }
  }
}
