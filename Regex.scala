import scala.collection.mutable
import scala.collection.mutable.ListBuffer

object Regex {

  def syntacticSugar(char: Char) : List[Char] = {
    char match {
      case 'a' => "(a|b|c|d|e|f|g|h|i|j|k|l|m|n|o|p|q|r|s|t|u|v|w|x|y|z)".toList
      case 'A' => "(A|B|C|D|E|F|G|H|I|J|K|L|M|N|O|P|Q|R|S|T|U|V|W|X|Y|Z)".toList
      case '0' => "(0|1|2|3|4|5|6|7|8|9)".toList
    }
  }

  def syntacticSugarParser(charToSearch : Char, finalList : scala.collection.mutable.ListBuffer[Either[String, String]]) : Unit = {
    for (char <- syntacticSugar(charToSearch)) {
      char match {
        case '|' => finalList.addOne(Left(char.toString))
        case '(' => finalList.addOne(Left(char.toString))
        case ')' => finalList.addOne(Left(char.toString))
        case _ => finalList.addOne(Right(char.toString))
      }
    }
  }

  def questionMark(char: Char, finalList : scala.collection.mutable.ListBuffer[Either[String, String]]) : Unit = {
    char match {
      case ']' =>
        finalList.remove(finalList.length - 1)
        finalList.addOne(Left("|"))
        finalList.addOne(Right("eps"))
        finalList.addOne(Left(")"))
      case ')' =>
        finalList.remove(finalList.length - 1)
        finalList.addOne(Left("|"))
        finalList.addOne(Right("eps"))
        finalList.addOne(Left(")"))
      case _ =>
        val temp : Either[String, String] = finalList.remove(finalList.length - 1)
        finalList.addOne(Left("("))
        finalList.addOne(temp)
        finalList.addOne(Left("|"))
        finalList.addOne(Right("eps"))
        finalList.addOne(Left(")"))
    }
  }

  def findParentheses(finalList : scala.collection.mutable.ListBuffer[Either[String, String]]) : Int = {
    var findParentheses : Int = finalList.length - 1
    var stopLoop : Boolean = false

    while (!stopLoop) {
      finalList(findParentheses) match {
        case Left(l) =>
          if (l.equals("("))
            stopLoop = true
        case Right(r) =>
          if (r.equals("("))
            stopLoop = true
      }
      findParentheses -= 1
    }

    findParentheses += 1
    findParentheses
  }

  def plusParentheses(finalList : scala.collection.mutable.ListBuffer[Either[String, String]]) : Unit = {
    val iteratorBegin : Int = findParentheses(finalList)
    val iteratorStop : Int = finalList.length
    for (iterator <- iteratorBegin until iteratorStop) {
      finalList.addOne(finalList(iterator))
    }
    finalList.addOne(Left("*"))
  }

  def plusParser(charToSearch : Char, finalList : scala.collection.mutable.ListBuffer[Either[String, String]]) : Unit = {
    charToSearch match {
      case ']' =>
        plusParentheses(finalList)
      case ')' =>
        plusParentheses(finalList)
      case _ =>
        finalList.addOne(finalList.last)
        finalList.addOne(Left("*"))
    }
  }

  def preprocess(s:List[Char]): List[Either[String,String]] = {

    val finalList : scala.collection.mutable.ListBuffer[Either[String, String]] = ListBuffer()
    var skip : Int = 0

    for (iterator <- s.indices) {
      if (skip == 0) {
        s(iterator) match {
          case '\'' =>
            finalList.addOne(Right(s(iterator + 1).toString))
            skip += 2
          case '[' =>
            syntacticSugarParser(s(iterator + 1), finalList)
            skip += 4
          case '?' => questionMark(s(iterator - 1), finalList)
          case '+' => plusParser(s(iterator - 1), finalList)
          case '(' => finalList.addOne(Left(s(iterator).toString))
          case ')' => finalList.addOne(Left(s(iterator).toString))
          case '*' => finalList.addOne(Left(s(iterator).toString))
          case '|' => finalList.addOne(Left(s(iterator).toString))
          case _ => finalList.addOne(Right(s(iterator).toString))
        }
      } else {
        skip -= 1
      }
    }
    convertAllEps(finalList)
  }

  //This function does what it says it does; the implementation has over 400 roentgens/rad; proceed at your risk.
  def convertAllEps(finalList : ListBuffer[Either[String, String]]) : List[Either[String, String]] = {
    val myFinalList : scala.collection.mutable.ListBuffer[Either[String, String]] = ListBuffer()
    var skip : Int = 0

    for (iterator <- finalList.indices) {
      finalList(iterator) match {
        case Right(right) =>
          if (right.equals("e")) {
            finalList(iterator + 1) match {
              case Right(right2) =>
                if (right2.equals("p")) {
                  finalList(iterator + 2) match {
                    case Right(right3) =>
                      if (right3.equals("s")) {
                        myFinalList.addOne(Right("\u0000"))
                        skip = 3
                      }
                    case Left(_) =>
                  }
                }
              case Left(_) =>
            }
          }
          if (skip <= 0)
            myFinalList.addOne(Right(right))
        case Left(left) => myFinalList.addOne(Left(left))
      }
      skip -= 1
    }
    myFinalList.toList
  }

  def printJustForTesting(preprocessed : List[Either[String,String]]) : Unit = {
    for (w <- preprocessed){
      w match {
        case Left(l) => print(l)
        case Right(r) => print(r)
      }
    }
  }

  def createPrecedenceMap() : mutable.HashMap[String, Int] = {
    val map : mutable.HashMap[String, Int] = new mutable.HashMap[String, Int]()
    map.put(")", 4)
    map.put("(", 4)
    map.put("*", 3)
    map.put(".", 2)
    map.put("|", 1)
    map
  }

  def createAST(operatorStack : scala.collection.mutable.Stack[String], ASTStack : scala.collection.mutable.Stack[AST]) : Unit = {
    operatorStack.pop() match {
      case "*" => ASTStack.push(new Star(ASTStack.pop()))
      case "." => ASTStack.push(new BinOp(ASTStack.pop(), ASTStack.pop(), "CONCAT"))
      case "|" => ASTStack.push(new BinOp(ASTStack.pop(), ASTStack.pop(), "UNION"))
    }
  }

  def createUntilParentheses(operatorStack : scala.collection.mutable.Stack[String], ASTStack : scala.collection.mutable.Stack[AST]) : Unit = {
    while (!operatorStack.top.equals(")")) {
      createAST(operatorStack, ASTStack)
    }
    operatorStack.pop()
  }

  def checkForConcat(string : String, operatorStack : scala.collection.mutable.Stack[String], ASTStack : scala.collection.mutable.Stack[AST], lastChecked : Either[String, String], precedenceMap : mutable.HashMap[String, Int]) : Unit = {
    string match {
      case ")" =>
        lastChecked match {
          case Right(_) => operatorStack.push(".")
          case Left(value) => if (value.equals("(")) createUntilPrecedence(operatorStack, ASTStack, ".", precedenceMap)
        }
      case "*" =>
        lastChecked match {
          case Right(_) => operatorStack.push(".")
          case Left(value) => if (value.equals("(")) createUntilPrecedence(operatorStack, ASTStack, ".", precedenceMap)
        }
      case _ =>
    }
  }

  def createUntilPrecedence(operatorStack : scala.collection.mutable.Stack[String], ASTStack : scala.collection.mutable.Stack[AST], elem : String, precedenceMap : mutable.HashMap[String, Int]) : Unit = {
    if (operatorStack.nonEmpty && precedenceMap(elem) < precedenceMap(operatorStack.top) && !operatorStack.top.equals(")")) {
      while (operatorStack.nonEmpty && precedenceMap(elem) <= precedenceMap(operatorStack.top) && !operatorStack.top.equals(")")) {
        createAST(operatorStack, ASTStack)
      }
    }
    operatorStack.push(elem)
  }

  def createRemainingStack(operatorStack : scala.collection.mutable.Stack[String], ASTStack : scala.collection.mutable.Stack[AST]) : Unit = {
    while (operatorStack.nonEmpty){
      if (operatorStack.top.equals("(") || operatorStack.top.equals(")"))
        operatorStack.pop()
      else
        createAST(operatorStack, ASTStack)
    }
  }

  def toPrenex(str: String): String = {

    val toIterate = preprocess(str.toList).reverse
    val precedenceMap = createPrecedenceMap()
    val operatorStack : scala.collection.mutable.Stack[String] = new mutable.Stack()
    val ASTStack : scala.collection.mutable.Stack[AST] = new mutable.Stack()

    var lastChecked : Either[String, String] = null

    for (elem <- toIterate) {
      elem match {
        case Left(left) =>
          if (lastChecked != null)
            checkForConcat(left, operatorStack, ASTStack, lastChecked, precedenceMap)
          if (left.equals("("))
            createUntilParentheses(operatorStack, ASTStack)
          else if (operatorStack.isEmpty || operatorStack.top.equals(")") || left.equals(")") || precedenceMap(left) > precedenceMap(operatorStack.top))
            operatorStack.push(left)
          else
            createUntilPrecedence(operatorStack, ASTStack, left, precedenceMap)
        case Right(right) =>
          if (lastChecked != null) {
            lastChecked match {
              case Right(_) => createUntilPrecedence(operatorStack, ASTStack, ".", precedenceMap)
              case Left(value) => if (value.equals("(")) createUntilPrecedence(operatorStack, ASTStack, ".", precedenceMap)
            }
          }
          if (right.equals("\u0000"))
            ASTStack.push(new Character("eps"))
          else
            ASTStack.push(new Character(right))
      }
      lastChecked = elem
    }

    createRemainingStack(operatorStack, ASTStack)

    ASTStack.pop().toString
  }
}

