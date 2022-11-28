import scala.collection.mutable
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

class Nfa[A](var states : mutable.Set[State[A]], var initialState : State[A], var finalState : State[A]) {

  def map[B](f: A => B) : Nfa[B] = ???

  def next(state:A, c: Char): mutable.Set[A] = {
    val set: mutable.Set[A] = mutable.Set()
    for (st <- states){
      if (st.name == state) {
        val setToIterate: mutable.Set[State[A]] = st.transitions.getOrElse(c.toString, null)
        if (setToIterate != null) {
          for (w <- setToIterate) {
            set += w.name
          }
        }
      }
    }
    set
  }

  def accepts(str: String): Boolean = {

    if (states == null)
      return false

    val chArr: Array[Char] = str.toCharArray
    var stateSet : mutable.Set[State[A]] = mutable.Set(initialState)

    traverseEps(initialState, stateSet)

    for (ch <- chArr) {
      val newSet: mutable.Set[State[A]] = mutable.Set()

      for (st <- stateSet) {
        var tempSet: mutable.Set[State[A]] = st.transitions.getOrElse(ch.toString, null)
        if (tempSet == null)
          tempSet = mutable.Set()

        for (tSt <- tempSet) {
          newSet += tSt
        }
      }

      if (newSet.isEmpty) {
        return false //sink state
      }

      for (newSt <- newSet) {
        traverseEps(newSt, newSet)
      }

      stateSet = newSet
    }

    for (st <- stateSet) {
      if (st.name == finalState.name)
        return true
    }

    false
  }

  def getStates : mutable.Set[A] = {
    val toReturn: mutable.Set[A] = mutable.Set()
    for (state <- states){
      toReturn += state.name
    }
    toReturn
  }

  def isFinal(state: A): Boolean = {
    state == finalState.name
  }

  def traverseEps(state: State[A], setStates : mutable.Set[State[A]]): Unit = {
    val tempSet: mutable.Set[State[A]] = state.transitions.getOrElse("eps", null)
    if (tempSet == null)
      return

    for (st <- tempSet) {
      setStates.add(st)
    }

    for (st <- tempSet) {
      traverseEps(st, setStates)
    }

  }
}

object Nfa {

  var count : Int = 0

  def fromPrenex(str: String): Nfa[Int] = {
    treeToNfa(prenexToTree(str))
  }

  def parseString(str : String) : List[String] = {
    var sb = new StringBuilder
    var isSpaceBefore : Boolean = false
    val strList : ListBuffer[String] = ListBuffer()
    for (char <- str) {
      if (char.equals(' ')) {
        if(!isSpaceBefore) {
          strList.addOne(sb.toString())
          sb = new StringBuilder
          isSpaceBefore = true
        } else {
          isSpaceBefore = false
          sb += char
        }
      } else {
        isSpaceBefore = false
        sb += char
      }
    }
    strList.addOne(sb.toString())
    strList.toList
  }

  def prenexToTree(str : String): AST = {
    val treeStack: mutable.Stack[AST] = mutable.Stack[AST]()
    val strArr : Array[String] = parseString(str).toArray.reverse

    for (lexem <- strArr) {
      lexem match {
        case "void" =>
          treeStack.push(null)
        case "UNION" =>
          treeStack.push(new BinOp(treeStack.pop(), treeStack.pop(), "UNION"));
        case "CONCAT" =>
          treeStack.push(new BinOp(treeStack.pop(), treeStack.pop(), "CONCAT"));
        case "STAR" =>
          treeStack.push(new Star(treeStack.pop()))
        case _ =>
          treeStack.push(new Character(lexem))
      }
    }

    treeStack.pop()
  }

  def treeToNfa(tree : AST): Nfa[Int] = {

    if (tree == null)
      return new Nfa[Int](null, null, null)

    val nfaStack = new mutable.Stack[Nfa[Int]]

    recursive(tree, nfaStack)

    val nfa: Nfa[Int] = nfaStack.pop()

    nfa
  }

  def recursive(tree : AST, nfaStack : mutable.Stack[Nfa[Int]]) : Any = {

    tree match {
      case character: Character =>
        nfaStack.push(accCharNfa(character.str, count))
        Nfa.count += 2
      case op: BinOp =>
        recursive(op.left, nfaStack)
        recursive(op.right, nfaStack)
        if (op.op == "UNION") {
          nfaStack.push(accUnionNfa(nfaStack.pop(), nfaStack.pop(), count))
          Nfa.count += 2
        }
        else {
          nfaStack.push(accConcatNfa(nfaStack.pop(), nfaStack.pop()))
        }
      case star: Star =>
        recursive(star.un, nfaStack)
        nfaStack.push(accStarNfa(nfaStack.pop(), count))
        Nfa.count += 2
      case _ =>
    }

  }

  def accCharNfa(char : String, count : Int): Nfa[Int] = {
    val map1: mutable.HashMap[String, mutable.Set[State[Int]]] = new mutable.HashMap[String, mutable.Set[State[Int]]]()
    val state1: State[Int] = new State(count, map1)

    val map2: mutable.HashMap[String, mutable.Set[State[Int]]] = new mutable.HashMap[String, mutable.Set[State[Int]]]()
    val state2: State[Int] = new State(count + 1, map2)

    state1.transitions.put(char, mutable.Set(state2))
    new Nfa[Int](mutable.Set(state1, state2), state1, state2)
  }

  def accUnionNfa(nfa1 : Nfa[Int], nfa2 : Nfa[Int], count : Int) : Nfa[Int] = {

    val mapIn: mutable.HashMap[String, mutable.Set[State[Int]]] = new mutable.HashMap[String, mutable.Set[State[Int]]]()
    mapIn.put("eps", mutable.Set(nfa1.initialState, nfa2.initialState))
    val inState: State[Int] = new State(count, mapIn)
    val finState: State[Int] = new State(count + 1, new mutable.HashMap[String, mutable.Set[State[Int]]]())

    nfa1.finalState.transitions.put("eps", mutable.Set(finState))
    nfa2.finalState.transitions.put("eps", mutable.Set(finState))

    val allStates: mutable.Set[State[Int]] = mutable.Set()
    for (state <- nfa1.states){
      allStates += state
    }
    for (state <- nfa2.states){
      allStates += state
    }
    allStates += inState
    allStates += finState
    new Nfa[Int](allStates, inState, finState)
  }

  def accConcatNfa(nfa1: Nfa[Int], nfa2: Nfa[Int]) : Nfa[Int] = {

    nfa2.finalState.transitions.put("eps", mutable.Set(nfa1.initialState))
    val allStates: mutable.Set[State[Int]] = mutable.Set()
    for (state <- nfa1.states){
      allStates += state
    }
    for (state <- nfa2.states){
      allStates += state
    }
    new Nfa[Int](allStates, nfa2.initialState, nfa1.finalState)
  }

  def accStarNfa(nfa : Nfa[Int], count : Int) : Nfa[Int] = {
    nfa.finalState.transitions.put("eps", mutable.Set(nfa.initialState))

    val inState: State[Int] = new State(count, new mutable.HashMap[String, mutable.Set[State[Int]]]())
    val finState: State[Int] = new State(count + 1, new mutable.HashMap[String, mutable.Set[State[Int]]]())

    inState.transitions.put("eps", mutable.Set(nfa.initialState, finState))
    nfa.finalState.transitions.put("eps", mutable.Set(finState, nfa.initialState))

    val allStates: mutable.Set[State[Int]] = mutable.Set()
    for (state <- nfa.states){
      allStates += state
    }
    allStates += inState
    allStates += finState

    new Nfa[Int](allStates, inState, finState)
  }
}