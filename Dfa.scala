
import scala.collection.mutable
import scala.language.postfixOps

class Dfa[A] (var states : mutable.Set[StateDfa[A]], var initialState : StateDfa[A], var finalStates : mutable.Set[StateDfa[A]]){

  // The following methods are only the methods directly called by the test suite. You can (and should) define more.

  def map[B](f: A => B) : Dfa[B] = ???

  def next(state:A, c: Char): A = {
    for (st <- states) {
      if (st.name == state){
        if (st.transitions.contains(c.toString))
          return st.transitions.getOrElse(c.toString, null).name
      }
    }
    state
  }

  def accepts(str: String): Boolean = {
    if (states == null)
      return false

    val chArr: Array[Char] = str.toCharArray
    var currentState : StateDfa[A] = initialState

    for (ch <- chArr) {
      currentState = currentState.transitions.getOrElse(ch.toString, null)
      if (currentState == null)
        return false
    }
    if (isFinal(currentState.name))
      return true
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
    for (stateF <- finalStates) {
      if (stateF.name == state)
        return true
    }
    false
  }
}

// This is a companion object to the Dfa class. This allows us to call the method fromPrenex without instantiating the Dfa class beforehand.
// You can think of the methods of this object like static methods of the Dfa class
object Dfa {

  var count : Int = 0
  var allSets : mutable.Set[mutable.Set[State[Int]]] = mutable.Set()

  def fromPrenex(str: String): Dfa[Int] = {

    val nfa: Nfa[Int] = Nfa.fromPrenex(str)
    if (nfa.states == null){
      return new Dfa[Int](null,null,null)
    }
    val nfaFinalState: State[Int] = nfa.finalState

    val initialStates: mutable.Set[State[Int]] = mutable.Set(nfa.initialState)
    val map: mutable.HashMap[String, StateDfa[Int]] = mutable.HashMap()
    nfa.traverseEps(nfa.initialState, initialStates)

    val nfaTransition: mutable.HashMap[String, mutable.Set[State[Int]]] = mutable.HashMap()
    nextStates(initialStates, nfaTransition, nfa)
    val inState: StateDfa[Int] = new StateDfa[Int](count, map, initialStates, nfaTransition)

    val finalStates: mutable.Set[StateDfa[Int]] = mutable.Set()
    if (checkFinal(initialStates, nfaFinalState))
      finalStates += inState
    val allStates: mutable.Set[StateDfa[Int]] = mutable.Set(inState)
    createDfa(inState, finalStates, nfa.finalState, nfa, allStates)

    //print(allStates.size)
    new Dfa[Int](allStates, inState, finalStates)
  }

  def checkFinal(states : mutable.Set[State[Int]], finalState : State[Int]): Boolean ={
    for(state <- states) {
      if (state == finalState)
        return true
    }
    false
  }

  def createDfa(prev : StateDfa[Int], finalStates : mutable.Set[StateDfa[Int]], finalState : State[Int], nfa : Nfa[Int],
                allStates : mutable.Set[StateDfa[Int]]) : Unit = {

    val keySet = prev.transitionsNfa.keySet

    for (key <- keySet) {
      val states: mutable.Set[State[Int]] = prev.transitionsNfa.getOrElse(key, null) //never null

      var alreadyThere : Boolean = false

      //check if already exists
      for (state <- allStates) {
        val tempStates = state.nfaStates
        if (tempStates.equals(states)) {
          prev.transitions.put(key, state)
          alreadyThere = true
        }
      }

      if (!alreadyThere) {
        val nfaTransition: mutable.HashMap[String, mutable.Set[State[Int]]] = mutable.HashMap()
        nextStates(states, nfaTransition, nfa)
        count += 1
        val mapForState: mutable.HashMap[String, StateDfa[Int]] = mutable.HashMap()
        val state: StateDfa[Int] = new StateDfa[Int](count, mapForState, states, nfaTransition)
        if (checkFinal(states, finalState)) {
          finalStates += state
        }
        prev.transitions.put(key, state)
        allStates += state
        createDfa(state, finalStates, finalState, nfa, allStates)
      }
    }
  }

  def getKeySet(states :mutable.Set[State[Int]]) : mutable.Set[String] = {
    val keySet: mutable.Set[String] = mutable.Set()
    for (state <- states) {
      val tempKeySet = state.transitions.keySet
      for (key <- tempKeySet) {
        keySet += key
      }
    }
    keySet
  }

  def nextStates(states : mutable.Set[State[Int]], map : mutable.HashMap[String, mutable.Set[State[Int]]], nfa : Nfa[Int]): Unit = {

    val keySet: mutable.Set[String] = getKeySet(states)
    for (state <- states) {
      for (key <- keySet) {
        if (!key.equals("eps")) {
          if (state.transitions.contains(key)) {
            val tempSet: mutable.Set[State[Int]] = state.transitions.getOrElse(key, null) //never going to be null
            if (map.contains(key)) {
              val set: mutable.Set[State[Int]] = map.getOrElse(key, null) //never going to be null
              for (state1 <- set) {
                tempSet += state1
              }

            }
            val epsilonSet: mutable.Set[State[Int]] = mutable.Set()
            for (tempState <- tempSet) {
              nfa.traverseEps(tempState, epsilonSet)
            }
            for (epsState <- epsilonSet) {
              tempSet += epsState
            }
            map.put(key, tempSet) //replace
          }
        }
      }
    }
  }

  // You can add more methods to this object
}
