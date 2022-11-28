import scala.collection.mutable
class StateDfa[A] (var name : A, var transitions : mutable.HashMap[String, StateDfa[A]], var nfaStates : mutable.Set[State[A]],
                   var transitionsNfa : mutable.HashMap[String, mutable.Set[State[A]]]){}

