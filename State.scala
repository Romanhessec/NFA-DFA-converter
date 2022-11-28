import scala.collection.mutable
class State[A] (var name : A, var transitions : mutable.HashMap[String, mutable.Set[State[A]]]){}
