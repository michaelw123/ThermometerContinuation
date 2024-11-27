import scala.collection.mutable
import scala.collection.mutable.Stack

class ThermometerConti[A]:
  var future = List[A | Null]()
  var past = List[A | Null]()
  var cur_expr: Option[() => A] = None
  var nest: mutable.Stack[(Option[() => A], List[A | Null], List[A | Null])] = Stack()

object ThermometerConti:
 //var theThermometer = null
  def reset[A](func:  () => A)(using theThermometer:ThermometerConti[A]):A = {
    println(theThermometer.past)
    thermometer[A](func, List[A | Null]())
  }
  def shift[A](func: => (A =>A) => A) (using theThermometer:ThermometerConti[A]): A= {
    println(theThermometer.past)
    val k = (v: A) => thermometer(theThermometer.cur_expr.get, theThermometer.future :+ v)
    val result = func(k)
    result
  }
  def thermometer[A](func: => () =>A, fn_future: List[A | Null])(using theThermometer:ThermometerConti[A]): A = {
    println(theThermometer.past)
    func()
  }
end ThermometerConti



