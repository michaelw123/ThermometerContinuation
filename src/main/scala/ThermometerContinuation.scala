import scala.collection.mutable
import scala.collection.mutable.Stack

class Thermometer[A] {
  var future = List[A | Null]()
  var past = List[A | Null]()
  var cur_expr: Option[() => A] = None
  var nest: mutable.Stack[(Option[() => A], List[A | Null], List[A | Null])] = Stack()
}

object Thermometer {
  final case class Done[A](value: A) extends Exception
  inline def apply[A](): Thermometer[A] = new Thermometer[A]()
  def reset[A](func:  () => A)(using theThermometer:Thermometer[A]): A = {
    thermometer(func, List[A | Null]())
  }

  def thermometer[A](func: => () =>A, fn_future: List[A | Null])(using theThermometer:Thermometer[A]): A = {
    theThermometer.nest.push((theThermometer.cur_expr, theThermometer.past, theThermometer.future))
    theThermometer.past = List[A]()
    theThermometer.future = fn_future
    theThermometer.cur_expr = Option(func)
    def run(): A = {
      try {
        func()
      } catch {
        case e: Done[A] => e.value
      }
    }

    val result = run()
    val x = theThermometer.nest.pop
    theThermometer.cur_expr = x._1
    theThermometer.past = x._2
    theThermometer.future = x._3
    result
  }

  def shift[A](func: => (A =>A) => A)(using theThermometer:Thermometer[A]): A= {
    val status = if (theThermometer.future.isEmpty) 1 else {
      if (theThermometer.future.head == null) 1 else 2
    }
    status match {
      case 1 =>
        val new_future = theThermometer.past.reverse
        val k = (v: A) => thermometer(theThermometer.cur_expr.get, new_future :+ v)
        theThermometer.past = theThermometer.past :+ null
        theThermometer.future = if (theThermometer.future.isEmpty) theThermometer.future else theThermometer.future.tail
        val result = func(k)
        throw Done(result)
      case 2 =>
        val value = theThermometer.future.head
        theThermometer.past = theThermometer.past :+ value
        theThermometer.future = theThermometer.future.tail
        value.asInstanceOf[A]
    }

  }

}


