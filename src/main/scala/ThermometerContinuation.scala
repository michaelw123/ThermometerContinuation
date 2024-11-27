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
  def reset[A](func:  () => A)(using theThermometer:Thermometer[A]): A = {
    thermometer(func, List[A | Null]())
  }

  def thermometer[A](func: => () =>A, fn_future: List[A | Null])(using thermo:Thermometer[A]): A = {
    thermo.nest.push((thermo.cur_expr, thermo.past, thermo.future))
    thermo.past = List[A]()
    thermo.future = fn_future
    thermo.cur_expr = Option(func)
    def run(): A = {
      try {
        func()
      } catch {
        case e: Done[A] => e.value
      }
    }

    val result = run()
    val x = thermo.nest.pop
    thermo.cur_expr = x._1
    thermo.past = x._2
    thermo.future = x._3
    result
  }

  def shift[A](func: => (A =>A) => A)(using thermo:Thermometer[A]): A= {
    val status = if (thermo.future.isEmpty) 1 else {
      if (thermo.future.head == null) 1 else 2
    }
    status match {
      case 1 =>
        val new_future = thermo.past.reverse
        val k = (v: A) => thermometer(thermo.cur_expr.get, new_future :+ v)
        thermo.past = thermo.past :+ null
        thermo.future = if (thermo.future.isEmpty) thermo.future else thermo.future.tail
        val result = func(k)
        throw Done(result)
      case 2 =>
        val value = thermo.future.head
        thermo.past = thermo.past :+ value
        thermo.future = thermo.future.tail
        value.asInstanceOf[A]
    }

  }

}


