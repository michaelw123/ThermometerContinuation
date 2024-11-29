import scala.collection.mutable.Stack

class Thermometer[A]:
  var future = List.empty[A | Null]
  var past = List.empty[A | Null]
  var cur_expr: Option[() =>A] = None
  var nest = Stack.empty[(Option[() =>A], List[A | Null], List[A | Null])]
  def pushNest = nest.push((cur_expr, past, future))
  def popNest = {
    val x = nest.pop
    cur_expr = x._1
    past = x._2
    future = x._3
  }


object Thermometer:
  final case class Done[A](value: A) extends Exception
  def reset[A](func:  => A)(using thermo:Thermometer[A]): A = {
    thermometer(() =>func, List.empty[A | Null])
  }

  def thermometer[A](func: => () =>A, fn_future: List[A | Null])(using thermo:Thermometer[A]): A = {
    thermo.pushNest
    thermo.past = List.empty[A| Null]
    thermo.future = fn_future
    thermo.cur_expr = Some(func)
    def run(): A = {
      try {
        func()
      } catch {
        case e: Done[A] => e.value
      }
    }
    val result = run()
    thermo.popNest
    result
  }

  def shift[A](func: => (A =>A) => A)(using thermo:Thermometer[A]): A= {
    val (status, value) = if (thermo.future.isEmpty) (1, null) else {
      val x = thermo.future.head
      thermo.future = thermo.future.drop(1)
      if (x == null) (1, null) else (2, x)
    }
    status match {
      case 1 =>
        val new_future = thermo.past.reverse
        val new_expr = thermo.cur_expr
        val k = (v: A) => thermometer(new_expr.get, new_future :+ v)
        thermo.past = thermo.past :+ null
        throw Done[A](func(k))
      case 2 =>
        thermo.past = thermo.past :+ value
        value.nn
    }

  }
