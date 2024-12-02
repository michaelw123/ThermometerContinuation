import scala.collection.mutable


class Thermometer[A]:
  var future = List.empty[A | Null]
  var past = List.empty[A | Null]
  var curExpr: Option[() =>A] = None
  var nest: mutable.Stack[(Option[() => A], List[A | Null], List[A | Null])] = mutable.Stack((curExpr, past, future))
  def popNest(): Unit =
    val x = nest.pop
    curExpr = x._1
    past = x._2
    future = x._3
  def newFuture(func: =>  () =>A, funcFuture: List[A | Null]): Unit =
    nest.push((curExpr, past, future))
    past = List.empty[A | Null]
    future = funcFuture
    curExpr = Some(func)

  def pushPast(p:A | Null): Unit =  past = past :+ p

object Thermometer:
  final private case class Done[A](value: A) extends Exception
  def reset[A](func:  => A)(using thermo:Thermometer[A]): A = {
    thermometer(() =>func, List.empty[A | Null])
  }

  private def thermometer[A](func: => () =>A, funcFuture: List[A | Null])(using thermo:Thermometer[A]): A = {
    thermo.newFuture(func, funcFuture)
    def run(): A = try {
        func()
      } catch {
        case Done(e) => e.asInstanceOf[A]
      }

    val result = run()
    thermo.popNest()
    result
  }

  def shift[A](func: => (A =>A) => A)(using thermo:Thermometer[A]): A= thermo.future match
      case Nil => thermoDone(func)
      case null :: tail =>
        thermo.future = tail
        thermoDone(func)
      case value :: tail =>
        thermo.pushPast(value)
        value.nn

  private def thermoDone[A](func: => (A =>A) => A)(using thermo:Thermometer[A]): A =
    val newFuture = thermo.past.reverse
    val newExpr = thermo.curExpr.orNull
    val k = (v: A) => thermometer(newExpr, newFuture :+ v)
    thermo.pushPast(null)
    throw Done[A](func(k))
