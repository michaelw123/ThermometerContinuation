import scala.collection.mutable


class Thermometer[A]:
  var future = List.empty[Option[A]]
  var past = List.empty[Option[A]]
  var curExpr: Option[() =>A] = None
  var nest: mutable.Stack[(Option[() => A], List[Option[A]], List[Option[A]])] = mutable.Stack((curExpr, past, future))
  def popNest(): Unit =
    val x = nest.pop
    curExpr = x._1
    past = x._2
    future = x._3
  def newFuture(func: =>  () =>A, funcFuture: List[Option[A]]): Unit =
    nest.push((curExpr, past, future))
    past = List.empty[Option[A]]
    future = funcFuture
    curExpr = Some(func)

  def pushPast(p:Option[A]): Unit =  past = past :+ p

object Thermometer:
  final private case class Done[A](value: A) extends Exception
  def reset[A](func:  => A)(using thermo:Thermometer[A]): A = {
    thermometer(() =>func, List.empty[Option[A]])
  }

  private def thermometer[A](func: => () =>A, funcFuture: List[Option[A]])(using thermo:Thermometer[A]): A = {
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
      case None :: tail =>
        thermo.future = tail
        thermoDone(func)
      case Some(value) :: tail =>
        thermo.future = tail
        thermo.pushPast(Some(value))
        value

  private def thermoDone[A](func: => (A =>A) => A)(using thermo:Thermometer[A]): A =
    val newFuture = thermo.past.reverse
    val newExpr = thermo.curExpr.orNull
    val k = (v: A) => thermometer(newExpr, newFuture :+ Some(v))
    thermo.pushPast(None)
    throw Done[A](func(k))
