import scala.collection.mutable
import scala.quoted._

class ContinuationState:
  var future = List.empty[Option[Any]]
  var past = List.empty[Option[Any]]
  var curExpr: Option[() =>Any] = None

  var recording: mutable.Stack[(Option[() => Any], List[Option[Any]], List[Option[Any]])] = mutable.Stack((curExpr, past, future))
  def popRecording(): Unit =
    val x = recording.pop
    curExpr = x._1
    past = x._2
    future = x._3
  def pushRecording(func: =>  () =>Any, funcFuture: List[Option[Any]]): Unit =
    recording.push((curExpr, past, future))
    past = List.empty[Option[Any]]
    future = funcFuture
    curExpr = Some(func)

  def pushPast(p:Option[Any]): Unit =  past = past :+ p

object Thermometer:
  final private case class Done[A](value: A) extends Exception
  def reset[A](func:  => A)(using state:ContinuationState): A = {
    thermometer(() =>func, List.empty[Option[A]])
  }

  private def thermometer[A](func: => () =>A, funcFuture: List[Option[A]])(using state:ContinuationState): A = {
    state.pushRecording(func, funcFuture)
    def run(): A = try {
        func()
      } catch {
        case Done(e) => e.asInstanceOf[A]
      }

    val result = run()
    state.popRecording()
    result
  }

  def shift[A](func: => (A =>A) => A)(using state:ContinuationState): A= state.future match
      case Nil => thermoDone(func)
      case None :: tail =>
        state.future = tail
        thermoDone(func)
      case Some(value) :: tail =>
        state.future = tail
        state.pushPast(Some(value))
        value.asInstanceOf[A]

  private def thermoDone[A](func: => (A =>A) => A)(using state:ContinuationState): A =
    val newFuture = state.past.reverse
    val newExpr = state.curExpr.orNull
    val k = (v: A) => thermometer(newExpr, newFuture :+ Some(v)).asInstanceOf[A]
    state.pushPast(None)

    throw Done[A](func(k))
