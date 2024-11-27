import cps.*

case class Continuation[R, A](run: (A => R) => R) {
  def flatMap[B](f: A => Continuation[R, B]): Continuation[R, B] =
    Continuation(r => run(a => f(a).run(r)))

  def map[B](f: A => B): Continuation[R, B] =
    flatMap(a => Continuation.pure(f(a)))
}

object Continuation {
  def pure[R, A](value: A): Continuation[R, A] =
    Continuation(k => k(value))
}

object ContinuationExample extends App {

  def addCPS(a: Int, b: Int): Continuation[String, Int] =
    Continuation { cont =>
      val result = a + b
      cont(result)
    }

  val continuation = for {
    sum <- addCPS(2, 3)
  } yield s"The sum is $sum"

  val result = continuation.run(identity)
  println(result)  // Output: The sum is 5
}
