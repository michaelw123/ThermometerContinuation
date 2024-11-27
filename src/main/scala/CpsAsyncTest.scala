/*
import cps.*
import scala.concurrent.{Future, ExecutionContext}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Success, Failure}

object CpsContinuationMonad {
  given CpsAsyncMonad[Future] = cps.monads.FutureAsyncMonad

  case class CpsContinuationMonad[R, A](run: (A => Future[R]) => Future[R]) {
    def flatMap[B](f: A => CpsContinuationMonad[R, B])(implicit ec: ExecutionContext): CpsContinuationMonad[R, B] =
      CpsContinuationMonad { cont =>
        run { a =>
          f(a).run(cont)
        }
      }

    def map[B](f: A => B)(implicit ec: ExecutionContext): CpsContinuationMonad[R, B] =
      flatMap(a => CpsContinuationMonad.pure(f(a)))
  }

  object CpsContinuationMonad {
    def pure[R, A](value: A): CpsContinuationMonad[R, A] =
      CpsContinuationMonad(cont => cont(value))
  }

  def shift[R, A](f: (A => CpsContinuationMonad[R, Unit]) => CpsContinuationMonad[R, A])(implicit ec: ExecutionContext): CpsContinuationMonad[R, A] =
    CpsContinuationMonad { cont =>
      f(a => CpsContinuationMonad(_ => cont(a).map(_ => ()))) // Ensure the continuation type aligns
        .run(identity)
    }

  def reset[R, A](body: => CpsContinuationMonad[R, A])(implicit ec: ExecutionContext): Future[A] =
    body.run(a => Future.successful(a.asInstanceOf[R])).map(_.asInstanceOf[A])
}

 */


