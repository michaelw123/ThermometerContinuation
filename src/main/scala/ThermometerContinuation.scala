import scala.collection.mutable
import scala.collection.mutable.Stack
class Thermometer[A] {
  final case class Done(value: A) extends Exception

  var future = List[A | Null]()
  var past = List[A | Null]()
  var cur_expr: Option[() => A] = None
  var nest: mutable.Stack[(Option[() => A], List[A | Null], List[A | Null])] = Stack()
  def reset(func:  () => A): A = {
    thermometer(func, List[A | Null]())
  }

  def thermometer(func: => () =>A, fn_future: List[A | Null]): A = {
    nest.push((cur_expr, past, future))
    past = List[A]()
    future = fn_future
    cur_expr = Option(func)
    def run(): A = {
      try {
        func()
      } catch {
        case e: Done => e.value
      }
    }

    val result = run()
    val x = nest.pop
    cur_expr = x._1
    past = x._2
    future = x._3
    result
  }

  def shift(func: => (A =>A) => A): A= {
    val status = if (future.isEmpty) 1 else {
     if (future.head == null) 1 else 2
    }
    status match {
      case 1 =>
        val new_future = past.reverse
        val k = (v: A) => thermometer(cur_expr.get, new_future :+ v)
        past = past :+ null
        future = if (future.isEmpty) future else future.tail
        val result = func(k)
        throw Done(result)
      case 2 =>
        val value = future.head
        past = past :+ value
        future = future.tail
        value.asInstanceOf[A]
    }

  }
}

object Thermometer {
  inline def apply[A](): Thermometer[A] = new Thermometer[A]()
  def testThermometer = println("Hello")

}

object ThermometerContinuation extends App {
  def test = {
    //lambda: 2 * shift(lambda k: 1 +  k(5))
    // 2*5 + 1 = 11
    val thermometer = Thermometer[Int]()
    val x = thermometer.reset {() =>
      2 * thermometer.shift { (k: Int => Int) => 1 + k(5)
      }
    }
    println(x)
  }
  def test1 = {
    //lambda: 1 + shift(lambda k: k(1) * k(2) * k(3))
    // (1+1) * (1+2) * (1+3) = 24
    val thermometer = Thermometer[Int]()

    val x = thermometer.reset { () =>
      1 + thermometer.shift{
        (k: Int =>Int) => k(1) * k(2) * k(3)
      }
    }
    println(x)
  }

  def test3 = {
    //1+ reset(lambda: 2 + shift(lambda k:3 * shift(lambda l: l(k(5)))))
    // 1 + (3*(2+5)) = 22
    val thermometer = Thermometer[Int]()

    val x = 1 + thermometer.reset { () =>
      2 + thermometer.shift {
        (k: Int => Int) => 3 * thermometer.shift{(l: Int => Int) => l(k(5))}
      }
    }
    println(x)
  }

  def test4 = {
    //1 + reset(lambda: 2 + shift(lambda k:3 * shift(lambda l: l(k(10)))))
    // 1+ 3*(2+10)=37
    val thermometer = Thermometer[Int]()

    val x = 1 + thermometer.reset { () =>
      2 + thermometer.shift {
        (k: Int => Int) => 3 * thermometer.shift { (l: Int => Int) => l(k(10)) }
      }
    }
    println(x)
  }

  def test5 = {
    //reset(lambda: 2 * shift(lambda k: k(k(4))))
    // 2*4*4 = 16
    val thermometer = Thermometer[Int]()

    val x = thermometer.reset { () =>
      2 * thermometer.shift {
        (k: Int => Int) => k(k(4))
      }
    }
    println(x)
  }

  def test6 = {
    //reset(lambda:1 + shift(lambda k: reset(lambda: 2 + shift(lambda l:l(3) + k(4)))))
    // (2+3) + (2+5) (1+4) = 17
    val thermometer = Thermometer[Int]()

    val x = thermometer.reset { () =>
      1 + thermometer.shift {
        (k: Int => Int) => thermometer.reset{
          () => {
            2 * thermometer.shift{
              (l: Int => Int) => l(3) + l(5) + k(4)
            }
          }
        }
      }
    }
    println(x)
  }
def test7 = {
  val thermometer = Thermometer[Int]()
  val x = thermometer.reset {() =>
    2 * thermometer.shift { (k: Int => Int) =>
      1 + k(5)
    }
  }
  println(x)
}

  def test8 = {
    val thermometer1 = Thermometer[Int]()
    val thermometer2 = Thermometer[Int]()
    val x = thermometer1.reset { ()=>
      1 + thermometer1.shift{
        (k: Int =>Int) =>
          thermometer2.reset {() =>
            2 + thermometer2.shift{ (l: Int => Int) =>
              5 + l(3) + k(4)
            }
          }
      }
    }
    println(x)
  }
  def test9 = {
    import Thermometer.{testThermometer}
    val thermometer = Thermometer[Int]()
    val x = thermometer.reset {() =>
      2 * thermometer.shift { (k: Int => Int) =>
        1+ k(5) + k(6)
      }
    }
    println(x)
    testThermometer
  }
  //test
  //test1
  //test3
  //test4
  //test5
  //test6
  //test7
  test9
}

