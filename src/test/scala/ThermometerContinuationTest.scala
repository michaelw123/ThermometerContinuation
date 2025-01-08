import Thermometer.{reset, shift}
given IntState: ContinuationState[Int] = ContinuationState[Int]
given stringState: ContinuationState[String] = ContinuationState[String]


def test1:Unit = {
  // 2*5 + 1 = 11
  val x = reset {
    2 * shift { (k: Int => Int) => 1 + k(5)
    }
  }
  println(x)
}

def test2:Unit = {
  // (1+1) * (1+2) * (1+3) = 24
  val x = reset {
    1 + shift{
      (k: Int =>Int) => k(1) * k(2) * k(3)
    }
  }
  println(x)
}

def test3: Unit = {
  // 1 + (3*(2+5)) = 22
  val x = 1 + reset {
    2 + shift {
      (k: Int => Int) => 3 * shift{(l: Int => Int) => l(k(5))}
    }
  }
  println(x)
}
def test4: Unit={
  //2 + (((1 + (2*5)) +6)  = 19
  val x = reset {
    2 *  shift{(k: Int => Int) => 1 +k(5)}  + shift{(k: Int => Int) => 2 +k(6)}
  }

  println(x)

}

def test5:Unit = {
  // 1+2*2*5 = 21
  val x = reset {
    2 * shift { (k: Int => Int) => 1 + k(k(5)) }
  }
  println(x)
}
def test6: Unit = {
  // (2*3) + (2*5) + (1+4) = 21
  val x = reset {
    1 + shift {
      (k: Int => Int) => reset{
        {
          2 * shift{
            (l: Int => Int) => l(3) + l(5) + k(4)
          }
        }
      }
    }
  }
  println(x)
}
def test7={
  //2+3=5
  val x = reset{
    1 + shift{(k: Int => Int) => 2+3}
  }
  println(x)
}

def test8={
  //b+("a"+"c") = "bac"
  val x = reset{
    "a" + shift{(k: String => String) => "b" + k("c")}
  }
  println(x)
}

@main def tests:Unit =
  test1 //11
  test2 //24
  test3 //22
  test4  //19
  test5  //21
  test6 //21
  test7 //5
  test8  //bac

