import Thermometer.{reset, shift}
given thermoInstance: Thermometer[Int] = Thermometer[Int]


def test:Unit = {
  // 2*5 + 1 = 11
  val x = reset {
    2 * shift { (k: Int => Int) => 1 + k(5)
    }
  }
  println(x)
}

def test1:Unit = {
  // (1+1) * (1+2) * (1+3) = 24
  val x = reset {
    1 + shift{
      (k: Int =>Int) => k(1) * k(2) * k(3)
    }
  }
  println(x)
}

def test2: Unit = {
  // 1 + (3*(2+5)) = 22
  val x = 1 + reset {
    2 + shift {
      (k: Int => Int) => 3 * shift{(l: Int => Int) => l(k(5))}
    }
  }
  println(x)
}

def test3: Unit = {
  // 1+ 3*(2+10)=37
  val x = 1 + reset {
    2 + shift {
      (k: Int => Int) => 3 * shift { (l: Int => Int) => l(k(10)) }
    }
  }
  println(x)
}

def test4: Unit = {
  // 2*2*4 = 16
  val x = reset {
    2 * shift {
      (k: Int => Int) => k(k(4))
    }
  }
  println(x)
}

def test5: Unit = {
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
def test6: Unit = {
  //1 + 2*5 = 11
  val x = reset {
    2 * shift { (k: Int => Int) =>
      1 + k(5)
      }
  }
  println(x)
}

def test7: Unit = {
  //(1+4) +(2+3) +5 = 15
  val x = reset {
    1 + shift{
      (k: Int =>Int) =>
        reset {
          2 + shift{ (l: Int => Int) =>
            5 + l(3) + k(4)
            }
        }
    }
  }
  println(x)
}
def test8: Unit = {
  // 1 + (2*5) * (2*6) = 121
  val x = reset {
    2 * shift { (k: Int => Int) =>
      1+ k(5) * k(6)
      }
  }
  println(x)
}

def test9: Unit={
  //((1 + (2*5)) + 2+6  = 19
  val x = reset {
    2 *  shift{(k: Int => Int) => 1 +k(5)}  + shift{(k: Int => Int) => 2 +k(6)}
  }

  println(x)

}
@main def tests:Unit =
  test //11
  test1 //24
  test2 //22
  test3 //37
  test4 //16
  test5 //21
  test6  //11
  test7  //15
  test8  //121
  test9  //19
