import Thermometer.{reset, shift}
given thermoInstance:Thermometer[Int] = Thermometer[Int]()


def test = {
  // 2*5 + 1 = 11
  val x = reset {
    2 * shift { (k: Int => Int) => 1 + k(5)
    }
  }
  println(x)
}

def test1 = {
  // (1+1) * (1+2) * (1+3) = 24
  val x = reset {
    1 + shift{
      (k: Int =>Int) => k(1) * k(2) * k(3)
    }
  }
  println(x)
}

def test3 = {
  // 1 + (3*(2+5)) = 22
  val x = 1 + reset {
    2 + shift {
      (k: Int => Int) => 3 * shift{(l: Int => Int) => l(k(5))}
    }
  }
  println(x)
}

def test4 = {
  // 1+ 3*(2+10)=37
  val x = 1 + reset {
    2 + shift {
      (k: Int => Int) => 3 * shift { (l: Int => Int) => l(k(10)) }
    }
  }
  println(x)
}

def test5 = {
  // 2*2*4 = 16
  val x = reset {
    2 * shift {
      (k: Int => Int) => k(k(4))
    }
  }
  println(x)
}

def test6 = {
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
def test7 = {
  val x = reset {
    2 * shift { (k: Int => Int) =>
      1 + k(5)
      }
  }
  println(x)
}

def test8 = {
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
def test9 = {
  // 1 + (2*5) * (2*6) = 121
  val x = reset {
    2 * shift { (k: Int => Int) =>
      1+ k(5) * k(6)
      }
  }
  println(x)
}

def test10={
  //2 + ((1 + (2+5)) +6)  = 16
  val x = reset {
    2 *  shift{(k: Int => Int) => 1 +k(5)} + shift{(k: Int => Int) => 2 +k(6)}
  }
  println(x)
}
@main def tests =
  test8
  //test1
