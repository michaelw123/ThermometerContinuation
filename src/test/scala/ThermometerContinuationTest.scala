import Thermometer.{reset, shift}
implicit val thermoInstance:Thermometer[Int] = Thermometer[Int]()

object ThermometerTest extends App {
    def test = {
      //lambda: 2 * shift(lambda k: 1 +  k(5))
      // 2*5 + 1 = 11

      val x = reset {() =>
        2 * shift { (k: Int => Int) => 1 + k(5)
        }
      }
      println(x)
    }
    def test1 = {
      //lambda: 1 + shift(lambda k: k(1) * k(2) * k(3))
      // (1+1) * (1+2) * (1+3) = 24

      val x = reset { () =>
        1 + shift{
          (k: Int =>Int) => k(1) * k(2) * k(3)
        }
      }
      println(x)
    }

    def test3 = {
      //1+ reset(lambda: 2 + shift(lambda k:3 * shift(lambda l: l(k(5)))))
      // 1 + (3*(2+5)) = 22

      val x = 1 + reset { () =>
        2 + shift {
          (k: Int => Int) => 3 * shift{(l: Int => Int) => l(k(5))}
        }
      }
      println(x)
    }

    def test4 = {
      //1 + reset(lambda: 2 + shift(lambda k:3 * shift(lambda l: l(k(10)))))
      // 1+ 3*(2+10)=37

      val x = 1 + reset { () =>
        2 + shift {
          (k: Int => Int) => 3 * shift { (l: Int => Int) => l(k(10)) }
        }
      }
      println(x)
    }

    def test5 = {
      //reset(lambda: 2 * shift(lambda k: k(k(4))))
      // 2*4*4 = 16

      val x = reset { () =>
        2 * shift {
          (k: Int => Int) => k(k(4))
        }
      }
      println(x)
    }

    def test6 = {
      //reset(lambda:1 + shift(lambda k: reset(lambda: 2 + shift(lambda l:l(3) + k(4)))))
      // (2+3) + (2+5) (1+4) = 17

      val x = reset { () =>
        1 + shift {
          (k: Int => Int) => reset{
            () => {
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
      val x = reset {() =>
        2 * shift { (k: Int => Int) =>
          1 + k(5)
        }
      }
      println(x)
    }

    def test8 = {
      val x = reset { ()=>
        1 + shift{
          (k: Int =>Int) =>
            reset {() =>
              2 + shift{ (l: Int => Int) =>
                5 + l(3) + k(4)
              }
            }
        }
      }
      println(x)
    }
    def test9 = {
      val x = reset {() =>
        2 * shift { (k: Int => Int) =>
          1+ k(5) + k(6)
        }
      }
      println(x)
    }
    test
    test1
    test3
    test4
    test5
    //test6
    test7
    test8
    test9
  }

