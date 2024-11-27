
import ThermometerConti.{reset, shift}
object RandomTest extends App{
  implicit val aThemometer:ThermometerConti[Int] = ThermometerConti[Int]()

  val x = reset[Int]{
    () => 2 * shift[Int] {
      (k: Int => Int) => 1 + k(2)
    }
  }

}
