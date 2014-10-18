object fixed_points {

  import math.abs
  val tolerance = 0.00001
  def is_good_enough(guess: Double, value: Double): Boolean = {
    abs((guess - value) / value) < tolerance
  }


  def fixedPoint(fun: Double => Double)(initial_value: Double): Double = {
    def iterate(guess: Double): Double = {
      val next = fun(guess)
      if (is_good_enough(guess, next)) next
      else {
        val improved_guess = (guess + next) / 2
        iterate(improved_guess)
      }
    }

    iterate(initial_value)
  }

  def sqrt(x: Double) =
    fixedPoint(y => x / y)(1.0)
  sqrt(9)
}