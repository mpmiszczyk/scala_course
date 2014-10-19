object rationals{
  val x = new Rational(1,3)
  val y = new Rational(5,7)
  val z = new Rational(3,2)

  x.add(y)

  x.substract(y).substract(z)

  y.add(y)

  val strange = new Rational(3,0)

}


class Rational (x: Int, y: Int) {
  require(y != 0, "demoninator must be non zero")

  val numerator = x / gcd(x,y)
  val denominator =  y / gcd(x,y)

  override def toString =
    numerator + "/"  + denominator
  def add(that: Rational): Rational =
    new Rational(
      (numerator * that.denominator) + (that.numerator * denominator),
      denominator * that.denominator )

  def neg: Rational =
    new Rational(-numerator, denominator)

  def substract(that: Rational): Rational =
    add(that.neg)

  private def gcd(x: Int, y: Int): Int = {
    if (y == 0) x
    else gcd(y, x % y)
  }

  def less(that: Rational): Boolean =
    numerator * that.denominator < that.numerator * denominator

  def max(that: Rational): Rational =
    if (less(that)) that
    else this
}
