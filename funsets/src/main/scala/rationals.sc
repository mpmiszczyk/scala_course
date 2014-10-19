object rationals{
  val x = new Rational(1,3)
  val y = new Rational(5,7)
  val z = new Rational(3,2)

  x + y

  x - y - z

  z max x

  new Rational(3)

}

class Rational (x: Int, y: Int) {
  require(y != 0, "demoninator must be non zero")

  val numerator = x / gcd(x,y)
  val denominator =  y / gcd(x,y)

  def this(x: Int) = this(x, 1)

  private def gcd(x: Int, y: Int): Int = {
    if (y == 0) x
    else gcd(y, x % y)
  }

  override def toString =
    numerator + "/"  + denominator

  def + (that: Rational): Rational =
    new Rational(
      (numerator * that.denominator) + (that.numerator * denominator),
      denominator * that.denominator )

  def unary_- : Rational =
    new Rational(-numerator, denominator)

  def - (that: Rational): Rational =
    this + - that

  def < (that: Rational): Boolean =
    numerator * that.denominator < that.numerator * denominator

  def * (that: Rational): Rational =
    new Rational(
      numerator * that.numerator,
      denominator * that.denominator)

  def max(that: Rational): Rational =
    if ( this < that) that
    else this
}
