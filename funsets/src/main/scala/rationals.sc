object rationals{
  val x = new Rational(1,3)
  val y = new Rational(5,7)
  val z = new Rational(3,2)

  x.add(y)

  x.substract(y).substract(z)


}


class Rational (x: Int, y: Int) {
  def numerator = x
  def denominator =  y

  override def toString =
    numerator + "/"  + denominator

  def add(that: Rational): Rational =
    new Rational(
      (numerator * that.denominator) + (that.numerator * denominator),
      denominator * that.denominator )

  def neg() =
    new Rational(-numerator, denominator)

  def substract(that: Rational): Rational =
    add(that.neg())

}
