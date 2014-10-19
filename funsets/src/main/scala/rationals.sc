object rationals{
  val x = new Rational(1,2)

  val y = new Rational(3,7)

  x.add(y)


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

}
