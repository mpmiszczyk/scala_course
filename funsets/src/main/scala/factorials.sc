object factorials {
  def product(fun: Int => Int)(a: Int, b: Int): Int = {
    if (a > b) 1
    else fun(a) * product(fun)(a + 1, b)
  }

  product(x => x * x)(3, 4)

  def product_tail(fun: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, acc * fun(a))
    }
    loop(a, 1)
  }

  product_tail(x => x *x) (3, 4)

  def factorial(n: Int) = product_tail(x => x)(1, n)

  factorial(5)

  def mapReduce(combine: (Int, Int) => Int, zero: Int)
               (fun: Int=>Int)
               ( a:Int, b: Int):Int = {
    def loop (a: Int, acc: Int): Int = {
      if (a>b) acc
      else loop(a+1, combine(acc, fun(a)))
    }
    loop(a, zero)
  }

  def funProduct = mapReduce((x,y) => x*y, 1) _

  funProduct(x => x *x)(3, 4)

  //
  //  def factorial = product( x => x)
  //
  //  factorial(1, 3)
}