object exercise{
  def sum(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) + acc)
    }

    loop(a, 0)
  }

  def sumCube: (Int, Int) => Int = sum(x => x * x * x)

  sumCube(1, 4)

  def product(f: Int => Int)(a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, f(a) * acc)
    }

    loop(a, 1)
  }

  def product2: (Int => Int) => (Int, Int) => Int = arithOp(_ * _, 1)

  product2(x => x)(1, 5)

  def factorial(n: Int): Int = product(x => x)(1, n)

  factorial(5)

  def arithOp(op: (Int, Int) => Int, acc: Int)
             (f: Int => Int)
             (a: Int, b: Int): Int = {
    def loop(a: Int, acc: Int): Int = {
      if (a > b) acc
      else loop(a + 1, op(f(a), acc))
    }

    loop(a, acc)
  }

  product2(x => x)(1, 5)
}