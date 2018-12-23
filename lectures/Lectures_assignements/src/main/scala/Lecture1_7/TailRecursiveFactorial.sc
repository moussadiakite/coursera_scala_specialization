object TailRecursiveFactorial{
  def factorial(n: Int): Int = {
    def factorialIter(n: Int, product: Int): Int = {
      if(n == 0) product
      else factorialIter(n-1, n * product)
    }
    factorialIter(n, 1)
  }

  factorial(5)
}