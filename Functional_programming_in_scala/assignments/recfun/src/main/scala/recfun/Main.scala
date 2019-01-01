package recfun

object Main {
  def main(args: Array[String]) {
    /*println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }*/
    balance("(if (zero? x) max (/ 1 x))".toList)
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = {
      if(c == 0 || c == r) 1
      else pascal(c - 1, r - 1) + pascal(c, r - 1)
    }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
      def balanceIter(chars: List[Char], balance: Int):Boolean = {
        if(balance < 0) false
        else if(chars.isEmpty && balance == 0) true
        else {
          if(chars.isEmpty) false
          else if(chars.head == '(') balanceIter(chars.tail, balance + 1)
          else if(chars.head == ')') balanceIter(chars.tail, balance - 1)
          else balanceIter(chars.tail, balance)
        }
      }
      balanceIter(chars, 0)
    }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = {
      if(money < 0 || coins.isEmpty) 0
      else if(money == 0) 1
      else countChange(money - coins.head, coins) + countChange(money, coins.tail)
    }
  }
