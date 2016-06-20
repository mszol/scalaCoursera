package recfun

object Main {
  def main(args: Array[String]) {
    println("Pascal's Triangle")
    for (row <- 0 to 10) {
      for (col <- 0 to row)
        print(pascal(col, row) + " ")
      println()
    }
  }

  /**
   * Exercise 1
   */
    def pascal(c: Int, r: Int): Int = c match{
    case 0 => 1
    case `r` => 1
    case _ => pascal (c-1, r-1)+pascal(c,r-1)
  }
  
  /**
   * Exercise 2
   */
    def balance(chars: List[Char]): Boolean = {
    def countParentheses(open: Int, close: Int, chars: List[Char]): Boolean = chars match {
      case Nil => if (open == close) true else false
      case ')'::_ => if (open>close) countParentheses(open, close + 1, chars.tail) else false
      case '('::_ => countParentheses(open+1, close, chars.tail)
      case _ => countParentheses(open, close, chars.tail)
    }
    countParentheses(0, 0, chars)
  }
  
  /**
   * Exercise 3
   */
    def countChange(money: Int, coins: List[Int]): Int = money match{
        case 0 => 1
        case _ => if(money <0 || coins.isEmpty) 0 else countChange(money, coins.tail) + countChange(money-coins.head, coins)
      }
  }
