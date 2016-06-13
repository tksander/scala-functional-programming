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
  def pascal(c: Int, r: Int): Int = {
    def pascalLoop(innerC: Int, innerR: Int, prevRow: Vector[Int], currRow: Vector[Int], targetRow: Int): Int = {
      if (innerR == targetRow) calcNum(c, r, prevRow)
      else {
        if (incrementRow(innerC, innerR)) {
          val incC = 0
          val incR = innerR + 1
          val newPrevRow = currRow :+ calcNum(innerC, innerR, prevRow)
          pascalLoop(incC, incR, newPrevRow, Vector.empty[Int], targetRow)
        } else {
          val incC = innerC + 1
          val currentRow = currRow :+ calcNum(innerC, innerR, prevRow)
          pascalLoop(incC, innerR, prevRow, currentRow, targetRow)
        }
      }
    }
    pascalLoop(0, 0, Vector.empty[Int], Vector.empty[Int], r)
  }

  def atEdge(c: Int, r: Int): Boolean = {
    c == 0 || c == r
  }

  def calcNum(c: Int, r: Int, row: Vector[Int]): Int = {
    if (atEdge(c, r)) 1 else row(c - 1) + row(c)
  }

  def incrementRow(c: Int, r: Int): Boolean = {
    c == r
  }

  /**
    * Exercise 2
    */
  def balance(chars: List[Char]): Boolean = {
    val stack = List()

    def loop(restList: List[Char], loopStack: List[Char]): Boolean = {
      if (restList.isEmpty) {
        true
      }

      else if (restList.head == '(') {
        val newStack = restList.head :: loopStack
        loop(restList.tail, newStack)
      } else if (restList.head == ')') {
        if (loopStack.isEmpty || unbalanced(loopStack.head)) false else loop(restList.tail, loopStack.tail)
      }
      else loop(restList.tail, loopStack)
    }
    loop(chars, stack)
  }

  def unbalanced(paren: Char): Boolean = paren != '('

  /**
    * Exercise 3
    */

  def countChange(money: Int, coins: List[Int]): Int = {
    def count(innerM: Int, innerC: List[Int]): Int = {
      if (innerC.isEmpty) 0
      else if (innerM - innerC.head == 0) 1
      else if (innerM - innerC.head < 0) 0
      else countChange(innerM - innerC.head, innerC) + countChange(innerM, innerC.tail)
    }
    count(money, coins.sorted)
  }
}

