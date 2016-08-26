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
			if(c==0||c==r) 1 else pascal(c-1,r-1)+pascal(c,r-1)
	}

	/**
	 * Exercise 2
	 */
	def balance(chars: List[Char]): Boolean = {
	  def  doBalance(chs : List[Char], count : Int, size : Int) : Boolean ={
	    if(chs.isEmpty) return count == 0;
	    if (count<0) return false;
	    if(chs.head=='(') return doBalance(chs.tail,count+1,size);
	    else if (chs.head==')') return doBalance(chs.tail,count-1,size);
	    else return doBalance(chs.tail,count,size);
	    
	  }
	  
	  doBalance(chars,0,chars.size);
	}

	/**
	 * Exercise 3
	 */
	def countChange(money: Int, coins: List[Int]): Int = {
			def reduce(money: Int, coins: List[Int], accCounter: Int): Int = {
					if(money == 0) accCounter + 1
					else if(money < 0 || coins.isEmpty) accCounter
					else reduce(money - coins.head, coins, accCounter) + reduce(money, coins.tail, accCounter)
			}

			if(money <= 0 || coins.isEmpty) 0
			else reduce(money, coins, 0)
	}
}
