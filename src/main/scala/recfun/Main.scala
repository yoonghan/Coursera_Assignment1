package recfun
import common._

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
    if(c == 0 || r==c){
      1	//return 1
    }else{
      pascal(c-1,r-1) + pascal(c,r-1); //return previous row's left + previous row right in a triangle
    }    
  }

  /**
   * Exercise 2
   * Loop through all the characters, when ( is found +1, when ) is found -1.
   * When total is negative, immediately exit.
   * Compare the last value and see if it is 0, to find all opening and closing brackets are fulfilled.
   */
  def balance(chars: List[Char]): Boolean = {
	
    def loopCharacters(charList:List[Char],counter:Int):Int = {
      if(charList.isEmpty){
	    counter
	  }else{
	      val headChar = charList.head
		  val tailList = if(charList.size >1) charList.tail else Nil
		  
		  if(headChar == '('){
		    loopCharacters(tailList, counter+1) 
		  }else if(headChar == ')'){
		    if(counter - 1 < 0)
		      counter-1 //immediately return negative when no opening brackets are found.
		    else
		      loopCharacters(tailList, counter-1)  
		  }else{
		    loopCharacters(tailList, counter) 
		  }
	  }
    }
	
	loopCharacters(chars,0) == 0;
  }
  
  /**
   * Exercise 3
   * 1) Go through all the available coins.
   * 2) Check how many steps/times the same coin is needed. Check if the sum can be achieved, if yes consider as success.
   * 3) If there are next in list, do less step and go through the list - 1. Small Count makes step-1 with the remaining balance and use the remaining lists.
   * 4) Go through the remaining list with same steps as 2 and 3.
   */
  def countChange(money: Int, coins: List[Int]): Int = {
		def sumAllChanges(change:Int, sumChange:Int, steps:Int):Int = {
			if(sumChange < money){
				sumAllChanges(change, sumChange+change, steps+1)
			}else{
				steps
			} 
		}
		
		def smallCount(coinExchanges:List[Int], steps:Int, change:Int, successTime:Int):Int = {
		  if(steps <= 1){
		    successTime
		  }else{
		    val newChange = (steps-1)*change
		    val totalSuccessTime = countChange(money-newChange, coinExchanges)
		    smallCount(coinExchanges, steps-1, change, totalSuccessTime+successTime);
		  }
		}
		
		def checkStepsAchieved(steps:Int, popCoin:Int):Int = {
		  if(steps*popCoin == money) 1 else 0 
		}
		
		def coinChange(coinExchange:List[Int], successTime:Int):Int = {
		  if(!coinExchange.isEmpty){
		    
			  val popCoin = coinExchange.head
			  val steps = sumAllChanges(popCoin, 0,0)
			  val sumStepsSuccessful = checkStepsAchieved(steps, popCoin)
			  if(coinExchange.size > 1){
			    successTime + smallCount(coinExchange.tail, steps, popCoin, 0) + coinChange(coinExchange.tail, successTime)+sumStepsSuccessful 
			  }else{
			    sumStepsSuccessful
			  }
		  }else{
		    successTime
		  }
		}
		
		coinChange(coins, 0)
  	}
}
