object MyModule {

  def abs(n:Int): Int = 
    if(n < 0) -n
    else n
    
  private def formatAbs(x: Int) = {
    val msg = "The absolute value of %d is %d"
    msg.format(x, abs(x))
  }
  
  def factorial(n:Int): Int = {
    @annotation.tailrec
    def go(n:Int, acc:Int): Int =
      if(n <= 0) acc
      else go(n-1, n*acc)
      
    go(n, 1)
  }
  
  def fib(position: Int): Int = {
    
    @annotation.tailrec
    def go(position: Int, currentSum: Int, previousSum: Int): Int ={
      if(position == 0) currentSum
      else if(position == 1) previousSum
      else go(position-1, previousSum, currentSum + previousSum)
    }
      
    go(position, 0, 1)
  }
  
  def main(args: Array[String]): Unit = {
    println(formatAbs(-42))
    println(fib(4))
  }
}