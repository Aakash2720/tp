object HOFsCurries extends App{
  def nTimes(f:Int=>Int ,n:Int,x:Int):Int={
    print(x)
    if(n<=0) x
    else nTimes(f,n-1,f(x+1))
  }
  def plusOne=(x:Int)=>x+1
  println(nTimes(plusOne,10,1))

  def nTimesBetter(f:Int=>Int,n:Int):(Int=>Int)={
      if(n<=0)(x:Int)=>x
      else (x:Int)=>nTimesBetter(f,n-1)(f(x))
  }
  val plus10=nTimesBetter(plusOne,10)
// def factorial(f:Int=>Int,n:Int,x:Int):Int={
//   if(n<=0) x
//   else factorial(f,n-1,f(x));
// }
//  def mulit(i:Int):Int={
//      if(i<=0)1
//      else i*mulit(i-1)
//  }
//  println(factorial(mulit,5,1))
  // Define the factorial function using the given approach
  def factorial(f: Int => Int, n: Int, x: Int): Int = {
    if (n <= 0) x
    else factorial(f, n - 1, f(x))
  }

  // Define a function to multiply a number by its previous number (i.e., calculate factorial)
  def multiplyByPrevious(i: Int): Int = {
    if (i <= 1) 1
    else i * multiplyByPrevious(i - 1)
  }

  // Example usage: Calculate factorial of 5
  val result = factorial((x: Int) => x + 1, 5, 1)
  println(s"Factorial of 5 is: $result")
  val suAd:Int=>(Int=>Int)=(x:Int)=>(y:Int)=>x+y
//  println(suAd(5)(6))
//  println(plus10(1))
}

