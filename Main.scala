import scala.annotation.tailrec
import scala.math._
object Main extends App{
    val x:Int=43
    val new_x=if(x==43) 34 else 1
    "hello World"
    val someValue={
        2<3
    }
    val someOtherValue={
        if(someValue) 239 else 986
        43
    }
    def aRepeatedFunction(str :String,n:Int): String={
        if(n==1) str;
        else str+" "+aRepeatedFunction(str ,n-1)
    }
    def greeting(name:String,n:Int)=
        "Hii ,my name is "+name+" and In am "+n+" years old"



//        if(n==1)1
//        else n*factorial(n-1)
        @tailrec
        def fact(i:Int ,acc:Int):Int={
            if(i<=1)acc
            else fact(i-1,acc*i)
        }
    println(fact(5,1))


    def fibbonacci(n:Int):Int={
        if(n==1 || n==2)1
        else fibbonacci(n-1)+fibbonacci(n-2)
    }
    @tailrec
    def isPrime(n:Int,m:Int):Boolean={
        if(sqrt(n)>m){
            if(n%m==0)false else isPrime(n,m+1);

        }
        else true
    }
//    println(isPrime(50,2))
//    println(factorial(5000))
}