object ExceptionHandling extends App{
  class OverFlowException extends RuntimeException("over flow exception")
  class UnderException extends RuntimeException("under flow exception")
  class MathCalculationException extends RuntimeException("Division by 0")
  object PocketCalculator{
    def add(x:Int,y:Int)={
      val result=x+y;
      if(x>0 && y>0 && result<0) throw new OverFlowException
      else if(x<0 && y<0 && result >0) throw new UnderException
      else result;
    }
    def subtract(x:Int,y:Int)={
     val result=x-y
      if(x>0 && y<0 && result<0)throw new OverFlowException
      else if(x<0 && y>0 && result>0)throw new UnderException
      else result
    }
    def multiply(x:Int,y:Int)={
      val result=x*y
      if(x>0 && y>0 && result<0) throw new OverFlowException
      else if(x<0 && y <0&& result<0) throw new OverFlowException
      else if(x<0 && y>0 && result>0) throw new UnderException
      else if(x>0 && y<0 && result>0) throw new UnderException
      else result
    }
    def divide(x:Int,y:Int)={
      if(y==0) throw new MathCalculationException
      else x/y

    }

}
  try{
    println(PocketCalculator.add(Int.MaxValue,10))
  }
  catch {
    case e:OverFlowException =>println(e.getMessage)
    case e:UnderException =>println(e.getMessage)
    case e:MathCalculationException =>println(e.getMessage)
  }
}
