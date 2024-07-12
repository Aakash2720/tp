package advance

object curriedFunction extends App {
  val simpleAddFunction=(x:Int,y:Int)=>x+y
  def simpleAddMethod(x:Int,y:Int)=x+y
  def curriedAddMethod(x:Int)(y:Int)=x+y
  val add7 = simpleAddFunction(_,7);
  val add8=simpleAddMethod(_,_)
  println(add8(3))
  println(add7(6))

}
