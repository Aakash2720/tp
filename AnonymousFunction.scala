object AnonymousFunction extends App{
  val double=(x:Int)=>x*2

  println(double(4))

  val supperAdder=new Function1[Int,Function1[Int,Int]]{
    override def apply(x:Int):Function1[Int,Int]=new Function1[Int,Int]{
      override def apply(y:Int):Int=x+y;
    }}
//  println(supperAdder.apply(2)(3))
//  val supperAd :(Int,Function1[Int,Function1[Int,Int]]):(x:Int,Function1[Int,Int])=>Int=;
val supAd = (k: Int) => (l: Int) => k+l
  println(supAd(3)(3))

  val odd=(x:Int)=>x%2==1
  println(odd(8 ))
}

