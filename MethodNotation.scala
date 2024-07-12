object MethodNotation extends App{
//  val person=new Person("John");
//  val per2=new Person("aakash");
//  println(person .+( per2))
//  perfix notations
//    println(-1==1.unary_-)
//  val person=new Person("John");
//  val new_per=person + "aakash";
//  println(new_per getName)
  val mary=new Count(0)

  println((+mary).print())

}
//class Person(val name:String){
//    def getName():String=name;
//    def +(person:Person):String= s"${this.name} is hangind out with ${person.name}"
//    def +(name:String):Person={
//      new Person(s"Marry ${name}")
//    }
//
//}
class Count(val count:Int){
  def unary_+ : Count={
    new Count(count+1)
  }
//  println(count)
  def print():Unit={
    println(count)
  }
}
