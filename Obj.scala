object Obj extends App{
//  val person=new Person("aman",20,"male")
//  person.getPerson()
//    val writer=new Writer("aakash","vishwakarma")
//    println(writer.fullName())
    val counter=new Counter(0)
    counter.inc(10).print;

}
//class Person(name:String,age:Int)
//{
//  var gender: String="" ;
//   def getPerson(): Unit = {
//     println(s"person name is ${name} and age is ${age} and gender is ${gender}")
//   }
//  def this(name:String,age:Int,gender:String) = {
//    this(name,age)
//    this.gender=gender
//  }
//}

//class Writer(first_name:String,last_name:String){
//
//    def fullName():String={
//          return s"${this.first_name} ${this.last_name}";
//  }
//}
// class Novel(name:String,yearOfRelease:Int,writer:Writer){
//
//   def getNovel():Unit = {
//     var writer:Writer=new Writer(writer.first_name,writer.last_name)
//     var name:String=name
//     var yearOfRelease:Int=yearOfRelease
//   }
//
class Counter(val count:Int) {

  def inc = {
    println("Incrementing")
    new Counter(count + 1)
  }

  def dec = {
    println("Decrementing")
    new Counter(count - 1)
  }

  def inc(n: Int): Counter = {
    if (n <= 0) this
    else inc.inc(n - 1)
  }

  def dec(n: Int): Counter = {
    if (n <= 0) this
    else dec.dec(n - 1)
  }
  def print()=println(count)

}