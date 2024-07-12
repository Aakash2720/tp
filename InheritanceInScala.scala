object InheritanceInScala extends App{
  // sealed class is use to prevent the extension of class in other files
//  class Animal{
//    def eat:Unit = println("Animal eat")
//    def sleep:Unit = println("Animal sleep")
//    def speak:Unit = println("Animal speak")
//  }
//  class Dog extends Animal{
//    override def eat:Unit = println("Dog eat")
//    override def sleep:Unit = println("Dog sleep")
//    override def speak:Unit = println("bhaw bhaw")
//    def jump:Unit = println("Dog jump")
//
//  }
//  val dog:Animal = new Dog();
//  dog.eat;
//  dog.sleep;
//  dog.speak;

  //exercise


  abstract class MyList{
    def head:Int;
    def tail:MyList;
    def isEmpty:Boolean;
    def add(x:Int):MyList;
    def printElement:String;

    override def toString:String="["+printElement+"]";
  }
  object Empty extends MyList{
    def head:Int=throw new NoSuchElementException;
    def tail:MyList=throw new NoSuchElementException;
    def isEmpty:Boolean=true;
    def add(x:Int):MyList=new Cons(x,Empty);
    def printElement:String="";


  }
  class Cons(h:Int,t:MyList) extends MyList{
    def head:Int=h;
    def tail:MyList=t;
    def isEmpty:Boolean=false;
    def add(x:Int):MyList=new Cons(x,this);
    def printElement:String=if(t.isEmpty)""+h else h+" "+t.printElement;
  }
  val list=new Cons(1,new Cons(2,new Cons(3,Empty)))

  print(list.toString)

}
