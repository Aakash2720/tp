object GenericInScala extends App{
//    //variance problem
//  class Animal
//  class Dog extends Animal
//  class Cat extends Animal
//  class Bird extends Animal
//
//  //1. yes,List[Cat] extends List[Animal]=covariance
//  class CovariantList[+A]
//  val animalList:CovariantList[Animal]=new CovariantList[Cat]
//
//
//  //2. No=Invariance
//  class InvariantList[A]
//  val invariantAnimalList:InvariantList[Animal]=new InvariantList[Animal]
//
//  //3 hell , no! Contravariance
//  class Trainer[-A]
//  val trainer:Trainer[Cat]=new Trainer[Animal]
//
//  // bounded types
//  class Cage[A<:Animal](Animal :A)
//  val cage=new Cage(new Dog)


  abstract class MyList[+A]{
    def head:A;
    def tail:MyList[A];
    def isEmpty:Boolean;
    def add[B >: A](x:B):MyList[B];
    def printElement:String;

    override def toString:String="["+printElement+"]";
    def map[B](transformer:MyTransformer[A,B]):MyList[B]
    def flatMap[B](transformer: MyTransformer[A,MyList[B]]):MyList[B]
    def filter(predicate:MyPredicate[A]):MyList[A];
    def ++[B>:A](list:MyList[B]):MyList[B]
  }
  object Empty extends MyList[Nothing]{
    def head: Nothing = throw new NoSuchElementException;
    def tail:MyList[Nothing]=throw new NoSuchElementException;
    def isEmpty:Boolean=true;
    def add[B>:Nothing](x:B):MyList[B]=new Cons(x,Empty);
    def printElement:String="";
    def map[B](transformer: MyTransformer[Nothing,B]):MyList[B]=Empty
    def flatMap[B](transformer: MyTransformer[Nothing,MyList[B]]):MyList[B]=Empty
    def filter(predicate:MyPredicate[Nothing]):MyList[Nothing]=Empty
    def ++[B>:Nothing](list:MyList[B]):MyList[B]=list
  }
  class Cons[+A](h:A,t:MyList[A]) extends MyList[A]{
    def head:A=h;
    def tail:MyList[A]=t;
    def isEmpty:Boolean=false;
    def add[B>:A](x:B):MyList[B]=new Cons(x,this);
    def printElement:String=if(t.isEmpty)""+h else h+" "+t.printElement;

    def filter(predicate: MyPredicate[A]):MyList[A]={
      if(predicate.test(h)) new Cons(h,t.filter(predicate))
      else t.filter(predicate);
    }
    def map[B](transformer: MyTransformer[A,B]) :MyList[B]={
    new Cons(transformer.transform(h),t.map(transformer ))}

    def ++[B>:A](list:MyList[B]):MyList[B]=new Cons(h,t++list)

     def flatMap[B](transformer: MyTransformer[A, MyList[B]]): MyList[B] = transformer.transform(h)++t.flatMap(transformer)
  }
  val list:MyList[Int]=new Cons(1,new Cons(2,new Cons(3,Empty)));
  val anotherList:MyList[Int]=new Cons(5,new Cons(6,new Cons(7,Empty)))

  println(list.map(new MyTransformer[Int,Int] {
    override def transform(x: Int): Int = x*2
  }).toString)


  println(list.filter(new MyPredicate[Int] {
    override def test(x: Int): Boolean = x%2==0
  }).toString)

println(list ++ anotherList)

  println(list.flatMap(new MyTransformer[Int,MyList[Int]] {
    override def transform(x: Int): MyList[Int] = new Cons(x,new Cons(x+1,Empty))
  }).toString)
  val list2:MyList[String]=new Cons("aakash",new Cons("aman",new Cons("arpit",Empty)));
  print(list2.toString)


    trait  MyPredicate[-A]{
      def test(x:A):Boolean;
    }
  trait MyTransformer[-A,B]{
    def transform(x:A):B;
  }




}

