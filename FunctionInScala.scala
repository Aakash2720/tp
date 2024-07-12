object FunctionInScala extends App{

//  val adder : ( (Int,Int  ) => Int){
//      override def apply(a:Int,b:Int):Int=a+b
//  }
//    val stringToIntConverter=new Function1[String,Int]{
//      override def apply(s:String):Int=s.toInt
//    }
//    val stringConcatination=new Function2[String,String,String]{
//      override def apply(s1:String,s2:String):String =s1+" "+s2;
//    }
//  println(stringConcatination("aakash","vishwakarma"))
//
//  println(stringToIntConverter("5")+5)



  abstract class MyList[+A]{
    def head:A;
    def tail:MyList[A];
    def isEmpty:Boolean;
    def add[B >: A](x:B):MyList[B];
    def printElement:String;

    override def toString:String="["+printElement+"]";
    def map[B](transformer:A=>B):MyList[B]
    def flatMap[B](transformer: A=>MyList[B]):MyList[B]
    def filter(predicate:A=>Boolean):MyList[A];
    def ++[B>:A](list:MyList[B]):MyList[B]
    def foreach(f:A=>Unit):Unit
    def sort(compare :(A,A)=>Int):MyList[A]
  }
  object Empty extends MyList[Nothing]{
    def head: Nothing = throw new NoSuchElementException;
    def tail:MyList[Nothing]=throw new NoSuchElementException;
    def isEmpty:Boolean=true;
    def add[B>:Nothing](x:B):MyList[B]=new Cons(x,Empty);
    def printElement:String="";
    def map[B](transformer: Nothing=>B):MyList[B]=Empty
    def flatMap[B](transformer: Nothing=>MyList[B]):MyList[B]=Empty
    def filter(predicate:Nothing=>Boolean):MyList[Nothing]=Empty
    def ++[B>:Nothing](list:MyList[B]):MyList[B]=list
    def foreach(f:Nothing=>Unit):Unit=()
    def sort(compare :(Nothing,Nothing)=>Int)=Empty
  }
  class Cons[+A](h:A,t:MyList[A]) extends MyList[A]{
    def head:A=h;
    def tail:MyList[A]=t;
    def isEmpty:Boolean=false;
    def add[B>:A](x:B):MyList[B]=new Cons(x,this);
    def printElement:String=if(t.isEmpty)""+h else h+" "+t.printElement;

    def filter(predicate: A=>Boolean):MyList[A]={
      if(predicate(h)) new Cons(h,t.filter(predicate))
      else t.filter(predicate);
    }
    def map[B](transformer: A=>B) :MyList[B]={
      new Cons(transformer(h),t.map(transformer ))}

    def ++[B>:A](list:MyList[B]):MyList[B]=new Cons(h,t++list)

    def flatMap[B](transformer: A=> MyList[B]): MyList[B] = transformer(h)++t.flatMap(transformer)
    def foreach(f:A=>Unit):Unit={
      f(h)
      t.foreach(f)
    }

    def sort(compare: (A, A) => Int): MyList[A] = {
      def insert(x: A, sortedList: MyList[A]): MyList[A] = {
        if (sortedList.isEmpty) new Cons(x, Empty)
        else if (compare(x, sortedList.head) <= 0) new Cons(x, sortedList)
        else new Cons(sortedList.head, insert(x, sortedList.tail))
      }

      val sorted = t.sort(compare)
      insert(h, sorted)
    }


  }
  val list:MyList[Int]=new Cons(1,new Cons(2,new Cons(3,Empty)));
  val anotherList:MyList[Int]=new Cons(5,new Cons(6,new Cons(7,Empty)))
  println(list.foreach(println))
  println(list.sort((x,y)=>y-x))
  println(list.map(new Function1[Int,Int] {
    override def apply(x: Int): Int = x*2
  }).toString)


  println(list.filter(new Function1[Int,Boolean] {
    override def apply(x: Int): Boolean = x%2==0
  }).toString)

  println(list ++ anotherList)

  println(list.flatMap(new Function[Int,MyList[Int]] {
    override def apply(x: Int): MyList[Int] = new Cons(x,new Cons(x+1,Empty))
  }).toString)
  val list2:MyList[String]=new Cons("aakash",new Cons("aman",new Cons("arpit",Empty)));
  print(list2.toString)


//  trait  MyPredicate[-A]{
//    def test(x:A):Boolean;
//  }
//  trait MyTransformer[-A,B]{
//    def transform(x:A):B;
//  }

  val superAdder=new Function1[Int,Function1[Int,Int]]{
    override def apply(x:Int):Function1[Int,Int]=new Function1[Int,Int]{
      override def apply(y:Int):Int=x+y
    }
  }
  val adder =superAdder(4)
  println(adder(4))
}