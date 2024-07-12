package advance

object Pre extends App{
//    val prependList=  List(3,4) ::  List(5,6)
//      println(prependList)
//    class TeenGirl(name:String){
//        def `and the said`(gossip:String)=println(s"$name said $gossip")
//    }
//    val lilly =new TeenGirl("lilly")
//    lilly `and the said` "scala is so sweet"


    class Person(val name:String,val age:Integer)

    object personPattern{
        // unapply method extracts data from an object to be used in pattern matching.
        //option is use to handle null values if the value is present the return some otherwise none
        // we can override the unapply method
        def unapply(person:Person):Option[(String,Int)]=Some((person.name,person.age))}

    val bob =new Person("bob",25)
    val greeting= bob match{
//    Pattern Matching: When you write case personPattern(n, a), Scala looks for an unapply
//    method in the personPattern object. If unapply returns a Some value, the pattern match i
//    s successful, and the values can be bound to variables (n and a).
        case personPattern(n,a)=>s"Hii ,my name is $n and my age is $a"
    }
    val n:Int=12
    object mathPro1{
        def unapply(n:Int):Option[Int]={
            if( n <10) Some(n) else None
        }

    }
    object mathPro2 {
        def unapply(n:Int):Option[Int]={
            if(n%2==0) Some(n) else None
        }
    }

    val mathProperty = n match {
        case mathPro1(_)=>"single digit"
        case mathPro2(_)=>"an even number"
        case _=>"no property"
    }
//    println(mathProperty)


    abstract class MyList[+A]{
        def head:A
        def tail:MyList[A]
    }
    case object Empty extends MyList[Nothing]{
        def head:Nothing=throw new NoSuchElementException
        def tail:MyList[Nothing] =throw new NoSuchElementException
    }
    case class Cons[+A](override val head:A, override val tail:MyList[A]) extends MyList[A]
    object MyList{
        def unapplySeq[A](list:MyList[A]):Option[Seq[A]]=
            if(list==Empty) Some(Seq.empty)
            else unapplySeq(list.tail).map(list.head+:_)
    }
    val myList:MyList[Int]=Cons(3,Cons(2,Cons(1,Empty)))
    val decomposed=myList match{
        case MyList(1,2,_*)=>"starting with 1,2"
        case _=>"something else"
    }

//    println(decomposed)


    // instead of option you can class where it should have method isEmpty ,get and this name
    // is only for better understanding of the code you can change the name
    abstract class Wrapper[A]{

        def isEmpty:Boolean
        def get:A
    }
    object personWrapper {
        def unapply(person:Person):Wrapper[String]=new Wrapper[String]{
            def isEmpty=false;
            def get=person.name
        }

    }
    println(bob match{
        case personWrapper(name)=>s"Hii ,my name is $name"
        case _=>"something else"
    })


}
