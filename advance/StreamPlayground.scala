package advance

abstract class MyStream[+A]{
  def isEmpty:Boolean
  def head:A
  def tail:MyStream[A]
  def #::[B>:A](elem:B):MyStream[B]
  def ++[B>:A](anotherStream:MyStream[B]):MyStream[B]
  def foreach(f:A=>Unit);Unit
  def map[B](f:A=>B):MyStream[B]
  def flatMap[B](f:A=>MyStream[B]):MyStream[B]
  def filter(predicate:A=>Boolean):MyStream[A]
  def take(n:Int):MyStream[A]
  def takeAsList(n:Int):List[A]}
object EmptyStream extends MyStream[Nothing]{
  def isEmpty:Boolean=true
  def head:Nothing=throw new NoSuchElementException()
  def tail:MyStream[Nothing]=throw new NoSuchElementException
  def #::[B>:Nothing](elem:B):MyStream[B]=new Cons(elem,this)
  def ++[B>:Nothing](anotherStream:MyStream[B]):MyStream[B]=anotherStream
  def foreach(f:Nothing=>Unit)=Unit
  def map[B](f:Nothing=>B):MyStream[B]=this
  def flatMap[B](f:Nothing=>MyStream[B]):MyStream[B]=this
  def filter(predicate:Nothing=>Boolean):MyStream[Nothing]=this
  def take(n:Int):MyStream[Nothing]=this
  def takeAsList(n:Int):List[Nothing]=Nil
}
class Cons[+A](h:A,t : => MyStream[A])extends MyStream[A]{
  def isEmpty:Boolean=false
  override val head:A=h
  override lazy val tail:MyStream[A]=t
  def #::[B>:A](elem:B):MyStream[B]=new Cons(elem,this)
  def ++[B>:A](anotherStream:MyStream[B]):MyStream[B]=new Cons(head,tail ++ anotherStream)
  def foreach(f:A=>Unit)={
    f(head)
    tail.foreach(f)
  }

  def map[B](f:A=>B):MyStream[B]=new Cons(f(head),tail.map(f))
  def flatMap[B](f:A=>MyStream[B]):MyStream[B]=f(head)++ tail.flatMap(f)
  def filter(predicate:A=>Boolean):MyStream[A]={
    if(predicate(head))new Cons(head,tail.filter(predicate))
    else tail.filter(predicate)
  }
  def take(n:Int):MyStream[A]
  def takeAsList(n:Int):List[A]
}
object StreamPlayground extends App{

}
