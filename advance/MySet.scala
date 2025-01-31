package advance

import scala.annotation.tailrec

trait MySet[A] extends(A =>Boolean){
  def apply(elem:A):Boolean=contains(elem)
  def contains(elem:A):Boolean
  def +(elem:A):MySet[A]
  def ++(anotherSet:MySet[A]):MySet[A]
  def remove(elem:A):MySet[A]
  def intersection(anotherSet:MySet[A]):MySet[A]
  def map[B](f:A => B):MySet[B]
  def flatMap[B](f:A=>MySet[B]):MySet[B]
  def filter(predicate:A=>Boolean):MySet[A]
  def foreach(f:A=>Unit):Unit

}
class EmptySet[A] extends MySet[A]{

  def contains(elem:A):Boolean=false
  def +(elem:A):MySet[A]=new NonEmptySet[A](elem,this)
  def ++(anotherSet:MySet[A]):MySet[A]=anotherSet
  def remove(elem:A):MySet[A]=new EmptySet[A]
  def intersection(anotherSet:MySet[A]):MySet[A]=new EmptySet[A]
  def map[B](f:A => B):MySet[B]=new EmptySet[B]
  def flatMap[B](f:A=>MySet[B]):MySet[B]=new EmptySet[B]
  def filter(predicate:A=>Boolean):MySet[A]=this
  def foreach(f:A=>Unit):Unit=()
}
class NonEmptySet[A](head:A,tail:MySet[A]) extends MySet[A] {
  def contains(elem:A):Boolean= elem==head||tail.contains(elem)
  def +(elem:A):MySet[A]=if(this contains elem )this else new NonEmptySet[A](elem,this)

  /*
  * [1,2,3]++[4,5]
  * [2,3]++[4,5]+1
  * [3]++[4,5]+1+2
  * []++[4,5]+1+2+3
  * [4,5]+1+2+3=[4,5,1,2,3]
  * */
  def ++(anotherSet:MySet[A]):MySet[A]=tail ++anotherSet + head
  def remove(elem:A):MySet[A]={
    if(head==elem)tail
    else tail.remove(elem)+head
  }
  def intersection(anotherSet:MySet[A]):MySet[A]=filter(anotherSet)
  def map[B](f:A => B):MySet[B]=tail.map(f)+f(head)
  def flatMap[B](f:A=>MySet[B]):MySet[B]=tail.flatMap(f)++f(head)
  def filter(predicate:A=>Boolean):MySet[A]={
      val filteredTail=tail filter predicate
      if(predicate(head))filteredTail+head
      else filteredTail
  }
  def foreach(f:A=>Unit):Unit={
    f(head)
    tail.foreach(f)}
}
object MySet{
  def apply[A](values:A*):MySet[A]={
    @tailrec
    def buildSet(valSeq:Seq[A],acc:MySet[A]):MySet[A]=
      if(valSeq.isEmpty)acc
      else buildSet(valSeq.tail,acc+valSeq.head)
    buildSet(values.toSeq,new EmptySet[A])

  }
}
object MySetPlayGround extends App{
  val s=MySet(1,2,3,4)
  s foreach print
  println()
  val t=s.remove(2)

  t foreach print
}