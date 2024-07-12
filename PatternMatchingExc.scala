import scala.reflect.runtime.universe.Match
import scala.util.matching.Regex.Match

object PatternMatchingExc extends App{
  trait Expr
  case class Number(n:Int) extends Expr
  case class Sum(e1 :Expr,e2:Expr) extends Expr
  case class Prod(e1 :Expr,e2:Expr) extends Expr


  def show(e:Expr):String=e match{
    case Number(n)=>s"$n"
    case Sum(e1 ,e2)=>show(e1)+"+"+show(e2)
    case Prod(e1,e2)=>{
      def mayBeShowParentheses(exp:Expr)=exp match{
      case Prod(_,_)=>show(exp)
      case Number(_)=>show(exp)
      case _ =>"("+show(exp) +")"      }
      mayBeShowParentheses(e1) + "*" + mayBeShowParentheses(e2)
    }

  }
  println(show(Prod(Sum(Number(2),Number(1)),Number(3))))
  println(show(Sum(Prod(Number(2),Number(1)),Number(3))))
}
