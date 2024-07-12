package advance

object partialFunction extends App{
  val totalFunction=(x:Int)=> x match {
    case 1=>78
    case 2=>34
    case 3=>23
  }
  val partialFunction:PartialFunction[Int,Int] = {
    case 1=>78
    case 2=>34
    case 3=>23
    case _ => -1
  }
  //lift
  val liftedPartialFunction=partialFunction.lift //Int=>Option[Int]

  val chatbot:PartialFunction[String,String]={
    case "hello"=>"Hi , my name chiti robot"
    case "call mom"=>"calling mom"
    case "sing a song for me"=>"chiti chiti robot chiti robot..."
    case "adios"=>"once you start chatting with me there is no going back"
    case _ =>"proceesing your text..."
  }
//  scala.io.Source.stdin.getLines().foreach(line=>println("chatbot says: "+chatbot(line)))



}
