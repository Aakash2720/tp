import java.util.Random

import java.lang.System
import scala.util.Try
object TryExc extends App{
  val host="localhost"
  val port ="8080"
  def renderHTML(page:String)=println(page)

  class Connection{
    def get(url:String):String={
      val random=new Random {
        System.nanoTime()
      }
      if(random.nextBoolean()) "<Html>...</Html>"
      else throw new RuntimeException("Connection interrupted")
    }
    def getSafe(url:String) : Try[String] = Try(get(url))
  }
  object HttpService{
    val random=new Random(System.nanoTime())
    def getConnection(host:String,port:String):Connection=
      if(random.nextBoolean())new Connection
      else throw new RuntimeException("Some else took the port")

    def getSafeConnection(host:String,port:String):Try[Connection]=Try(getConnection(host,port));
  }
  val possibleConnection=HttpService.getSafeConnection(host,port)
  val possibleHtml=possibleConnection.flatMap(connection=>connection.getSafe("/home"))
  possibleHtml.foreach(renderHTML)
}
