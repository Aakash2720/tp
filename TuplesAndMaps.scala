object TuplesAndMaps extends App{
//  val   aTuples=(2,"Hello, world","all good")
//  println(aTuples._1)
//  println(aTuples.copy(_2 = "aakash vishwakarma"))
//  println(aTuples)
//
//  val aMap:Map[String,Int]=Map()
//  val phoneBook=Map(("Aakash",3333),"Aman"->999)
////  println(phoneBook)
//  val name:String="aakash"
//  if(phoneBook.contains(name))println(phoneBook(name))
//  else println("not found")
//
//  //add pair to the map
//  val newPair = ("Arpit" , 43543)
//  val newPhoneBook =phoneBook + newPair
////  println(newPhoneBook.map(pair=>pair._1.toLowerCase ->pair._2))
//
//  val studentDiary:Map[Int,Map[String,String]]=Map(1->Map("aakash"->"IT"),2->Map("aman"->"Finance"))
//  println(studentDiary.foreach(pair=>{ val new_pair=pair._2
//    new_pair.foreach(pa=>println(pa._2))}))
//  val student =Map("jim"->9999,"JIM"->111)
//  val newStundet=student.map(pair=>pair._1.toLowerCase->pair._2)
//  println(newStundet)

  def add(network:Map[String,Set[String]],name:String):Map[String,Set[String]]={
    network +(name->Set())
  }
  def friend(network:Map[String,Set[String]],a:String,b:String):Map[String,Set[String]]={
    val network1=network(a)
    val network2=network(b)
    network + (a -> (network1 + b )) +( b -> ( network2 + a));
  }
  def unfriend(network:Map[String,Set[String]],a:String,b:String)={
    val network1=network(a)
    val network2=network(b)
    network + (a->(network1-b)) + (b->(network2-a));
  }
  def  remove(network:Map[String,Set[String]],person:String):Map[String,Set[String]]={
    def removeAux(friend:Set[String],networkAcc:Map[String,Set[String]]):Map[String,Set[String]]={
      if(friend.isEmpty) networkAcc
      else removeAux(friend.tail,unfriend(networkAcc,person,friend.head))
    }
    val unfriended=removeAux(network(person),network)
    unfriended-person
  }
  val empty:Map[String,Set[String]]=Map()
  val network=add(add(empty,"Bob"),"Mary")
  var new_network=add(network,"aman")

  new_network=friend(new_network,"Bob","Mary")
  new_network=friend(new_network,"aman","Bob")

  def socialNetworking(network:Map[String,Set[String]],a:String,b:String):Boolean={
    def bfs(target:String,consideredPeople:Set[String],discoveredPeople:Set[String]):Boolean={
      if(discoveredPeople.isEmpty)false
      else {
        val person=discoveredPeople.head
        if(person==target)true
        else if(consideredPeople.contains(person))bfs(target,consideredPeople,discoveredPeople.tail)
        else bfs(target,consideredPeople+person,discoveredPeople.tail ++ network(person))
      }

    }
    bfs(b,Set(),network(a)+a)
  }
  println(new_network)
}
