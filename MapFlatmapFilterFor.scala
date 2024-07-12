object MapFlatmapFilterFor extends App{

  val list=List(1,2,3,4,"aakash")
  println(list)
  val char=List('a','b','c','d')
//  for(i <-0 until list.size){
//    for(j<-0 until char.size){
//      println(s"${char(j)}${list(i)}")
//    }}
  val colors=List("black","White")
  val combinations=list.flatMap(n=>char.flatMap(c=>colors.map(colors=>c+""+n+" "+colors)))

  val forCombinations=for{
    n<- list
    c<- char
    colors<- colors
  } yield c+""+n+" "+colors
//  println(forCombinations)
  val aSeq:Seq[Int]=Seq(1,2,3,4,5);
  println(aSeq)
}
