object Frelsi extends App {


  val nombreSorteo = "melate"
  val sorteo = new Parameters(nombreSorteo)
  val version = "1.02.00"
  val intentos = 10


  new Foretell(sorteo, intentos)

  println
  println("Game: " + sorteo.getNombre)
  println("frelsi " + version )


}
