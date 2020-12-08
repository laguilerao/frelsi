object Frelsi extends App {


  val nombreSorteo = "melate"
  val sorteo = new Parameters(nombreSorteo)
  val version = "1.00.00"
  val intentos = 20


  new Foretell(sorteo, intentos)

  println
  println("Game: " + sorteo.getNombre)
  println("frelsi " + version )


}
