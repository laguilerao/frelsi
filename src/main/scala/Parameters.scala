class Parameters(sorteo: String) {
  private val nombre = sorteo
  private var tope = 0

  case class SinDatosException(message: String) extends Exception(message)

  private def textSorteos = sorteo match {
    case "melate" => io.Source.fromFile("../datos/melate.csv")
    case "retro" => io.Source.fromFile("../datos/retro.csv")
    case "revancha" => io.Source.fromFile("../datos/revancha.csv")
    case _ => null
  }

  def getNombre: String = nombre

  private val iterSorteos = textSorteos.getLines().map(_.split(","))
  var history: List[Draw] = List()

  def last: Draw = history.last
  iterSorteos.foreach(s =>
    history = history ::: List(new Draw(s(0).toInt, s(1).toInt, s(2).toInt, s(3).toInt,
      s(4).toInt, s(5).toInt, s(6).toInt, s(7).toInt )))
  textSorteos.close


}