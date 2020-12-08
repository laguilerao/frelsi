class Record(i: Integer, ob: Double, fr: Integer, ul: Integer) {

  private val id = i
  private val observada = ob
  private val frecuencia = fr
  private val ultima = ul

  def getId = id
  def getObservada = observada
  def getFrecuencia = frecuencia
  def getUltima = ultima

  override def toString() = id + " " + frecuencia + " " + ultima + " " + observada

}
