import scala.collection.immutable
import scala.collection.immutable.TreeSet

// ToDo atrapar errores causados por cadenas mal formadas

class Draw(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int) {
  private val id = a
  private val c1 = b
  private val c2 = c
  private val c3 = d
  private val c4 = e
  private val c5 = f
  private val c6 = g
  private val ad = h


  def getId: Int = id
  def getC1: Int = c1
  def getC2: Int = c2
  def getC3: Int = c3
  def getC4: Int = c4
  def getC5: Int = c5
  def getC6: Int = c6
  def getAdicional: Int = ad

  def pretty: String = "[%2s %2s %2s %2s %2s %2s]".format(c1, c2, c3, c4, c5, c6)

  def getConsecutivos: Int = {
    var ac = 0
    if (c2 - c1 == 1) ac += 1
    if (c3 - c2 == 1) ac += 1
    if (c4 - c3 == 1) ac += 1
    if (c5 - c4 == 1) ac += 1
    if (c6 - c5 == 1) ac += 1
    ac
  }

  def contiene(i: Int): Boolean = c1 == i || c2 == i || c3 == i || c4 == i || c5 == i || c6 == i

  def cont(t: TreeSet[Int]): Int = this.cont1(t) + this.cont2(t) + this.cont3(t) + this.cont4(t) + this.cont5(t) + this.cont6(t)

  def cont1(t: TreeSet[Int]): Int = if (t.contains(this.c1)) 1 else 0

  def cont2(t: TreeSet[Int]): Int = if (t.contains(this.c2)) 1 else 0

  def cont3(t: TreeSet[Int]): Int = if (t.contains(this.c3)) 1 else 0

  def cont4(t: TreeSet[Int]): Int = if (t.contains(this.c4)) 1 else 0

  def cont5(t: TreeSet[Int]): Int = if (t.contains(this.c5)) 1 else 0

  def cont6(t: TreeSet[Int]): Int = if (t.contains(this.c6)) 1 else 0

  def contiene(t: TreeSet[Int]): Boolean = {
    var aux = false

    t.foreach(i => aux = aux || c1 == i || c2 == i || c3 == i || c4 == i || c5 == i || c6 == i)
    aux
  }

  def cuenta(t: TreeSet[Int]): Int = if (t.isEmpty) 0
  else {
    var aux = 0
    if (t.contains(c1)) aux += 1
    if (t.contains(c2)) aux += 1
    if (t.contains(c3)) aux += 1
    if (t.contains(c4)) aux += 1
    if (t.contains(c5)) aux += 1
    if (t.contains(c6)) aux += 1
    aux
  }

  def aciertos(r: Draw): Int = {
    var aux = 0
    if (this.c1 == r.c1) aux += 1
    if (this.c2 == r.c2) aux += 1
    if (this.c3 == r.c3) aux += 1
    if (this.c4 == r.c4) aux += 1
    if (this.c5 == r.c5) aux += 1
    if (this.c6 == r.c6) aux += 1
    aux
  }

  def distancia(r: Draw): Int = {
    var aux = 0
    if (this.c1 != r.c1) aux += 1
    if (this.c2 != r.c2) aux += 1
    if (this.c3 != r.c3) aux += 1
    if (this.c4 != r.c4) aux += 1
    if (this.c5 != r.c5) aux += 1
    if (this.c6 != r.c6) aux += 1
    aux
  }

   def tuple(n: Int): List[List[Int]] = combinations(n, List(c1, c2, c3, c4, c5, c6))

  def contieneTuple(n: Int, l: List[List[Int]]): Boolean = {
    this.tuple(n).foreach { x => if (l.contains(x))  true }
    false
  }

  def contienePar(n: Int, l: List[Int]): Boolean = {
    this.tuple(n).contains(l)
  }

  def cuentaTuple(n: Int, l: List[List[Int]]): Int = {
    var i = 0
    this.tuple(n).foreach { x => if (l.contains(x)) i = i + 1 }
    i
  }

  private def flatMapSublists[A, B](ls: List[A])(f: List[A] => List[B]): List[B] =
    ls match {
      case Nil => Nil
      case sublist@_ :: tail => f(sublist) ::: flatMapSublists(tail)(f)
    }

  private def combinations[A](n: Int, ls: List[A]): List[List[A]] =
    if (n == 0)
      List(Nil)
    else
      flatMapSublists(ls) { sl =>
        combinations(n - 1, sl.tail) map {
          sl.head :: _
        }
      }

  def patronDecenas(): String = {
    var aux = 0
    aux = aux + math.pow(10, this.c1 / 10).toInt
    aux = aux + math.pow(10, this.c2 / 10).toInt
    aux = aux + math.pow(10, this.c3 / 10).toInt
    aux = aux + math.pow(10, this.c4 / 10).toInt
    aux = aux + math.pow(10, this.c5 / 10).toInt
    aux = aux + math.pow(10, this.c6 / 10).toInt
    val s = "%06d".format(aux)
    s.reverse
  }

  override def toString: String = "%s %s %s %s %s %s".format(c1, c2, c3, c4, c5, c6)
}
