import java.io.{File, PrintWriter}
import java.text.DecimalFormat
import scala.util.Random
import scala.collection.immutable.TreeSet
import scala.collection.mutable

class GameRecords(resultados: List[Draw]) {

  var ganadores: TreeSet[String] = TreeSet()

  def printVals(): Unit = {
    val formatter = new DecimalFormat("#,###");
    val pl = resultados.last

    println("History records: " + formatter.format(resultados.size))
    println("Last draw: " + resultados.last.getId + " " + resultados.last)
    println("Candidatos: ")
    println
  }

  def addMap(t: (Int, Int), m: mutable.Map[(Int, Int), Int]) = {
    if (m.contains(t))
      m(t) = m(t) + 1
    else
      m(t) = 1
  }

  def has(l: List[(Int, Int)], t: (Int, Int)) = {
    if (l.contains(t))
     "1 "
    else
     "0 "
  }

  def distHist(d: Draw, t: Int, l: List[Draw]): Int =
    l match {
      case Nil => t
      case head :: tail => {
        val x = d.distancia(head)
        if (x == 0)
          0
        else
          distHist(d, if (x < t) x else t , tail)
      }
    }

  def getPairFrequent(lp: List[((Int,Int),Int)]): List[(Int,Int)] = {
    var acum = 0.0
    var aux = 0
    var lt: List[(Int,Int)] = List()

    lp.sortBy(_._2).reverse.foreach(p => {
      acum += p._2.toDouble / cont.toDouble
      if (acum < MAX || aux == p._2) {
        aux = p._2
        lt = lt ::: List(p._1)
      }
    })
    lt
  }

  def getPairExpected(lp: List[((Int,Int),Int)],lpe: List[(Int,Int)],s: Int): List[(Int,Int)] = {
    var lt: List[(Int,Int)] = List()
    lp.sortBy(_._2).reverse.foreach(t => if (lpe.contains(t._1) && lt.size < s) lt = t._1 :: lt)
    lt.reverse
  }

  var m12F: mutable.Map[(Int, Int), Int] = mutable.Map()
  var m23F: mutable.Map[(Int, Int), Int] = mutable.Map()
  var m34F: mutable.Map[(Int, Int), Int] = mutable.Map()
  var m45F: mutable.Map[(Int, Int), Int] = mutable.Map()
  var m56F: mutable.Map[(Int, Int), Int] = mutable.Map()

  var m12R: mutable.Map[(Int, Int), Int] = mutable.Map()
  var m23R: mutable.Map[(Int, Int), Int] = mutable.Map()
  var m34R: mutable.Map[(Int, Int), Int] = mutable.Map()
  var m45R: mutable.Map[(Int, Int), Int] = mutable.Map()
  var m56R: mutable.Map[(Int, Int), Int] = mutable.Map()

  var TOPE = 0
  val MAX = .80
  var n = resultados.size

  println("Obteniendo pares recientes")

  resultados.foreach(ri => {
    ganadores += ri.toString
    m12R((ri.getC1, ri.getC2)) = n
    m23R((ri.getC2, ri.getC3)) = n
    m34R((ri.getC3, ri.getC4)) = n
    m45R((ri.getC4, ri.getC5)) = n
    m56R((ri.getC5, ri.getC6)) = n
    if (TOPE < ri.getC6) TOPE = ri.getC6
    n -= 1
  })
  println("R12: " + m12R.size)
  println("R23: " + m23R.size)
  println("R34: " + m34R.size)
  println("R45: " + m45R.size)
  println("R56: " + m56R.size)
  println
  println("Obteniendo esperados")
  var cont = 0
  for (i1 <- 1 to TOPE - 5)
    for (i2 <- i1 + 1 to TOPE - 4)
      for (i3 <- i2 + 1 to TOPE - 3)
        for (i4 <- i3 + 1 to TOPE - 2)
          for (i5 <- i4 + 1 to TOPE - 1)
            for (i6 <- i5 + 1 to TOPE) {
              addMap((i1, i2), m12F)
              addMap((i2, i3), m23F)
              addMap((i3, i4), m34F)
              addMap((i4, i5), m45F)
              addMap((i5, i6), m56F)
              cont += 1
            }

  val l12e = getPairFrequent(m12F.toList)
  println("Frecuentes 12: " + l12e.size + " / " + m12F.size)

  val l23e = getPairFrequent(m23F.toList)
  println("Frecuentes 23: " + l23e.size + " / " + m23F.size)

  val l34e = getPairFrequent((m34F.toList))
  println("Frecuentes 34: " + l34e.size + " / " + m34F.size)

  val l45e = getPairFrequent(m45F.toList)
  println("Frecuentes 45: " + l45e.size + " / " + m45F.size)

  val l56e = getPairFrequent(m56F.toList)
  println("Frecuentes 56: " + l56e.size + " / " + m56F.size)
  println

  val l12E = getPairExpected(m12R.toList, l12e, l12e.size/5)
  val l23E = getPairExpected(m23R.toList, l23e, l23e.size/4)
  val l34E = getPairExpected(m34R.toList, l34e, l34e.size/3)
  val l45E = getPairExpected(m45R.toList, l45e, l45e.size/4)
  val l56E = getPairExpected(m56R.toList, l56e, l56e.size/5)

  println("Esperados 12: " + l12E.size)
  println("Esperados 23: " + l23E.size)
  println("Esperados 34: " + l34E.size)
  println("Esperados 45: " + l45E.size)
  println("Esperados 56: " + l56E.size)

}

