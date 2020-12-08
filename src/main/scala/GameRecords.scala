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
    if (!l.contains(t))
     "1 "
    else
     "0 "
  }

  n = resultados.size
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

  var l12E: List[(Int, Int)] = List()
  var l23E: List[(Int, Int)] = List()
  var l34E: List[(Int, Int)] = List()
  var l45E: List[(Int, Int)] = List()
  var l56E: List[(Int, Int)] = List()

  var l12o: List[(Int, Int)] = List()
  var l23o: List[(Int, Int)] = List()
  var l34o: List[(Int, Int)] = List()
  var l45o: List[(Int, Int)] = List()
  var l56o: List[(Int, Int)] = List()

  val LIM1 = 10
  val LIM2 = 150
  var TOPE = 0
  val MAX = .40

  println("Obteniendo pares recientes")
  var n = resultados.size
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
  println("Obteniendo outliers")
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

  var acum = 0.0
  var aux = 0
  m12F.toList.sortBy(_._2).foreach(p => {
    acum += p._2.toDouble / cont.toDouble
    if (acum < MAX || aux == p._2) {
      aux = p._2
      l12o = l12o ::: List(p._1)
    }
  })
  println("Outliers 12: " + l12o.size + " / " + m12F.size)

  acum = 0.0
  aux = 0
  m23F.toList.sortBy(_._2).foreach(p => {
    acum += p._2.toDouble / cont.toDouble
    if (acum < MAX || aux == p._2) {
      aux = p._2
      l23o = l23o ::: List(p._1)
    }
  })
  println("Outliers 23: " + l23o.size + " / " + m23F.size)

  acum = 0.0
  aux = 0
  m34F.toList.sortBy(_._2).foreach(p => {
    acum += p._2.toDouble / cont.toDouble
    if (acum < MAX || aux == p._2) {
      aux = p._2
      l34o = p._1 :: l34o
    }
  })
  println("Outliers 34: " + l34o.size + " / " + m34F.size)

  acum = 0.0
  aux = 0
  m45F.toList.sortBy(_._2).foreach(p => {
    acum += p._2.toDouble / cont.toDouble
    if (acum < MAX || aux == p._2) {
      aux = p._2
      l45o = p._1 :: l45o
    }
  })
  println("Outliers 45: " + l45o.size + " / " + m45F.size)

  acum = 0.0
  aux = 0
  m56F.toList.sortBy(_._2).foreach(p => {
    acum += p._2.toDouble / cont.toDouble
    if (acum < MAX || aux == p._2) {
      aux = p._2
      l56o = p._1 :: l56o
    }
  })
  println("Outliers 56: " + l56o.size + " / " + m56F.size)
  println

  m12R.toList.filter(l => LIM1 < l._2 && l._2 < LIM2).foreach(t => if (!l12o.contains(t._1)) l12E = t._1 :: l12E)
  m23R.toList.filter(l => LIM1 < l._2 && l._2 < LIM2).foreach(t => if (!l23o.contains(t._1)) l23E = t._1 :: l23E)
  m34R.toList.filter(l => LIM1 < l._2 && l._2 < LIM2).foreach(t => if (!l34o.contains(t._1)) l34E = t._1 :: l34E)
  m45R.toList.filter(l => LIM1 < l._2 && l._2 < LIM2).foreach(t => if (!l45o.contains(t._1)) l45E = t._1 :: l45E)
  m56R.toList.filter(l => LIM1 < l._2 && l._2 < LIM2).foreach(t => if (!l56o.contains(t._1)) l56E = t._1 :: l56E)

  println("Esperados 12: " + l12E.size)
  println("Esperados 23: " + l23E.size)
  println("Esperados 34: " + l34E.size)
  println("Esperados 45: " + l45E.size)
  println("Esperados 56: " + l56E.size)

  println
  resultados.foreach(ri => {
    print(has(l12E,(ri.getC1, ri.getC2)))
    print(has(l23E,(ri.getC2, ri.getC3)))
    print(has(l34E,(ri.getC3, ri.getC4)))
    print(has(l45E,(ri.getC4, ri.getC5)))
    print(has(l56E,(ri.getC5, ri.getC6)))
    println
  })
}

