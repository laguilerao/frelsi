import java.security.SecureRandom

import scala.collection.immutable.TreeSet
import scala.collection.mutable
import scala.util.Random

class Foretell(param: Parameters, intentos: Int) {

  def urna(tope: Int): List[Int] = {
    val random = new SecureRandom();
    var l: List[Int] = List()

    while (l.size < tope) {
      val i = random.nextInt(tope) + 1
      if (!l.contains(i)) l = List(i) ::: l
    }
    l
  }

  def guess(u: List[Int], d: TreeSet[Int]): List[Int] = {
    var s = Random.shuffle(u)
    do {
      s = Random.shuffle(u)
    } while (Random.nextBoolean)
    if (d.size == 6)
      d.toList
    else
      guess(s.tail, (d + u.head))
  }

  val records = new GameRecords(param.history)

  var e12 = 0
  var e23 = 0
  var e34 = 0
  var e45 = 0
  var e56 = 0
  var g = 0
  var g1 = 0




  records.printVals()
  var i = 0
  var j = 0

  while (i < intentos) {
    j += 1
    val l = guess(urna(records.TOPE), TreeSet())
    val d = new Draw(j, l(0), l(1), l(2), l(3), l(4), l(5), 0)
    if (records.ganadores.contains(d.toString)) g1 += 1
    if (!records.l12E.contains((d.getC1, d.getC2)))
      if (!records.l23E.contains((d.getC2, d.getC3)))
        if (!records.l34E.contains((d.getC3, d.getC4)))
          if (!records.l45E.contains((d.getC4, d.getC5)))
            if (!records.l56E.contains((d.getC5, d.getC6)))
              if (records.ganadores.contains(d.toString)) {
                i += 1
                println(i + " " + d.pretty)
              } else
                g += 1
            else
              e56 += 1
          else
            e45 += 1
        else
          e34 += 1
      else
        e23 += 1
    else
      e12 += 1
  }
  println
  println("Ganadores: " + g + " / " + g1)
  println("Descartados por P12: " + e12)
  println("Descartados por P23: " + e23)
  println("Descartados por P34: " + e34)
  println("Descartados por P45: " + e45)
  println("Descartados por P56: " + e56)
  println("Intentos: " + j)
  println
}