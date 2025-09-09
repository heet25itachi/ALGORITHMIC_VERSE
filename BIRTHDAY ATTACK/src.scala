import scala.util.Random
import scala.collection.mutable

object BirthdayAttack {
  val R = 65536

  def birthdayAttack(): Unit = {
    val table = mutable.Map[Int, Long]()
    var trials = 0
    while (true) {
      val x = Random.nextLong()
      val h = (x % R).toInt
      trials += 1
      if (table.contains(h) && table(h) != x) {
        println(s"Collision found after $trials trials: x_i = ${table(h)}, x_j = $x, hash = $h")
        return
      }
      table(h) = x
    }
  }

  def main(args: Array[String]): Unit = {
    birthdayAttack()
  }
}
