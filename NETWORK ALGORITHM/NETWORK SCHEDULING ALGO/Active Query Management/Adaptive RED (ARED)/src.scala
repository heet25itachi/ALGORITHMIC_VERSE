import scala.util.Random

case class Packet(size: Int, arrivalTime: Double = 0.0)

class Queue(capacity: Int) {
  private val items = scala.collection.mutable.ArrayBuffer[Packet]()
  def enqueue(p: Packet): Boolean = {
    if (items.size >= capacity) false
    else { items += p; true }
  }
  def size: Int = items.size
  def printQueue(): Unit = println(s"Final queue: ${items.map(_.size).mkString(" ")}")
}

object AQM {
  def simulateARED(packets: Seq[Packet], minTh: Double, maxTh: Double, wq: Double, target: Double, alpha: Double, beta: Double, interval: Double, capacity: Int): Unit = {
    val q = new Queue(capacity)
    var avg = 0.0
    var maxP = 0.1
    var lastUpdate = 0.0
    var currentTime = 0.0
    var count = 0
    var dropped = 0
    println("Initial queue: empty")

    packets.foreach { p =>
      avg = if (q.size == 0) 0.0 else (1 - wq) * avg + wq * q.size
      if (currentTime - lastUpdate >= interval) {
        if (avg > target && maxP <= 0.5) maxP *= (1 + alpha)
