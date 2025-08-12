import scala.util.Random

case class Packet(size: Int, arrivalTime: Double = 0.0)

class CoDelQueue(capacity: Int) {
  private val items = scala.collection.mutable.ArrayBuffer[Packet]()
  private var firstAboveTime = 0.0
  private var dropNext = Double.PositiveInfinity
  private var dropCount = 0

  def enqueue(p: Packet): Boolean = {
    if (items.size >= capacity) {
        println("Queue full, packet dropped, size: " + p.size)
        false
    } else {
        items += p
        println("Packet enqueued, size: " + p.size)
        true
    }
}

  def dequeue(currentTime: Double, target: Double, interval: Double) {
    while (items.size > 0) {
        val p = items(0)
        val sojournTime = currentTime - p.arrivalTime

        if (sojournTime < target || items.size <= 4) {
            firstAboveTime = 0.0
            dropNext = Double.PositiveInfinity
            items.remove(0)
            println(f"Packet dequeued, size: ${p.size}, sojourn time: $sojournTime%.2f")
            dropCount = 0
        } else if (firstAboveTime == 0.0) {
            firstAboveTime = currentTime + interval
            dropNext = firstAboveTime
            items.remove(0)
            println(f"Packet dequeued, size: ${p.size}, sojourn time: $sojournTime%.2f")
        } else if (currentTime >= dropNext) {
            items.remove(0)
            println(f"Packet dropped, size: ${p.size}, sojourn time: $sojournTime%.2f")
            dropCount += 1
            dropNext = currentTime + interval / math.sqrt(dropCount.toDouble)
        } else {
            items.remove(0)
            println(f"Packet dequeued, size: ${p.size}, sojourn time: $sojournTime%.2f")
            dropCount = 0
        }
    }
}

  def size: Int = items.size
}

def simulateCoDel(packets: Seq[Packet], target: Double, interval: Double, capacity: Int): Unit = {
    val q = new CoDelQueue(capacity)
    var currentTime = 0.0
    var dropped = 0
    println("Initial queue: empty")

    packets.foreach { p =>
        val packet = p.copy(arrivalTime = currentTime)
        if (q.enqueue(packet)) {
            q.dequeue(currentTime, target, interval)
        } else {
            dropped += 1
        }
        currentTime += 1.0
    }

    q.dequeue(currentTime, target, interval)

    println(s"Final queue length: ${q.size}")
    println(s"Packets dropped: $dropped")
    println("Final queue: empty")
}

def main(args: Array[String]): Unit = {
    val rand = new Random(42)
    val packets = (1 to 200).map(_ => Packet(rand.nextInt(100) + 1))
    simulateCoDel(packets, 5.0, 100.0, 100)
}
