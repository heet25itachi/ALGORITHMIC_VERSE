import scala.util.Random

// Define Packet and Queue classes
case class Packet(size: Int)
class Queue(capacity: Int) {
    private val items = scala.collection.mutable.ArrayBuffer[Packet]()
    def enqueue(p: Packet): Boolean = {
        if (items.size >= capacity) false
        else { items += p; true }
    }
    def size: Int = items.size
    def printQueue(): Unit = println(s"Final queue: ${items.map(_.size).mkString(" ")}")
}

// Simulate RED algorithm
object RED {
    def simulateRed(packets: Seq[Packet], minTh: Double, maxTh: Double, wq: Double, maxP: Double, capacity: Int): Unit {
        val q = new Queue(capacity)
        var avg = 0.0
        var count = 0.0
        var dropped = 0.0
        val rand = new Random(42)
        println("Initial queue: empty")
        packets.foreach { p =>
            avg = if (q.size == 0) 0.0 else (1 - wq) * avg + wq * q.size
            val drop = if (avg < minTh) false
                       else if (avg >= maxTh) true
                       else {
                           val pb = maxP * (avg - minTh) / (maxTh - minTh)
                           val pa = pb / (1 - count * pb)
                           count += 1
                           rand.nextDouble() < pa
                       }
            if (drop) {
                println(f"Packet dropped, size: ${p.size}, avg queue length: $avg%.2f, max_p: $maxP%.4f")
                dropped += 1
            } else if (q.enqueue(p)) {
                println(f"Packet enqueued, size: ${p.size}, avg queue length: $avg%.2f, max_p: $maxP%.4f")
                count = 0
            } else {
                println(f"Queue full, packet dropped, size: ${p.size}")
                dropped += 1
            }
        }
        println(s"Final queue length: ${q.size}")
        println(s"Packets dropped: ${dropped.toInt}")
        q.printQueue()
    }

    def main(args: Array[String]): Unit = {
        val rand = new Random(42)
        val packets = (1 to 200).map(_ => Packet(rand.nextInt(100) + 1))
        println("=== RED ===")
        simulateRed(packets, 20.0, 80.0, 0.002, 0.1, 100)
    }
}
