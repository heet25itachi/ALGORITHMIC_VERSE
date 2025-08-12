import scala.util.Random

// Define Packet and Queue classes
case class Packet(size: Int, var arrivalTime: Double = 0.0)
class Queue(capacity: Int) {
    private val items = scala.collection.mutable.ArrayBuffer[Packet]()
    def enqueue(p: Packet): Boolean = {
        if (items.size >= capacity) false
        else { items += p; true }
    }
    def dequeue(): Packet = items.remove(0)
    def peek(): Packet = items(0)
    def size: Int = items.size
}

// Simulate PIE algorithm
object PIE {
    def simulatePie(packets: Seq[Packet], target: Double, updateInterval: Double, alpha: Double, beta: Double, maxDropProb: Double, maxBurst: Double, capacity: Int): Unit = {
        val q = new Queue(capacity)
        var currentTime = 0.0
        var lastUpdate = 0.0
        var dropProb = 0.0
        var prevDelay = 0.0
        var burstTime = maxBurst
        var dropped = 0
        val rand = new Random(42)
        println("Initial queue: empty")

        packets.foreach { p =>
            p.arrivalTime = currentTime
            val delay = if (q.size == 0) 0.0 else currentTime - q.peek().arrivalTime

            if (currentTime - lastUpdate >= updateInterval) {
                val error = delay - target
                dropProb += alpha * error + beta * (delay - prevDelay)
                dropProb = math.max(0.0, math.min(maxDropProb, dropProb))
                prevDelay = delay
                lastUpdate = currentTime
                if (delay > target) burstTime = 0.0
                else if (burstTime < maxBurst) burstTime += updateInterval
            }

            val drop = burstTime < maxBurst && delay > target && rand.nextDouble() < dropProb

            if (drop) {
                println(f"Packet dropped, size: ${p.size}, queue delay: $delay%.2f, drop prob: $dropProb%.4f")
                dropped += 1
            } else if (q.enqueue(p)) {
                println(f"Packet enqueued, size: ${p.size}, queue delay: $delay%.2f, drop prob: $dropProb%.4f")
                val deqP = q.dequeue()
                println(f"Packet dequeued, size: ${deqP.size}, queue delay: $delay%.2f")
            } else {
                println(f"Queue full, packet dropped, size: ${p.size}")
                dropped += 1
            }
            currentTime += 1.0
        }

        while (q.size > 0) {
            val delay = currentTime - q.peek().arrivalTime
            if (currentTime - lastUpdate >= updateInterval) {
                val error = delay - target
                dropProb += alpha * error + beta * (delay - prevDelay)
                dropProb = math.max(0.0, math.min(maxDropProb, dropProb))
                prevDelay = delay
                lastUpdate = currentTime
                if (delay > target) burstTime = 0.0
                else if (burstTime < maxBurst) burstTime += updateInterval
            }
            val deqP = q.dequeue()
            println(f"Packet dequeued, size: ${deqP.size}, queue delay: $delay%.2f")
            currentTime += 1.0
        }

        println(s"Final queue length: ${q.size}")
        println(s"Packets dropped: $dropped")
        println("Final queue: empty")
    }

    def main(args: Array[String]): Unit = {
        val rand = new Random(42)
        val packets = (1 to 200).map(_ => Packet(rand.nextInt(100) + 1))
        println("=== PIE ===")
        simulatePie(packets, 15.0, 30.0, 0.125, 1.25, 0.1, 150.0, 100)
    }
}
