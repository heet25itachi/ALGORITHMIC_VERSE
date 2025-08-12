import kotlin.random.Random
import kotlin.math.max
import kotlin.math.min

data class Packet(var size: Int, var arrivalTime: Double = 0.0)

class Queue(val capacity: Int) {
    private val items = mutableListOf<Packet>()
    fun enqueue(p: Packet): Boolean {
        if (items.size >= capacity) return false
        items.add(p)
        return true
    }
    fun dequeue(): Packet = items.removeAt(0)
    fun peek(): Packet = items[0]
    fun size(): Int = items.size
}

fun simulatePie(packets: List<Packet>, target: Double, updateInterval: Double, alpha: Double, beta: Double, maxDropProb: Double, maxBurst: Double, capacity: Int) {
    val q = Queue(capacity)
    var currentTime = 0.0
    var lastUpdate = 0.0
    var dropProb = 0.0
    var prevDelay = 0.0
    var burstTime = maxBurst
    var dropped = 0
    val rand = Random(42)
    println("Initial queue: empty")

    packets.forEach { p ->
        p.arrivalTime = currentTime
        val delay = if (q.size() == 0) 0.0 else currentTime - q.peek().arrivalTime

        if (currentTime - lastUpdate >= updateInterval) {
            val error = delay - target
            dropProb += alpha * error + beta * (delay - prevDelay)
            dropProb = max(0.0, min(maxDropProb, dropProb))
            prevDelay = delay
            lastUpdate = currentTime
            if (delay > target) burstTime = 0.0
            else if (burstTime < maxBurst) burstTime += updateInterval
        }

        val drop = burstTime < maxBurst && delay > target && rand.nextDouble() < dropProb

        if (drop) {
            println("Packet dropped, size: ${p.size}, queue delay: %.2f, drop prob: %.4f".format(delay, dropProb))
            dropped++
        } else if (q.enqueue(p)) {
            println("Packet enqueued, size: ${p.size}, queue delay: %.2f, drop prob: %.4f".format(delay, dropProb))
            val deqP = q.dequeue()
            println("Packet dequeued, size: ${deqP.size}, queue delay: %.2f".format(delay))
        } else {
            println("Queue full, packet dropped, size: ${p.size}")
            dropped++
        }
        currentTime += 1.0
    }

    while (q.size() > 0) {
        val delay = currentTime - q.peek().arrivalTime
        if (currentTime - lastUpdate >= updateInterval) {
            val error = delay - target
            dropProb += alpha * error + beta * (delay - prevDelay)
            dropProb = max(0.0, min(maxDropProb, dropProb))
            prevDelay = delay
            lastUpdate = currentTime
            if (delay > target) burstTime = 0.0
            else if (burstTime < maxBurst) burstTime += updateInterval
        }
        val deqP = q.dequeue()
        println("Packet dequeued, size: ${deqP.size}, queue delay: %.2f".format(delay))
        currentTime += 1.0
    }

    println("Final queue length: ${q.size()}")
    println("Packets dropped: $dropped")
    println("Final queue: empty")
}

fun main() {
    val rand = Random(42)
    val packets = List(200) { Packet(rand.nextInt(1, 101)) }
    println("=== PIE ===")
    simulatePie(packets, 15.0, 30.0, 0.125, 1.25, 0.1, 150.0, 100)
}
