import kotlin.random.Random

// Define Packet and Queue classes
data class Packet(val size: Int)
class Queue(val capacity: Int) {
    private val items = mutableListOf<Packet>()
    fun enqueue(p: Packet): Boolean {
        if (items.size >= capacity) return false
        items.add(p)
        return true
    }
    fun size(): Int = items.size
    fun printQueue() {
        print("Final queue: ")
        items.forEach { print("${it.size} ") }
        println()
    }
}

// Simulate RED algorithm
fun simulateRed(packets: List<Packet>, minTh: Double, maxTh: Double, wq: Double, maxP: Double, capacity: Int) {
    val q = Queue(capacity)
    var avg = 0.0
    var count = 0.0
    var dropped = 0.0
    val rand = Random(42)
    println("Initial queue: empty")
    packets.forEach { p ->
        avg = if (q.size() == 0) 0.0 else (1 - wq) * avg + wq * q.size()
        val drop = when {
            avg < minTh -> false
            avg >= maxTh -> true
            else -> {
                val pb = maxP * (avg - minTh) / (maxTh - minTh)
                val pa = pb / (1 - count * pb)
                count++
                rand.nextDouble() < pa
            }
        }
        if (drop) {
            println("Packet dropped, size: ${p.size}, avg queue length: %.2f, max_p: %.4f".format(avg, maxP))
            dropped++
        } else if (q.enqueue(p)) {
            println("Packet enqueued, size: ${p.size}, avg queue length: %.2f, max_p: %.4f".format(avg, maxP))
            count = 0.0
        } else {
            println("Queue full, packet dropped, size: ${p.size}")
            dropped++
        }
    }
    println("Final queue length: ${q.size()}")
    println("Packets dropped: ${dropped.toInt()}")
    q.printQueue()
}

fun main() {
    val rand = Random(42)
    val packets = List(200) { Packet(rand.nextInt(1, 101)) }
    println("=== RED ===")
    simulateRed(packets, 20.0, 80.0, 0.002, 0.1, 100)
}
