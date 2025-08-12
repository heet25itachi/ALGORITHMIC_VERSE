import kotlin.random.Random
import kotlin.math.sqrt

data class Packet(var size: Int, var arrivalTime: Double = 0.0)

class CoDelQueue(capacity: Int) {
    private val items = mutableListOf<Packet>()
    private var firstAboveTime = 0.0
    private var dropNext = Double.POSITIVE_INFINITY
    private var dropCount = 0

    fun enqueue(p: Packet): Boolean {
        if (items.size >= capacity) {
            println("Queue full, packet dropped, size: ${p.size}")
            return false
        }
        items.add(p)
        println("Packet enqueued, size: ${p.size}")
        return true
    }

    fun dequeue(currentTime: Double, target: Double, interval: Double) {
        while (items.isNotEmpty()) {
            val p = items[0]
            val sojournTime = currentTime - p.arrivalTime

            if (sojournTime < target || items.size <= 4) {
                firstAboveTime = 0.0
                dropNext = Double.POSITIVE_INFINITY
                items.removeAt(0)
                println("Packet dequeued, size: ${p.size}, sojourn time: %.2f".format(sojournTime))
                dropCount = 0
            } else if (firstAboveTime == 0.0) {
                firstAboveTime = currentTime + interval
                dropNext = firstAboveTime
                items.removeAt(0)
                println("Packet dequeued, size: ${p.size}, sojourn time: %.2f".format(sojournTime))
            } else if (currentTime >= dropNext) {
                items.removeAt(0)
                println("Packet dropped, size: ${p.size}, sojourn time: %.2f".format(sojournTime))
                dropCount += 1
                dropNext = currentTime + interval / sqrt(dropCount.toDouble())
            } else {
                items.removeAt(0)
                println("Packet dequeued, size: ${p.size}, sojourn time: %.2f".format(sojournTime))
                dropCount = 0
            }
        }
    }
}

fun simulateCoDel(packets: List<Packet>, target: Double, interval: Double, capacity: Int) {
    val q = CoDelQueue(capacity)
    var currentTime = 0.0
    var dropped = 0
    println("Initial queue: empty")

    packets.forEach { p ->
        p.arrivalTime = currentTime
        if (q.enqueue(p)) {
            q.dequeue(currentTime, target, interval)
        } else {
            dropped += 1
        }
        currentTime += 1.0
    }

    q.dequeue(currentTime, target, interval)

    println("Final queue length: ", q.size())
    println("Packets dropped: ", dropped)
    println("Final queue: empty")
}

fun main() {
    val rand = Random(42)
    val packets = List(200) { Packet(rand.nextInt(1, 101)) }

    simulateCoDel(packets, 5.0, 100.0, 100)
}
