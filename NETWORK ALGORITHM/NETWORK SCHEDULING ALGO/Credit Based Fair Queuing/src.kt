import kotlin.random.Random
import kotlin.math.max

data class Packet(val size: Int, val weight: Int, val flowId: Int)

class Queue(val capacity: Int, val creditRate: Double, val totalBandwidth: Int) {
    private val items = mutableListOf<Packet>()
    var credit = 0.0
        private set

    fun enqueue(p: Packet): Boolean {
        if (items.size >= capacity) return false
        items.add(p)
        return true
    }

    fun dequeue(): Packet = items.removeAt(0)

    fun updateCredit(deltaTime: Double) {
        credit += creditRate * deltaTime
        credit = max(0.0, credit)
    }

    fun spendCredit(amount: Double) {
        credit -= amount
        credit = max(0.0, credit)
    }

    fun size(): Int = items.size
}

fun simulateCbfq(packets: List<Packet>, capacity: Int, bandwidth: Int, baseRate: Double) {
    val weights = listOf(1.0, 2.0, 3.0, 4.0, 5.0)
    val queues = weights.map { Queue(capacity / 5, it * baseRate, bandwidth / 5) }
    var currentTime = 0.0
    val serviceRate = 1000.0
    var dropped = 0
    println("=== CBFQ Scheduler ===")
    println("Initial queue: empty")

    packets.forEach { p ->
        val queueIdx = p.flowId % 5
        queues[queueIdx].updateCredit(0.001)

        if (queues[queueIdx].enqueue(p)) {
            println("Packet enqueued, size: ${p.size}, weight: ${weights[queueIdx]}, flow_id: ${p.flowId}, credit: ${queues[queueIdx].credit}")
        } else {
            println("Queue full, packet dropped, size: ${p.size}")
            dropped++
        }

        var maxCredit = -1.0
        var maxIdx = -1
        queues.forEachIndexed { j, q ->
            if (q.size() > 0 && q.credit > maxCredit) {
                maxCredit = q.credit
                maxIdx = j
            }
        }
        if (maxIdx != -1) {
            val p = queues[maxIdx].dequeue()
            queues[maxIdx].spendCredit(serviceRate * 0.001)
            println("Packet dequeued, size: ${p.size}, weight: ${weights[maxIdx]}, flow_id: ${p.flowId}, credit: ${queues[maxIdx].credit}")
        }
        currentTime += 0.001
    }

    queues.forEachIndexed { queueIdx, q ->
        while (q.size() > 0) {
            val p = q.dequeue()
            q.spendCredit(serviceRate * 0.001)
            println("Packet dequeued, size: ${p.size}, weight: ${weights[queueIdx]}, flow_id: ${p.flowId}, credit: ${q.credit}")
        }
    }

    println("Final queue length: 0")
    println("Packets dropped: $dropped")
    println("Final queue: empty")
}

fun main() {
    Random.seed(42)
    val packets = List(200) { Packet(Random.nextInt(1, 101), Random.nextInt(1, 6), Random.nextInt(0, 5)) }
    simulateCbfq(packets, 100, 1000, 1.0)
}
