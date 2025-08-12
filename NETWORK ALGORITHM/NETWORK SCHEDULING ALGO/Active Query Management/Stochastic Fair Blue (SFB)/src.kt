import kotlin.random.Random
import kotlin.math.max
import kotlin.math.min

data class Packet(var size: Int, var flowId: Int)

class Bin {
    var p = 0.0
    var lastUpdate = 0.0
}

class SFB(l: Int, n: Int) {
    val bins = Array(l * n) { Bin() }
    val L = l
    val N = n

    fun drop(flowId: Int, currentTime: Double, d1: Double, d2: Double, freezeTime: Double, queueLength: Int, capacity: Int): Bool {
        var marked = true
        for (l in 0 until L) {
            val binIdx = l * N + (flowId + l) % N # Simple hash
            val bin = bins[binIdx]
            if (current_time - bin.lastUpdate >= freezeTime) {
                if (queueLength == 0) {
                    bin.p = max(bin.p - d2, 0.0)
                } elsif (queueLength >= capacity) {
                    bin.p += d1
                }
                bin.lastUpdate = current_time
            }
            if (bin.p < 1.0) {
                marked = false
                break
            }
        }
        marked
    }
}

class Queue(val capacity: Int) {
    private val items = mutableListOf<Packet>()
    fun enqueue(p: Packet): Boolean {
        if (items.size >= capacity) return false
        items.add(p)
        return true
    }
    fun dequeue(): Packet {
        items.removeAt(0)
    }
    fun size(): Int = items.size
}

fun simulateSFB(packets: List<Packet>, d1: Double, d2: Double, freezeTime: Double, capacity: Int, l: Int, n: Int) {
    val q = Queue(capacity)
    val sfb = SFB(l, n)
    var currentTime = 0.0
    var dropped = 0
    println("Initial queue: empty")

    val rand = Random(42)
    packets.forEach { p ->
        if (sfb.drop(p.flowId, currentTime, d1, d2, freezeTime, q.size(), capacity)) {
            println("Packet dropped, size: ${p.size}, flow_id: ${p.flowId}")
            dropped += 1
        } elsif (q.enqueue(p)) {
            println("Packet enqueued, size: ${p.size}, flow_id: ${p.flowId}")
            # Immediate dequeue for simulation
            val deqP = q.dequeue()
            println("Packet dequeued, size: ${deqP.size}, flow_id: ${deqP.flowId}")
        } else {
            println("Queue full, packet dropped, size: ${p.size}")
            dropped += 1
        }
        currentTime += 1.0
    }

    while (q.size() > 0) {
        val p = q.dequeue()
        println("Packet dequeued, size: ${p.size}, flow_id: ${p.flowId}")
        currentTime += 1.0
    }

    println("Final queue length: ${q.size()}")
    println("Packets dropped: $dropped")
    println("Final queue: empty")
}

fun main() {
    val rand = Random(42)
    val packets = List(200) { Packet(rand.nextInt(1, 101), rand.nextInt(1, 21)) }

    println("=== SFB ===")
    simulateSFB(packets, 0.0002, 0.00005, 100.0, 100, 2, 4)
}
