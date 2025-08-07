import kotlin.random.Random
import kotlin.math.max

data class Packet(val size: Int, var arrivalTime: Double = 0.0)

class Queue(val capacity: Int) {
    private val items = mutableListOf<Packet>()
    fun enqueue(p: Packet): Boolean {
        if (items.size >= capacity) return false
        items.add(p)
        return true
    }
    fun size(): Int = items.size
    fun printQueue() {
        println("Final queue: ${items.joinToString(" ") { it.size.toString() }}")
    }
}

fun simulateARED(packets: List<Packet>, minTh: Double, maxTh: Double, wq: Double, target: Double, alpha: Double, beta: Double, interval: Double, capacity: Int) {
    val q = Queue(capacity)
    var avg = 0.0
    var maxP = 0.1
    var lastUpdate = 0.0
    var currentTime = 0.0
    var count = 0
    var dropped = 0
    println("Initial queue: empty")

    packets.forEach { p ->
        avg = if (q.size() == 0) 0.0 else (1 - wq) * avg + wq * q.size()
        if (currentTime - lastUpdate >= interval) {
            if (avg > target && maxP <= 0.5) maxP *= (1 + alpha)
            else if (avg < target && maxP >= 0.01) maxP *= beta
            lastUpdate = currentTime
        }

        val drop = when {
            avg < minTh -> false
            avg >= maxTh -> true
            else -> {
                val pb = maxP * (avg - minTh) / (maxTh - minTh)
                val pa = pb / (1 - count * pb)
                count++
                Random.nextDouble() < pa
            }
        }

        if (drop) {
            println("Packet dropped, size: ${p.size}, avg queue length: %.2f, max_p: %.4f".format(avg, maxP))
            dropped++
        } else if (q.enqueue(p)) {
            println("Packet enqueued, size: ${p.size}, avg queue length: %.2f, max_p: %.4f".format(avg, maxP))
            count = 0
        } else {
            println("Queue full, packet dropped, size: ${p.size}")
            dropped++
        }
        currentTime += 1.0
    }

    println("Final queue length: ${q.size()}")
    println("Packets dropped: $dropped")
    q.printQueue()
}

fun simulateBlue(packets: List<Packet>, d1: Double, d2: Double, freezeTime: Double, capacity: Int) {
    val q = Queue(capacity)
    var p = 0.0
    var lastUpdate = 0.0
    var currentTime = 0.0
    var dropped = 0
    println("Initial queue: empty")

    packets.forEach { p ->
        if (q.size() >= capacity) {
            p += d1
            lastUpdate = currentTime
            println("Queue full, packet dropped, size: ${p.size}, drop prob: %.4f".format(p))
            dropped++
        } else {
            if (currentTime - lastUpdate >= freezeTime && q.size() == 0) {
                p = max(p - d2, 0.0)
                lastUpdate = currentTime
            }
            if (Random.nextDouble() < p) {
                println("Packet dropped, size: ${p.size}, drop prob: %.4f".format(p))
                dropped++
            } else if (q.enqueue(p)) {
                println("Packet enqueued, size: ${p.size}, drop prob: %.4f".format(p))
            }
        }
        currentTime += 1.0
    }

    println("Final queue length: ${q.size()}")
    println("Packets dropped: $dropped")
    q.printQueue()
}

fun simulatePI(packets: List<Packet>, qRef: Double, a: Double, b: Double, capacity: Int) {
    val q = Queue(capacity)
    var p = 0.0
    var prevError = 0.0
    var currentTime = 0.0
    var dropped = 0
    println("Initial queue: empty")

    packets.forEach { p ->
        val error = q.size() - qRef
        p += a * error - b * prevError
        prevError = error
        if (p < 0.0) p = 0.0 else if (p > 1.0) p = 1.0

        if (Random.nextDouble() < p) {
            println("Packet dropped, size: ${p.size}, drop prob: %.4f".format(p))
            dropped++
        } else if (q.enqueue(p)) {
            println("Packet enqueued, size: ${p.size}, drop prob: %.4f".format(p))
        } else {
            println("Queue full, packet dropped, size: ${p.size}")
            dropped++
        }
        currentTime += 1.0
    }

    println("Final queue length: ${q.size()}")
    println("Packets dropped: $dropped")
    q.printQueue()
}

fun main() {
    val n = 200
    val capacity = 100
    val packets = List(n) { Packet(Random.nextInt(1, 101)) }

    println("=== ARED ===")
    simulateARED(packets, 20.0, 80.0, 0.002, 50.0, 0.01, 0.9, 1000.0, capacity)
    println("\n=== Blue ===")
    simulateBlue(packets, 0.0002, 0.00005, 100.0, capacity)
    println("\n=== PI ===")
    simulatePI(packets, 50.0, 0.00001822, 0.00001816, capacity)
}
