import kotlin.random.Random
import kotlin.math.sqrt

data class Packet(val size: Int, val priority: Int, val flowId: Int)

class Queue(val capacity: Int, val totalBandwidth: Int) {
    private val items = mutableListOf<Packet>()
    
    fun enqueue(p: Packet): Boolean {
        if (items.size >= capacity) return false
        items.add(p)
        return true
    }
    
    fun dequeue(): Packet = items.removeAt(0)
    
    fun size(): Int = items.size
}

class PRIQ(numQueues: Int, capacity: Int, bandwidth: Int) {
    private val queues = List(numQueues) { Queue(capacity / numQueues, bandwidth / numQueues) }
    
    fun enqueue(p: Packet) {
        val pri = p.priority % queues.size
        if (queues[pri].enqueue(p)) {
            println("Packet enqueued, size: ${p.size}, priority: $pri, flow_id: ${p.flowId}")
        } else {
            println("Queue full, packet dropped, size: ${p.size}")
        }
    }
    
    fun dequeueAll() {
        for (pri in queues.indices.reversed()) {
            while (queues[pri].size() > 0) {
                val p = queues[pri].dequeue()
                println("Packet dequeued, size: ${p.size}, priority: $pri, flow_id: ${p.flowId}")
            }
        }
    }
}

class CoDelQueue(capacity: Int, bandwidth: Int, val targetDelay: Double, val interval: Double) {
    private val q = Queue(capacity, bandwidth)
    private var firstAboveTime = 0.0
    private var dropNext = Double.POSITIVE_INFINITY
    private var dropCount = 0
    
    fun enqueue(p: Packet, currentTime: Double): Boolean {
        if (q.enqueue(p)) {
            println("Packet enqueued, size: ${p.size}, priority: ${p.priority}, flow_id: ${p.flowId}")
            processQueue(currentTime)
            return true
        }
        println("Queue full, packet dropped, size: ${p.size}")
        return false
    }
    
    private fun processQueue(currentTime: Double) {
        while (q.size() > 0) {
            val sojournTime = currentTime // Simplified
            if (sojournTime < targetDelay || q.size() <= 4) {
                val p = q.dequeue()
                println("Packet dequeued, size: ${p.size}, priority: ${p.priority}, flow_id: ${p.flowId}")
                firstAboveTime = 0.0
                dropNext = Double.POSITIVE_INFINITY
                dropCount = 0
            } else if (firstAboveTime == 0.0) {
                firstAboveTime = currentTime + interval
                dropNext = firstAboveTime
                val p = q.dequeue()
                println("Packet dequeued, size: ${p.size}, priority: ${p.priority}, flow_id: ${p.flowId}")
            } else if (currentTime >= dropNext) {
                val p = q.dequeue()
                println("Packet dropped, size: ${p.size}, priority: ${p.priority}, flow_id: ${p.flowId}")
                dropCount++
                dropNext = currentTime + interval / sqrt(dropCount.toDouble())
            } else {
                val p = q.dequeue()
                println("Packet dequeued, size: ${p.size}, priority: ${p.priority}, flow_id: ${p.flowId}")
                dropCount = 0
            }
        }
    }
    
    fun size(): Int = q.size()
}

class CBQ(numNodes: Int, capacity: Int, bandwidth: Int) {
    private val nodes = List(numNodes) { Queue(capacity / numNodes, bandwidth / numNodes) }
    
    fun enqueue(p: Packet) {
        val nodeIdx = p.flowId % nodes.size
        if (nodes[nodeIdx].enqueue(p)) {
            println("Packet enqueued, size: ${p.size}, priority: ${p.priority}, flow_id: ${p.flowId}")
        } else {
            println("Queue full, packet dropped, size: ${p.size}")
        }
    }
    
    fun dequeueAll() {
        nodes.forEach { node ->
            while (node.size() > 0) {
                val p = node.dequeue()
                println("Packet dequeued, size: ${p.size}, priority: ${p.priority}, flow_id: ${p.flowId}")
            }
        }
    }
}

class FairQ(numFlows: Int, capacity: Int, bandwidth: Int) {
    private val flowQueues = List(numFlows) { Queue(capacity / numFlows, bandwidth / numFlows) }
    
    fun enqueue(p: Packet) {
        val flow = p.flowId % flowQueues.size
        if (flowQueues[flow].enqueue(p)) {
            println("Packet enqueued, size: ${p.size}, priority: ${p.priority}, flow_id: $flow")
        } else {
            println("Queue full, packet dropped, size: ${p.size}")
        }
    }
    
    fun dequeueAll() {
        flowQueues.forEachIndexed { flow, queue ->
            while (queue.size() > 0) {
                val p = queue.dequeue()
                println("Packet dequeued, size: ${p.size}, priority: ${p.priority}, flow_id: $flow")
            }
        }
    }
}

class HFSC(numNodes: Int, capacity: Int, bandwidth: Int) {
    private val nodes = List(numNodes) { Queue(capacity / numNodes, bandwidth / numNodes) }
    
    fun enqueue(p: Packet) {
        val nodeIdx = p.priority % nodes.size
        if (nodes[nodeIdx].enqueue(p)) {
            println("Packet enqueued, size: ${p.size}, priority: ${p.priority}, flow_id: ${p.flowId}")
        } else {
            println("Queue full, packet dropped, size: ${p.size}")
        }
    }
    
    fun dequeueAll() {
        nodes.forEach { node ->
            while (node.size() > 0) {
                val p = node.dequeue()
                println("Packet dequeued, size: ${p.size}, priority: ${p.priority}, flow_id: ${p.flowId}")
            }
        }
    }
}

fun simulateAltq(packets: List<Packet>, capacity: Int, bandwidth: Int) {
    println("=== ALTQ Schedulers Simulation ===")

    println("\n=== PRIQ Scheduler ===")
    println("Initial queue: empty")
    val priq = PRIQ(16, capacity, bandwidth)
    var dropped = 0
    packets.forEach { priq.enqueue(it) }
    priq.dequeueAll()
    println("Final queue length: 0")
    println("Packets dropped: $dropped")
    println("Final queue: empty")

    println("\n=== CoDel Scheduler ===")
    println("Initial queue: empty")
    val codel = CoDelQueue(capacity, bandwidth, 5.0, 100.0)
    var currentTime = 0.0
    packets.forEach {
        if (!codel.enqueue(it, currentTime)) dropped++
        currentTime += 1.0
    }
    println("Final queue length: ${codel.size()}")
    println("Packets dropped: $dropped")
    println("Final queue: empty")

    println("\n=== CBQ Scheduler ===")
    println("Initial queue: empty")
    val cbq = CBQ(4, capacity, bandwidth)
    packets.forEach { cbq.enqueue(it) }
    cbq.dequeueAll()
    println("Final queue length: 0")
    println("Packets dropped: $dropped")
    println("Final queue: empty")

    println("\n=== FairQ Scheduler ===")
    println("Initial queue: empty")
    val fairq = FairQ(5, capacity, bandwidth)
    packets.forEach { fairq.enqueue(it) }
    fairq.dequeueAll()
    println("Final queue length: 0")
    println("Packets dropped: $dropped")
    println("Final queue: empty")

    println("\n=== HFSC Scheduler ===")
    println("Initial queue: empty")
    val hfsc = HFSC(4, capacity, bandwidth)
    packets.forEach { hfsc.enqueue(it) }
    hfsc.dequeueAll()
    println("Final queue length: 0")
    println("Packets dropped: $dropped")
    println("Final queue: empty")
}

fun main() {
    Random.seed(42)
    val packets = List(200) { Packet(Random.nextInt(1, 101), Random.nextInt(1, 16), Random.nextInt(1, 6)) }
    simulateAltq(packets, 100, 1000)
}
