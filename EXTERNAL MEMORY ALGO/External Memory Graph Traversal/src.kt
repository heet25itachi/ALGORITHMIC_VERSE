fun removeDuplicates(arr: MutableList<Int>) {
    arr.sort()
    val result = mutableListOf<Int>()
    for (i in arr.indices) {
        if (i == 0 || arr[i] != arr[i - 1]) {
            result.add(arr[i])
        }
    }
    arr.clear()
    arr.addAll(result)
}

fun setDifference(a: List<Int>, b: List<Int>): List<Int> {
    val bSet = b.toSet()
    return a.filter { it !in bSet }
}

fun externalBFS(adj: List<List<Int>>, start: Int, M: Int, B: Int) {
    val n = adj.size
    val visited = BooleanArray(n)
    val level = MutableList(M) { 0 }
    val prevLevel = MutableList(M) { 0 }
    val prevPrevLevel = MutableList(M) { 0 }
    var levelSize = 0
    var prevLevelSize = 0
    var prevPrevLevelSize = 0
    var t = 0

    // Initialize L(0) = {start}
    level[0] = start
    levelSize = 1
    visited[start] = true
    println("Level $t: $start")

    while (levelSize > 0) {
        // Step 1: Compute A(t) = neighbors of L(t-1)
        val neighbors = mutableListOf<Int>()
        for (i in 0 until levelSize) {
            val v = level[i]
            neighbors.addAll(adj[v])
        }

        // Step 2: Compute A'(t) by removing duplicates
        removeDuplicates(neighbors)

        // Step 3: Compute L(t) = A'(t) \ (L(t-1) \cup L(t-2))
        val temp = prevLevel.take(prevLevelSize) + prevPrevLevel.take(prevPrevLevelSize)
        val tempUnique = temp.toMutableList()
        removeDuplicates(tempUnique)
        val newLevel = setDifference(neighbors, tempUnique)

        // Update visited
        newLevel.forEach { visited[it] = true }

        // Print current level
        if (newLevel.isNotEmpty()) {
            println("Level ${t + 1}: ${newLevel.joinToString(" ")}")
        }

        // Update levels
        prevPrevLevelSize = prevLevelSize
        prevPrevLevel.clear()
        prevPrevLevel.addAll(prevLevel.take(prevLevelSize))
        prevLevelSize = levelSize
        prevLevel.clear()
        prevLevel.addAll(level.take(levelSize))
        levelSize = newLevel.size
        level.clear()
        level.addAll(newLevel)
        t++
    }
}

fun main() {
    val n = 10
    val adj = MutableList(n) { mutableListOf<Int>() }
    val edges = listOf(
        0 to 1, 0 to 3, 0 to 9, 1 to 0, 1 to 2, 1 to 4, 2 to 1, 2 to 3,
        3 to 0, 3 to 2, 3 to 4, 4 to 1, 4 to 3, 4 to 5, 5 to 4, 5 to 6,
        5 to 8, 6 to 5, 6 to 7, 7 to 6, 7 to 8, 8 to 5, 8 to 7, 8 to 9,
        9 to 0, 9 to 8
    )
    for ((u, v) in edges) adj[u].add(v)

    println("Adjacency lists:")
    adj.forEachIndexed { i, list -> println("$i: ${list.joinToString(" ")}") }

    externalBFS(adj, 0, 5, 2)
}
