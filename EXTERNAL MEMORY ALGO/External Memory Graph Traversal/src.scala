object ExternalBFS {
  def removeDuplicates(arr: Array[Int]): Array[Int] = {
    arr.sorted.distinct
  }

  def setDifference(a: Array[Int], b: Array[Int]): Array[Int] = {
    a.filterNot(b.toSet)
  }

  def externalBFS(adj: Array[Array[Int]], start: Int, M: Int, B: Int): Unit = {
    val n = adj.length
    val visited = Array.fill(n)(false)
    val level = Array.fill(M)(0)
    val prevLevel = Array.fill(M)(0)
    val prevPrevLevel = Array.fill(M)(0)
    var levelSize = 0
    var prevLevelSize = 0
    var prevPrevLevelSize = 0
    var t = 0

    // Initialize L(0) = {start}
    level(0) = start
    levelSize = 1
    visited(start) = true
    println(s"Level $t: $start")

    while (levelSize > 0) {
      // Step 1: Compute A(t) = neighbors of L(t-1)
      val neighbors = (0 until levelSize).flatMap(i => adj(level(i))).toArray

      // Step 2: Compute A'(t) by removing duplicates
      val uniqueNeighbors = removeDuplicates(neighbors)

      // Step 3: Compute L(t) = A'(t) \ (L(t-1) \cup L(t-2))
      val temp = prevLevel.take(prevLevelSize) ++ prevPrevLevel.take(prevPrevLevelSize)
      val tempUnique = removeDuplicates(temp)
      val newLevel = setDifference(uniqueNeighbors, tempUnique)

      // Update visited
      newLevel.foreach(v => visited(v) = true)

      // Print current level
      if (newLevel.nonEmpty) {
        println(s"Level ${t + 1}: ${newLevel.mkString(" ")}")
      }

      // Update levels
      prevPrevLevelSize = prevLevelSize
      Array.copy(prevLevel, 0, prevPrevLevel, 0, prevLevelSize)
      prevLevelSize = levelSize
      Array.copy(level, 0, prevLevel, 0, levelSize)
      levelSize = newLevel.length
      Array.copy(newLevel, 0, level, 0, newLevel.length)
      t += 1
    }
  }

  def main(args: Array[String]): Unit = {
    val n = 10
    val adj = Array.fill(n)(Array[Int]())
    val edges = Array(
      (0,1), (0,3), (0,9), (1,0), (1,2), (1,4), (2,1), (2,3),
      (3,0), (3,2), (3,4), (4,1), (4,3), (4,5), (5,4), (5,6),
      (5,8), (6,5), (6,7), (7,6), (7,8), (8,5), (8,7), (8,9),
      (9,0), (9,8)
    )
    for ((u, v) <- edges) {
      adj(u) = adj(u) :+ v
    }

    println("Adjacency lists:")
    adj.zipWithIndex.foreach { case (list, i) => println(s"$i: ${list.mkString(" ")}") }

    externalBFS(adj, 0, 5, 2)
  }
}
