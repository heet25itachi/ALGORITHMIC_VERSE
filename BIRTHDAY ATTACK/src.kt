import kotlin.random.Random

const val R = 65536

fun birthdayAttack() {
    val table = mutableMapOf<Int, Long>()
    var trials = 0
    while (true) {
        val x = Random.nextLong()
        val h = (x % R).toInt()
        trials++
        if (table.containsKey(h) && table[h] != x) {
            println("Collision found after $trials trials: x_i = ${table[h]}, x_j = $x, hash = $h")
            break
        }
        table[h] = x
    }
}

fun main() {
    birthdayAttack()
}
