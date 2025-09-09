import random

R = 65536

def birthday_attack():
    table = {}
    trials = 0
    while True:
        x = random.getrandbits(64)
        h = x % R
        trials += 1
        if h in table and table[h] != x:
            print(f"Collision found after {trials} trials: x_i = {table[h]}, x_j = {x}, hash = {h}")
            break
        table[h] = x

birthday_attack()
