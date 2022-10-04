from time import time


class HighScore:
    def __init__(self, id, score):
        self.id = id
        self.score = score

    def __eq__(self, other):
        return isinstance(other, HighScore) and self.score == other.score

    def __lt__(self, other):
        if not isinstance(other, HighScore):
            return NotImplemented
        return self.score > other.score

    def __str__(self):
        return f"Id {self.id} Score {self.score}"


if __name__ == "__main__":
    ref_instant = time()
    users = [
        "cowsay",
        "catsay",
        "dogsay",
        "ducksay",
        "pigsay",
        "goatsay",
        "mousesay",
        "tigersay",
    ]
    board = []
    score = 1
    a = 884
    m = 8191
    for i in range(3000):
        score = score * a % m
        board.append(HighScore(users[i % 8], score))

    print(f"[{round(time() - ref_instant, 6):<8}] start sorting")
    board.sort()
    print(f"[{round(time() - ref_instant, 6):<8}] done")
    for item in board[:10]:
        print(item)
