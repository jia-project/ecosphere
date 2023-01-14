from time import time

class XorShiftRng:
    def __init__(self, x, y, z, w):
        self.x = x
        self.y = y
        self.z = z
        self.w = w

    def next(self):
        t = (self.x ^ (self.x << 11)) & ((1 << 32) - 1)
        self.x = self.y
        self.y = self.z
        self.z = self.w
        self.w = (self.w ^ (self.w >> 19) ^ (t ^ (t >> 8))) & ((1 << 32) - 1)
        return self.w

def checksum(xs):
    sum1, sum2 = 0, 0
    for x in xs:
        sum1 = (sum1 + x) % ((1 << 16) - 1)
        sum2 = (sum1 + sum2) % ((1 << 16) - 1)
    return (sum1 << 16) | sum2

def main():
    zero_instant = time()
    rng = XorShiftRng(1, 2, 3, 4)
    xs = [(1 << 60) + rng.next() for _ in range(1 << 20)]
    print(f"{time() - zero_instant:.6f}s Generated")
    c = checksum(xs)
    print(f"{time() - zero_instant:.6f}s Sort start Checksum {c:x}")
    xs.sort()
    print(f"{time() - zero_instant:.6f}s Sort finish")
    c = checksum(xs)
    print(f"{time() - zero_instant:.6f}s Checksum {c:x}")

assert __name__ == '__main__'
main()
