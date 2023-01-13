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
        self.w = (self.w ^ (self.w >> 19) ^ (t ^ (t >> 8))) & ((1 << 32) -1)
        return self.w

def main():
    rng = XorShiftRng(1, 2, 3, 4)
    xs = [rng.next() for _ in range(1 << 20)]
    zero_instant = time()
    print(f"{time() - zero_instant:.6f}s Sort start")
    xs.sort()
    print(f"{time() - zero_instant:.6f}s Sort finish")


assert __name__ == '__main__'
main()
