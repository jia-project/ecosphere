from random import randrange
from time import time

def main():
    xs = [randrange(0, 1 << 32) for _ in range(1 << 20)]
    xs[0] = 0.1
    zero_instant = time()
    print(f"{time() - zero_instant:.6f}s Sort start")
    xs.sort()
    print(f"{time() - zero_instant:.6f}s Sort finish")


assert __name__ == '__main__'
main()
