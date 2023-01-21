from time import time
from sys import argv


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


class ListView:
    def __init__(self, data, offset, length):
        self.data = data
        self.offset = offset
        self.length = length

    @staticmethod
    def of(data, offset, length):
        if isinstance(data, list):
            assert offset + length <= len(data)
            return ListView(data, offset, length)
        else:
            assert offset + length <= data.length
            return ListView(data.data, data.offset + offset, length)

    def __getitem__(self, i):
        return self.data[i + self.offset]

    def __setitem__(self, i, element):
        self.data[i + self.offset] = element

    def __len__(self):
        return self.length

    def clone(self):
        return self.data[self.offset : self.offset + self.length]


def reverse(xs):
    low, high = 0, len(xs) - 1
    while low < high:
        xs[low], xs[high] = xs[high], xs[low]
        low += 1
        high -= 1


class Run:
    def __init__(self, start, length):
        self.start = start
        self.length = length


def merge_sort(v):
    MIN_RUN = 10

    length = len(v)
    runs = [None] * (length // MIN_RUN + 1)
    runs_len = 0
    end = length
    while end > 0:
        start = end - 1
        if start > 0:
            start -= 1
            if v[start + 1] < v[start]:
                while start > 0 and v[start] < v[start - 1]:
                    start -= 1
                reverse(ListView.of(v, start, end - start))
            else:
                while start > 0 and not v[start] < v[start - 1]:
                    start -= 1

        while start > 0 and end - start < MIN_RUN:
            start -= 1
            insert_head(ListView.of(v, start, end - start))

        runs[runs_len] = Run(start, end - start)
        runs_len += 1
        end = start

        while (r := collapse(ListView.of(runs, 0, runs_len))) is not None:
            left = runs[r + 1]
            right = runs[r]
            merge(ListView.of(v, left.start, left.length + right.length), left.length)
            runs[r] = Run(left.start, left.length + right.length)
            runs_len -= 1
            runs[r + 1], runs[runs_len] = runs[runs_len], runs[r + 1]

    assert runs_len == 1
    assert runs[0].start == 0
    assert runs[0].length == length


def insert_head(a):
    length = len(a)
    if not (length >= 2 and a[1] < a[0]):
        return
    x = a[0]
    a[0] = a[1]
    d = 1
    i = 2
    while i < length:
        if not a[i] < x:
            break
        a[i - 1] = a[i]
        d = i
        i += 1
    a[d] = x


def merge(a, mid):
    length = len(a)
    if mid <= length - mid:
        buf = ListView.of(a, 0, mid).clone()
        left_i, left_end, right_i, right_end = 0, mid, mid, length
        i = 0
        while left_i < left_end and right_i < right_end:
            if a[right_i] < buf[left_i]:
                to_copy = a[right_i]
                right_i += 1
            else:
                to_copy = buf[left_i]
                left_i += 1
            a[i] = to_copy
            i += 1
        while left_i < left_end:
            a[i] = buf[left_i]
            left_i += 1
            i += 1
    else:
        buf = ListView.of(a, mid, length - mid).clone()
        left_i, left_end, right_i, right_end = mid, 0, length - mid, 0
        i = length
        while left_end < left_i and right_end < right_i:
            if buf[right_i - 1] < a[left_i - 1]:
                left_i -= 1
                to_copy = a[left_i]
            else:
                right_i -= 1
                to_copy = buf[right_i]
            i -= 1
            a[i] = to_copy
        while right_end < right_i:
            right_i -= 1
            i -= 1
            a[i] = buf[right_i]


def collapse(runs):
    n = len(runs)
    if n >= 2 and (
        runs[n - 1].start == 0
        or runs[n - 2].length <= runs[n - 1].length
        or (n >= 3 and runs[n - 3].length <= runs[n - 2].length + runs[n - 1].length)
        or (n >= 4 and runs[n - 4].length <= runs[n - 3].length + runs[n - 2].length)
    ):
        if n >= 3 and runs[n - 3].length < runs[n - 1].length:
            return n - 3
        else:
            return n - 2


def main():
    zero_instant = time()
    rng = XorShiftRng(1, 2, 3, 4)
    xs = [(1 << 60) + rng.next() for _ in range(1 << 20)]
    print(f"[{time() - zero_instant:.6f}s] Generated")
    c = checksum(xs)
    print(f"[{time() - zero_instant:.6f}s] Sort start Checksum {c:x}")
    if argv[1:] == ["native"]:
        xs.sort()
    else:
        merge_sort(xs)
    print(f"[{time() - zero_instant:.6f}s] Sort finish")
    c = checksum(xs)
    print(f"[{time() - zero_instant:.6f}s] Checksum {c:x}")


assert __name__ == "__main__"
main()
