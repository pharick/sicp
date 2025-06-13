from typing import Callable


def make_pair(x: int, y: int) -> Callable[[Callable[[int, int], int]], int]:
    return (lambda m: m(x, y))


def first(pair: Callable[[Callable[[int, int], int]], int]) -> int:
    return pair(lambda x, _: x)


def second(pair: Callable[[Callable[[int, int], int]], int]) -> int:
    return pair(lambda _, y: y)


def main():
    pair = make_pair(3, 4)
    print("First element:", first(pair))  # Output: 3
    print("Second element:", second(pair))  # Output: 4


if __name__ == "__main__":
    main()
