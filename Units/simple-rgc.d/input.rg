# A sample RGC program
def fib(n: int) -> int:
    if n <= 1:
        return n
    return fib(n - 1) + fib(n - 2)

class Counter:
    value: int = 0

    def increment(self) -> None:
        self.value += 1

enum Color:
    Red
    Green
    Blue

interface Shape:
    def area(self) -> float

def main() -> None:
    counter: Counter = @var Counter()
    for i in range(10):
        counter.increment()
