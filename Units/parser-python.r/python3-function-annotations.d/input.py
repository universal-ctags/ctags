
def func01() -> "returns something":
    def sub01(): pass

def func02() -> 2 * 2 * 3 * 4:
    def sub02(): pass

def func03(a:"hello", b : 1 * 2 + 3 / 4 = 5):
    def sub03(): pass

def func04(a:(1, 2, 3, 4)=(1, 2, 3, 4), b:[5,6]=[7,8], c=0) -> {'foo' : 'bar' }:
    def sub04(): pass


# test
for f in (
func01,
func02,
func03,
func04,
):
    print(f.__name__, f.__annotations__)
