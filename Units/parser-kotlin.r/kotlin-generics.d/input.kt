interface Foo<T> {
    fun<T: Any> genericFunction() = 42
}

class Bar<T, U>(val a: T, val b: U): Foo<T>
