package com.example.test

object TestObject {
    val a = 1
    fun f1(): String {
        val b = "hi"
        return b
    }
}

class TestClass(val c: String, var d: Int) {
    val e = 2
    fun f2(): String {
        var f = 42 * 12
        return f.toString()
    }
}

interface TestInterface {
    abstract val g: Int
    fun f3(): String {
        val lmbd: () -> String = {
            val h = "B".toLowerCase()
            h
        }
        return lmbd()
    }
}

val anonymousFunction = fun(x: Int, y: Int): Int {
    val sum = x + y
    return sum
}
