package com.example.helloworld

interface AbstractWorker {
    abstract fun process(): String
}

class Worker<T>(val name: T): AbstractWorker {
    override fun process(): String = name.toString()
}

object Global {
    const val greeting = "Hello"
}

typealias StringWorker = Worker<String>

fun main() {
    var result: String = Global.greeting
    result = "$result " + StringWorker("World").process()
    result += "!"
    println(result)
}
