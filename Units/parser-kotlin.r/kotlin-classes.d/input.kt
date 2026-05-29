open class Person(val name: String, var age: Int) {
    constructor(name: String, age: String) : this(name, age.toIntOrNull() ?: 0) {
        println("$name created with converted age: ${this.age}")
    }

    companion object {
        val NOBODY = Person("", 0)
    }
}

@JvmInline
value class Permission(val id: Int)

data class User(val person: Person, val permissions: List<Permission>)

object Admin: Person("Administrator", 0) {
    const val SCOPE = "global"
}
