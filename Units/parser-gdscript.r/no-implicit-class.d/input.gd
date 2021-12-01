# Derived from https://docs.godotengine.org/en/latest/tutorials/scripting/gdscript/gdscript_basics.html

# A file is a class but it has no name. The name is given only when
# class_name is used.

# Inheritance

extends BaseClass

# (optional) class definition with a custom icon

class_name MyClass, "res://path/to/optional/icon.svg"


# Member variables

@export_range(start=0, end=100, step=1) var a = 5
@export
var s = "Hello"
@onready var arr = [1, 2, 3]
var dict = {"key": "value", 2: 3}
var typed_var: int
@onready
@export_multiline
var\
inferred_type\
:=\
"String"

# Constants

const ANSWER = 42
const THE_NAME:String = "Charly"

# Enums

enum {UNIT_NEUTRAL, UNIT_ENEMY, UNIT_ALL}
enum Named {THING_1, THING_2, ANOTHER_THING=1}

# Built-in vector types

var v2 = Vector2(1, 2)
var v3 = Vector3(1, 2, 3)


# Function

@master
func some_function(param1: Vector3, param2: int) -> int:
    var local_var = 5

    if param1 < local_var:
        print(param1)
    elif param2 > 5:
        print(param2)
    elif param2 == 2:
        print(20)
    elif param2 <= 2:
        print((-20 %



3) / 5)
    else:
        print("Fail!")

    for i in range(20):
        print(i)

    while param2 != 0:
        param2 -= 1

    var local_var2 = param1 + 3
    return local_var2


# Functions override functions with the same name on the base/parent class.
# If you still want to call them, use '.' (like 'super' in other languages).
@puppet
func something(p1, p2):
    .something(p1, p2)


# Inner class

class Something:
    var a = 10
    const _private_var:String = "hi\n\\escape"
	func foooooooo() -> String:
		print("""
		test\\
		
		test""")
        return ""

# Constructor

func _init():
    print("Constructed!")
    var lv = Something.new()
    print(lv.a)
