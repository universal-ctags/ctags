type MyString = "mystring"
type MyUnion = MyString | "other" | 99
export type MyExportedUnion = MyUnion | "default" //

type StringOrNumber = string | number
type Text = string | { text: string }
type NameLookup = Dictionary<string, Person>
type ObjectStatics = typeof Object
type Callback<T> = (data: T) => void
type Pair<T> = [T, T]
type Coordinates = Pair<number>
type Tree<T> = T | { left: Tree<T>, right: Tree<T> }
