defmodule RecordsModule do
  require Record
  Record.defrecord(:user1, name: "megan", age: "25")
  Record.defrecordp :user2, name: "ivan", age: "23"
  # This is not a typo but an intentional bad test, used to test the parser
  Record.defrecordp:bad, name: "ivan", age: "23"
end
