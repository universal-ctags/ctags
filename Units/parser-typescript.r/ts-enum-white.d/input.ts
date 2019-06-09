enum Color { Red Green Blue }
enum Test {
  A
  B
  C = Math.floor(Math.random() * 1000)
  D = 10
  E
}

export enum Style {
  None = 0
  Bold = 1
  Italic = 2
  Underline = 4
  Emphasis = Bold | Italic
  Hyperlink = Bold | Underline
}

const enum Comparison {
  LessThan = -1
  EqualTo = 0
  GreaterThan = 1
}
