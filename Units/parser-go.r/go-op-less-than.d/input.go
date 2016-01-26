package test

func f1(a int, b int) int {
  if a </* }
  func bug() { */ 42 {
    return 1
  }
  return a
}

func f2(a int, b int) int {
  return a + b
}

func f3() {}
