(ql:quickload :bi-lang)

(use-package :bi-lang)

'(defvar source "
fn makeList(n) {
  if n = 0 {
    []
  } else {
    ? : makeList(n - 1)
  }
}

fn list2d(width, height) {
  if height = 0 {
    []
  } else {
    makeList(width) : list2d(width, height - 1)
  }
}

printAll makeList(3)
printAll list2d(3, 3)
")

(defvar source "
fn consListCarsCdrs(list2d) {
  if list2d = [] {
    [[], []]
  } else {
    (a : b) : c = list2d;
    [cars, cdrs] = consListCarsCdrs(c);
    [a : cars, b : cdrs]
  }
}

fn list2dTranspose(list2d) {
  if list2d[0] = [] {
    []
  } else {
    [row, list2d_] = consListCarsCdrs(list2d);
    row : list2dTranspose(list2d_)
  }
}

fn lineNumbers(line) {
  match line {
    [] => [],
    0:line_ => lineNumbers(line_),
    [1] => [1],
    1:0:line_ => 1:lineNumbers(line_),
    1:1:line_ => {
      n:numbers = lineNumbers(1:line_);
      (n + 1):numbers
    }
  }
}

fn illustLogic(field) {
  row = map('lineNumbers, field);
  column = map('lineNumbers, list2dTranspose(field));
  [row, column]
}

printAll list2dTranspose([[1, 2], [3, 4]])
printAll lineNumbers([0, 1, 0, 1, 1, 0, 1, 1, 1])
printAll illustLogic([[1, 0], [0, 1]])
printAll illustLogic([
  [1, 0, 1],
  [0, 1, 1],
  [0, 1, 1]])
print1 'a

printAll consListCarsCdrs([[1, 2], [3, 4]])
printAll consListCarsCdrs!([[1], [[]]])
printAll consListCarsCdrs!([[1, 2], [[3], [4]]])
printAll list2dTranspose!([[1, 2], [3, 4]])
")

(print (transpile source))

(exec source)
