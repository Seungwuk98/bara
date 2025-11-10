# Capybara Language
The educational interpreter language with LLVM framework

## Features

### Integer, Float, String, Boolean
```javascript
var x = 10;
var y = 20.0;
var z = "String";
var w = true || false; // true
```

### Tuple, List

```javascript
var tup = (1, 2, 3);
var lst = [1, 2, 3];

lst[0] = 10;
print(lst);
// [10, 2, 3]

// tup[0] = 10;  ==> error!
print(tup);
// (1, 2, 3)
```

### Struct
```javascript
struct Point(X, Y, Z);

var p1 = Point(1, 2, 3);

(p1.X, p1.Y, p1.Z) = (4, 5, 6);

print(p1);
// <struct 'Point': X=4, Y=5, Z=6>
```

### Function
```javascript
fn fibonacci(x) {
  if x <= 1 {
    return x;
  }
  return fibonacci(x - 1) + fibonacci(x - 2);
}

fibonacci(6);
// 8
```

### Lambda
```javascript
var getId = {
  var x = 0;
  \ => {
    x += 1
  } 
};

print(getId());
// 1
print(getId());
// 2

var add = \x, y => x + y;

print(add(1, 2));
// 3
```


### Compound Expression
```javascript
var x = {
  var y = 10;
  var z = 20;
};
print(x);
// nil

var y = {
  var u = 10;
  u
};
print(y);
// 10
```

### Conditional Expression
```javascript
var x = true;

print(x ? 1 : 2);
// 1

print(x ? "123" ? 1 : 2 : 3)
// 1
```


### Match Expression

```javascript
var x = match (1, 2) {
  (a, 3) => format("case 1 - a={}", a);
  (2, b) => format("case 2 - b={}", b);
  (a, b) => format("case 3 - a={}, b={}", a, b);
};

print(x);
// case 3 - a=1, b=2
```

### Control flow statements

```javascript

var x = 10;

if x < 10 {
  print("lt 10");
} else if x < 20 {
  print("ge 10 & lt 20");
} else {
  print("ge 20");
}
// ge 10 & lt 20


for (var i = 0; i < 5; i += 1) {
  print(i);
}
// 0
// 1
// 2
// 3
// 4

var i = 0;
while i < 5 {
  print(i);
  if (i == 2) {
    break;
  } 
  if (i >= 10) {
    continue;
  }
  i += 1;
}
// 0
// 1

var j = 3;
do {
  print(j);
  j += 1;
} while j < 3;
// 3

```

## Installation

This project uses llvm. Install LLVM in your system or build from source.
Recommended way is using apt in ubuntu 24.04 or later.

```bash
$ sudo apt install llvm-18 llvm-18-dev clang-18 cmake ninja-build 
```

### Build

```bash
$ cmake -G Ninja -B $(build-dir) -DCMAKE_BUILD_TYPE=$(build-type) -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++
$ cmake --build build 
```

### Install

```bash
$ cmake --install build --prefix $(install-dir)
```

```javascript
/// test.bara
var x = 10;
print(x);
```

```bash
$ bara test.bara
10
```

## Test

### Unit Test
```bash
$ $(build-dir)/test/unittest/ASTTests/ASTTests
$ $(build-dir)/test/unittest/ParserTests/ParserTests
$ $(build-dir)/test/unittest/InterpreterTests/InterpreterTests
```

### LLVM-lit

You need to install `lit` in your system
```bash
$ pip install lit
```

```bash
$ cmake --build $(build-dir) --target check-bara
```

### Benchmark

To execute benchmark, you need to build project by release. 
```bash
$ cmake -G Ninja -B $(build-dir) -DCMAKE_BUILD_TYPE=Release -DCMAKE_C_COMPILER=clang -DCMAKE_CXX_COMPILER=clang++
$ cmake --build build 
```


```bash
$ cmake --build build --target test-mst-all
$ cmake --build build --target test-sort-big
...
```

You can check the benchmark tests in `bara/test/benchmark/algorithm`.
Target name is `test-{algorithm}-{test_case}`.