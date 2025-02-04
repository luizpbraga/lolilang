### Loli, the  bytecode compiler

> > loli go brrr

##### Zig Version

    zig master

##### Build 
    zig build -Doptimize=ReleaseFast

### code example
#### Variable Declaration
```go
var t = 3
sex := true
```

#### Function and Closures
```rust
fn fibonacci(n, fib) {
    if (n <= 1) { 
      return fib[n]
    }
      
    if (fib[n-1] == null) {
      fib[n-1] = fibonacci(n-1, fib)
    }

    if (fib[n-2] == null) {
      fib[n-2] = fibonacci(n-2, fib)
    }

    fib[n] = fib[n-1] + fib[n-2]

    return fib[n]
}

fib := { 0: 1, 1: 1 }
fibonacci(45, fib)

foo := fn(x, y) {
    hello := "Hello Loli"
    return if (x > y) { x } else { hello }
}

```

#### Builtin Methods

```js
var my_name_is = "Eminem";
var yoyo = [1, 2, 3, 4];
@print(my_name_is.len != yoyo.len);
```

#### Arrays
```js
var v = [1, true, "2", false, 4];
v[0] += 1;
len := v.len;
```

#### Hashmap
```js
const map = {
    1: "a",
    2: "b",
    true: false,
    "list": {1,2,3}
    .tags: [.x, .x, .x]
}

@print( map["list"], map.list, map[true], map[1], map.tags )
```

#### Loops
```js
var i = 0
for (i < 100) {
    @print(i)
    i = i + 1
}

range := [1, 2, 3]
for (item in range) {
    str := item + " " + item
    @print(str)
}

for (i in 10) {
    @print("Integer Loop")
}

for (i in 10..100) {
    @print("Range Loop")
}
```

#### Match
```rust
x := match (10) {
    1 => { 0 }

    2 => { 1 }

    else => { -1 }
}
```

#### Structs and Enums
```zig
var Color = enum { 
    red,
    blue,
    black,
}

#//colons and semicolons are optional
var X = struct { 
    the_name_is,
    some_null_value,
    you_mama = {},
    default_color = Color.red,

    fn init() {
        return X{the_name_is:"Loli!"}
    }

    fn append(this, str_val) {
        @append(this.the_name_is, str_val)
    }
};

```

#### to fix:
    + enum and struct implementation sucks 
    + garbage collection
    + memory leaks
