Loli is an interpreter that sometimes does not know how to interpret.

> > loli gos brrr

##### Zig Version

    zig-linux-x86_64-0.12.0-dev.1372+8457439a8

### code example

#### Variable Declaration

```go
// multi-line declaration
con {
    ONE = 1
    TWO = 2
}

var t = 3
```

#### Function

```go
// functions (sometimes it does not work HEHE)
con PI = 3.14

func readFileExample(file_path) {
    con txt = readFile(file_path)
    defer { closeFile(txt) }

    var sum = 0
    for ch, idx in txt {
        sum += 1
        print(ch, idx)
    }

    return txt.len == sum
}

con foo = func(x, y) {
    var hello = "Hello Loli"
    return if x > y { x } else { hello }
}

```

#### Builtin Methods

```js
var my_name_is = "Eminem";
var yoyo = [1, 2, 3, 4];
print(my_name_is.len != yoyo.len);
```

#### Arrays

```js
// we have arrays
var v = [1, true, "2", false, 4];
v[0] += 1;
con v_size = v.len;
```

#### Hashmap

```js
// hashmaps
con map = {
    1: "a",
    2: "b",
    true: false,
    "list": {1,2,3}
}

print( map["list"], map[true], map[1] )
```

#### Loops

```js
// loops
var i = 0
for i < 100 {
    println(i)
    i+=1
}

con range = 1..5
for idx, item in range {
    con str = idx + " " + item
    print(str)
}
```

#### Switch

```js
// switch
con y = 10
con x = switch y {
    1 => { 0 }

    2 => { 1 }

    else => { -1 }
}
```

#### to implement:

    1. enum
    2. struct
    3. scopes ({ ... })
    4. erros KKKKK

#### to fix:

    + hashmap sometimes returns null
    + memory leaks
