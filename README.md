Loli is an interpreter that sometimes does not know how to interpret.

> > loli gos brrr

##### Zig Version

    zig-linux-x86_64-0.12.0-dev.1372+8457439a8

### code example

#### Variable Declaration

```go
// multi-line declaration
const {
    ONE = 1
    TWO = 2
}

var t = 3
```

#### Function

```js
// functions (sometimes it does not work HEHE)
var foo = fn(x, y) {
    var hello = "Hello Loli"
    return if x > y { x } else { hello }
}
// yes, Sr!!! The print function!!!
print( foo(1,2) )
```

#### Builtin Methods

```js
var my_name_is = "Eminem"
var yoyo = {1, 2, 3, 4}
println( my_name_is.len != yoyo.len )
```

#### Arrays

```js
// we have arrays
var v = [1, true, "2", false, 4];
v[0] += 1;
const v_size = v.len;
```

#### Hashmap

```js
// hashmaps
const map = {
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

const list = 1..5
for idx, item in list {
    const str = idx + " " + item
    print(str)
}
```

#### Switch

```js
// switch
const y = 10
var x = switch y {
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

    1. return strings from function
    2. hashmap sometimes returns null
    3. can't use global variables inside local blocks loops
