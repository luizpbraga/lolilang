Loli is an interpreter that sometimes does not know how to interpret.

>> loli gos brrr

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
```go
// functions (sometimes it does not work HEHE)
var foo = fn(x, y) {
    var hello = "Hello Loli"
    return if x > y { x } else { hello }
}
// yes, Sr!!! The print function!!!
print( foo(1,2) )
```

#### Builtin Methods 
```go
var my_name_is = "Eminem"
var yoyo = {1, 2, 3, 4}
print( my_name_is.len != yoyo.len )

```


#### Arrays
```go
// we have arrays
var v = { 1, true, "2" , false, 4 }
v[0] += 1
const v_size = v.len
```

#### Hashmap
```go
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
```go
// loops
var i = 0
for i < 100 {
    print(i)
    i+=1
}

const list = {1,2,3,4}
for idx, item in list {
    const str = idx + " " + item
    print(str)
}
```

#### Switch
```go
// switch 
const y = 10
var x = switch y {
    1 => { 0 },
    2 => { 1 },
    else => { -1 }
}
```

#### to implement:
    1. real numbers
    2. ranges
    4. enum
    5. struct
    6. scopes ({ ... })
    7. erros KKKKK

#### to fix:
    1. return strings from function 
    2. hashmap sometimes returns null
    3. can't use global variables in for range loops

