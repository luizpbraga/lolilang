FUTURE SYNTAX
----

// fn, return, match, continue, break, var, const, or, and, as, stop, try, catch
// struct, enum, struct_name, enum_name

### declaration
```zig
var x = 10
const y = 100
```

### FUCNTIONs: 'self'
```zig
const foo = fn(x = 10) { return 10 + x }
fn foo(y) { return 2 * y }
```

```zig
const {
    F = 10
    U = false
    C = "ola"
    K = {1, 2, 3}
}
```
## STRUCT and ENUMS
```zig
//struct | enum  P { // imutable
//struct P {
const B = struct { a }
const P = struct {
    ...B
    
    const PI = 3.14

    name
    age
    lenght

    fn foo(self) {}

    fn nameIs(self){
        return self.name
    }
}

try expect( P.PI == 3.14 )

const p1 = P{ a = 10, name = "Luiz", age =  27, lenght = 169 }
const p2 = P{ 10, "Luiz", 27, 169 }
```
### Erros 
```zig
const Err = Error { fileNotfound, canNotFindMyDead, missingPureLove  }

fn foo(x) ? {
    return if x < 0 {error.fileNotfound} else {x}
}

fn bar(x) Err? {
    return if x < 0 {error.fileNotfound} else {x}
}

var f = try bar(-1) // thows the error
var g = bar(-1) catch 0 // ignore the error 
var h = foo() catch err: { return err }
```

## IF: 'or' and 'and' 
```rust
if v == 10 and x != 12 { ... } else if !true { ... } else { stop } 
```

## FOR: 'break' and 'continue'
```c
for val in list {}
for 0..10 {}
for 0...10 {}
for 0.. {}
for {}
for i in 0..10 {}
for i in 0..10, j in 10..20 {}
for k = 10, k < 20, k++ {}
for k = 10, k < 20 {}
for k < 20 {}
for k = 10 {}
```

## SWITCH
```zig
var t = switch x {
    1..10 => 0,
    'a'..'z' => 1,
    20, 30 => 2,
    else => null
}

const Color = enum{ red, blue }

var t = switch x {
    .red => r: {
        // returns null 
    },

    .blue => b: {
        break b
    }
}

try expect( t == Color.vlue )
try expect( type(t) == Color )
```

### lists and maps 
---
methods: len
```python
var y = {1, true, "ola", fn(){ return 0 }() }
y = {i for i in y}

const x = { {1, 2, 3}, {4, 5, 6}, {7, 8, 9} }

for i in x, for k in i {}

var m = {
    key1 : val1,
    key2 : val2,
}

try expect(m[key1] == val1)

```
for k, v in m {}

var {
    a = {1,2}
    b = {2,3}
    c = a + b
    d = a ** b
}

try expect( c == {3, 5})
try expect( d == {1, 2, 2, 3})


### Modules


### main fn?

import module as m

import {
    mod1 as m1
    mod2 as m2
}

from mod1 import { m1, m2, m3 }

fn main() {

}
