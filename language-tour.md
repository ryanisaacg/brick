# Language Tour

> [!NOTE]
> The language is very early and things can evolve drastically! The code in the `tests/` directory is checked for every commit, this code is not.


At its most basic level, Brick is an attempt to take some core safety guarantees of Rust and bring them to a simpler, easier language.

Variable declarations look similar, but all variables are mutable:

```
let x = 1234;
x = 5678;
```

> [!NOTE]
> Currently code may be declared at the top-level, outside a function. This is subject to change in the future, I'm not sure it's a great idea.

Structs are declared the way you may expect coming:

```
struct Point2 {
    x: i32,
    y: i32,
}
```

Brick doesn't have traits or `impl` blocks; functions are declared right on structs like in Zig or Go:

```
struct Rectangle {
    tl: Point2,
    br: Point2,

    fn width(self: Rectangle): i32 {
        self.br.x - self.tl.x
    }

    fn height(self: Rectangle): i32 {
        self.br.y - self.tl.y
    }

    fn area(self: Rectangle): i32 {
        self.width() * self.height()
    }
}
```

> [!NOTE]
> There are no visibility modifiers in Brick. All fields and methods are public. Eventually I will probably add a `private` modifier, but for now I feel pretty good about default-public-fields

At time of writing Brick's Abstract Data Type / tagged union support is limited to a union with zero or one types in each case. Unions can be destructured with the `case` statement; no other level of pattern matching is supported.

```
union Option {
    Some(i32),
    None
}

let num = Option.None;
case num {
    Some(_) => "Some!",
    None => "None!",
}
```

By default types are "data": they may be freely copied whenever you like. Some types are `Resource`s; they cannot be copied and are automatically cleaned up by the compiler when they go out of scope. If you're familiar with Rust, these types obey "move semantics."

```
// This code DOES NOT COMPILE
struct MyData: Resource {}

let x = MyData{};
let y = x;
x // the resource was given to y; x can no longer be used
```

Any type may be declared a resource. Data types may not contain resource types.


