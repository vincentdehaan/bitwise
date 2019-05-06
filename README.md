# bitwise

This library allows for symbolic manipulation of boolean variables. For example, it is able to handle an expression like `x & (y | z)` and it understands that this is equal to `1 & (z | y) & x`.

## Usage

### Individual bits

```scala
sbt console
import nl.vindh.bitwise._
```

First, introduce some variables:
```scala
val List(x1, x2, x3, x4, x5) = (1 to 5).toList.map(i => BitVar(s"x$i"))
```

Try some basic arithmetic:
```scala
val a = x1 & x2 ^ x3
```

Now we can assign values to the variables and (partially) evaluate the expression:
```scala
a.substitute(Map(x1 -> ONE)) // yields x2 ^ x3
```

### Bitsequences

We can also handle sequences of bits (for example, bytes or 32-bit words) like they represent natural numbers:

Let's first introduce some 8-bit variables:
```scala
val x = BitSequence.variable("x", 8)
val y = BitSequence.variable("y", 8)
val z = BitSequence.variable("z", 8)
```

Now we can perform a complex operation:
```scala
val c = x + y & z
```

Let's now asign values to `x`, `y` and `z` to verify that the calculation is done correctly:

```scala
val vx = Valuation(17, 8, "x")
val vy = Valuation(42, 8, "y")
val vz = Valuation(123, 8, "z")
c.substitute(vx).substitute(vy).substitute(vz).toInt // yields 59, which is actually the result of 17 + 42 & 123
```

