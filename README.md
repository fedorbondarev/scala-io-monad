# Scala-IO-Monad

The `Scala-IO-Monad` implements an IO monad, inspired by the design of Cats Effect IO Monad. This implementation focuses on overcoming stack-overflow issues typically encountered with recursive operations.

## Usage

### Prerequisites

To use `Scala-IO-Monad` in your project, you need to have the following installed:

- Scala 2.13 or higher
- SBT (Scala Build Tool)
- Scalatest 3.2 or higher

### Example

Here's a simple usage example that demonstrates how to perform an IO operation:

```scala
package test

import io.IO

object Test {
  def main(args: Array[String]): Unit = {
    val program: IO[Unit] = for {
      _ <- IO { println("Hello, IO Monad!") }
      _ <- IO { println("Avoiding stack overflow...") }
    } yield ()

    program.unsafeRunSync()
  }
}

```

## Tests

Scala-IO-Monad has around 97% tests coverage

To run the unit tests using Scalatest, execute the following command:
```bash
sbt test
```