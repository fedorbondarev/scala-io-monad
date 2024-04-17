package io

import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should

import scala.util.Try

class IOSpec extends AnyFlatSpec with should.Matchers {
  "IO.apply" should "create expected IO" in {
    IO(()).unsafeRunSync() shouldEqual ()
    IO(12).unsafeRunSync() shouldEqual 12
  }

  "IO.suspend" should "create expected suspended IO" in {
    IO.suspend(IO(())).unsafeRunSync() shouldEqual ()
    IO.suspend(IO(19)).unsafeRunSync() shouldEqual 19
  }

  "IO.delay" should "create expected IO" in {
    IO.delay(()).unsafeRunSync() shouldEqual ()
    IO.delay(11).unsafeRunSync() shouldEqual 11
  }

  "IO.pure" should "create expected pure IO" in {
    IO.pure(()).unsafeRunSync() shouldEqual ()
    IO.pure(132).unsafeRunSync() shouldEqual 132
  }

  "IO.fromEither" should "create expected correct IO" in {
    IO.fromEither(Right(123)).unsafeRunSync() shouldEqual 123
  }

  "IO.fromEither" should "throw exception in run" in {
    a[Exception] should be thrownBy {
      IO.fromEither(Left(new Exception)).unsafeRunSync()
    }
  }

  "IO.fromTry" should "create expected correct IO" in {
    IO.fromTry(Try(1.0 / 2.0)).unsafeRunSync() shouldEqual 0.5
  }

  "IO.fromTry" should "throw exception in run" in {
    a[Exception] should be thrownBy {
      IO.fromTry(Try(1 / 0)).unsafeRunSync()
    }
  }

  "IO.fromOption" should "create expected correct IO" in {
    IO.fromOption(Some(314))(new Exception()).unsafeRunSync() shouldEqual 314
  }

  "IO.fromOption" should "throw exception in run" in {
    a[Exception] should be thrownBy {
      IO.fromOption(None)(new Exception).unsafeRunSync()
    }
  }

  "IO.none" should "create IO[None]" in {
    IO.none.unsafeRunSync() should be(None)
  }

  "IO.raiseError" should "throw exception in run" in {
    a[Exception] should be thrownBy {
      IO.raiseError(new Exception).unsafeRunSync()
    }
  }

  "IO.raiseUnless" should "create expected IO when cond is true" in {
    IO.raiseUnless(cond = true)(new Exception).unsafeRunSync() shouldEqual ()
  }

  "IO.raiseUnless" should "throw exception when cond is false" in {
    a[Exception] should be thrownBy {
      IO.raiseUnless(cond = false)(new Exception).unsafeRunSync()
    }
  }

  "IO.raiseWhen" should "create expected IO when cond is false" in {
    IO.raiseWhen(cond = false)(new Exception).unsafeRunSync() shouldEqual ()
  }

  "IO.raiseWhen" should "throw exception when cond is true" in {
    a[Exception] should be thrownBy {
      IO.raiseWhen(cond = true)(new Exception).unsafeRunSync()
    }
  }

  "IO.unlessA" should "process to expected IO when cond is false" in {
    a[Exception] should be thrownBy {
      IO.unlessA(cond = false)(IO.raiseError(new Exception())).unsafeRunSync()
    }
  }

  "IO.unlessA" should "create IO[Unit] when cond is true" in {
    IO.unlessA(cond = true)(IO.raiseError(new Exception())).unsafeRunSync() shouldEqual ()
  }

  "IO.whenA" should "process to expected IO when cond is true" in {
    a[Exception] should be thrownBy {
      IO.whenA(cond = true)(IO.raiseError(new Exception())).unsafeRunSync()
    }
  }

  "IO.whenA" should "create IO[Unit] when cond is false" in {
    IO.whenA(cond = false)(IO.raiseError(new Exception())).unsafeRunSync() shouldEqual ()
  }

  "IO.unit" should "create an empty IO[Unit]" in {
    IO.unit.unsafeRunSync() shouldEqual ()
  }

  "map" should "work as map" in {
    IO.pure(12).map(_ + 12).unsafeRunSync() shouldEqual 24
  }

  "map" should "not overflow the stack" in {
    val deepNestingLevel = 100000
    val baseIO = IO(1)

    val deeplyNestedIO = (1 to deepNestingLevel).foldLeft(baseIO)((acc, _) => acc.map(identity))

    deeplyNestedIO.unsafeRunSync() shouldEqual 1
  }

  "flatMap" should "work as flatMap" in {
    IO.pure(12).flatMap(x => IO.pure(x + 12)).unsafeRunSync() shouldEqual 24
  }

  "flatMap" should "not overflow the stack" in {
    val deepNestingLevel = 100000
    val baseIO = IO(1)

    val deeplyNestedIO = (1 to deepNestingLevel).foldLeft(baseIO)((acc, _) => acc.flatMap(IO.pure))

    deeplyNestedIO.unsafeRunSync() shouldEqual 1
  }

  "*>" should "work as *>" in {
    IO.pure(12).*>(IO.pure(24)).unsafeRunSync() shouldEqual 24
  }

  "*>" should "not overflow the stack" in {
    val deepNestingLevel = 100000
    val baseIO = IO.unit

    val deeplyNestedIO = (1 to deepNestingLevel).foldLeft(baseIO)((acc, _) => acc.*>(IO.unit))

    deeplyNestedIO.unsafeRunSync() shouldEqual ()
  }

  "as" should "work as as" in {
    IO.pure(12).as(31).unsafeRunSync() shouldEqual 31
  }

  "as" should "not overflow the stack" in {
    val deepNestingLevel = 100000
    val baseIO = IO(12)

    val deeplyNestedIO = (1 to deepNestingLevel).foldLeft(baseIO)((acc, _) => acc.as(12))

    deeplyNestedIO.unsafeRunSync() shouldEqual 12
  }

  "void" should "work as void" in {
    IO.pure(12).void.unsafeRunSync() shouldEqual ()
  }

  "void" should "not overflow the stack" in {
    val deepNestingLevel = 100000
    val baseIO = IO(12)

    val deeplyNestedIO = (1 to deepNestingLevel).foldLeft[IO[_]](baseIO)((acc, _) => acc.void)

    deeplyNestedIO.unsafeRunSync() shouldEqual ()
  }

  "attempt" should "work as attempt" in {
    IO.pure(12).attempt.unsafeRunSync() shouldEqual Right(12)
    IO.raiseError(new Exception).attempt.unsafeRunSync() match {
      case Left(_: Exception) => ()
      case _                  => fail
    }

  }

  "attempt" should "not overflow the stack" in {
    val deepNestingLevel = 100000
    val baseIO1 = IO(12)

    val deeplyNestedIO1 = (1 to deepNestingLevel).foldLeft[IO[_]](baseIO1)((acc, _) => acc.attempt)

    deeplyNestedIO1.unsafeRunSync() match {
      case Right(_) => ()
      case Left(_)  => fail
    }
  }

  "attempt" should "not overflow the stack with map" in {
    val deepNestingLevel = 100000
    val baseIO = IO(1)

    val deeplyNestedIO = (1 to deepNestingLevel).foldLeft(baseIO)((acc, _) =>
      acc.attempt.map {
        case Right(value) => value
        case Left(_)      => fail("Unexpected failure during IO operation")
      }
    )

    deeplyNestedIO.unsafeRunSync() shouldEqual 1
  }

  "option" should "work as option" in {
    IO.pure(12).option.unsafeRunSync() shouldEqual Some(12)
    IO.raiseError(new Exception).option.unsafeRunSync() shouldEqual None
  }

  "option" should "not overflow the stack" in {
    val deepNestingLevel = 100000
    val baseIO = IO(12)

    val deeplyNestedIO = (1 to deepNestingLevel).foldLeft[IO[_]](baseIO)((acc, _) => acc.option)

    deeplyNestedIO.unsafeRunSync() match {
      case Some(_) => ()
      case _       => fail
    }
  }

  "handleErrorWith" should "work as option" in {
    IO.raiseError(new Exception).handleErrorWith(_ => IO(12)).unsafeRunSync() shouldEqual 12
  }

  "handleErrorWith" should "not overflow the stack" in {
    val deepNestingLevel = 100000
    val baseIO = IO(14)

    val deeplyNestedIO =
      (1 to deepNestingLevel).foldLeft[IO[_]](baseIO)((acc, _) => acc.handleErrorWith(_ => IO(12: Any)))

    deeplyNestedIO.unsafeRunSync() shouldEqual 14
  }

  "redeem" should "work as option" in {
    IO.raiseError(new Exception).redeem(_ => 12, { _: Any => 10 }).unsafeRunSync() shouldEqual 12
    IO(12).redeem(_ => 12, { _: Any => 10 }).unsafeRunSync() shouldEqual 10
  }

  "redeem" should "not overflow the stack" in {
    val deepNestingLevel = 100000
    val baseIO = IO(14)

    val deeplyNestedIO =
      (1 to deepNestingLevel).foldLeft[IO[_]](baseIO)((acc, _) => acc.redeem(_ => 12, { _: Any => 10 }))

    deeplyNestedIO.unsafeRunSync() shouldEqual 10
  }

  "redeemWith" should "work as option" in {
    IO.raiseError(new Exception).redeemWith(_ => IO(12), { _: Any => IO(10) }).unsafeRunSync() shouldEqual 12
    IO(12).redeemWith(_ => IO(12), { _: Any => IO(10) }).unsafeRunSync() shouldEqual 10
  }

  "redeemWith" should "not overflow the stack" in {
    val deepNestingLevel = 100000
    val baseIO = IO(14)

    val deeplyNestedIO =
      (1 to deepNestingLevel).foldLeft[IO[_]](baseIO)((acc, _) => acc.redeemWith(_ => IO(12), { _: Any => IO(10) }))

    deeplyNestedIO.unsafeRunSync() shouldEqual 10
  }

  "custom test" should "work" in {
    IO(5)
      .flatMap[Int](_ => IO.suspend(IO.raiseError(new RuntimeException()).as(12)))
      .as(5)
      .attempt
      .unsafeRunSync() match {
      case Left(_: Exception) => ()
      case _ => fail
    }
  }
}
