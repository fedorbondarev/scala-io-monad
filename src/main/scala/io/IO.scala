package io

import scala.annotation.tailrec
import scala.util.Try

sealed trait IO[A] {
  self =>
  import io.IO.{Failure, FlatMap, Pure, RedeemWith, Suspended}
  def map[B](f: A => B): IO[B] = flatMap(x => Pure(f(x)))
  def flatMap[B](f: A => IO[B]): IO[B] = FlatMap(self, f)
  def *>[B](another: IO[B]): IO[B] = flatMap(_ => another)
  def as[B](newValue: => B): IO[B] = map(_ => newValue)
  def void: IO[Unit] = map(_ => ())
  def attempt: IO[Either[Throwable, A]] =
    redeemWith(x => Pure(Left(x)), { x: A => Pure(Right(x)) })
  def option: IO[Option[A]] =
    redeemWith(_ => Pure(None), { x: A => Pure(Some(x)) })
  def handleErrorWith[AA >: A](f: Throwable => IO[AA]): IO[AA] =
    redeemWith(f, x => Pure(x))
  def redeem[B](recover: Throwable => B, map: A => B): IO[B] =
    redeemWith(x => Pure(recover(x)), x => Pure(map(x)))
  def redeemWith[B](recover: Throwable => IO[B], bind: A => IO[B]): IO[B] = RedeemWith(self, recover, bind)
  def syncStep: Either[() => IO[A], A] = self match {
    case Pure(v)         => Right(v)
    case Suspended(call) => Left(call)
    case Failure(th)     => throw th
    case FlatMap(sub, cont) =>
      sub match {
        case Pure(v)              => Left(() => cont(v))
        case Suspended(call)      => Left(() => FlatMap(call(), cont))
        case Failure(th)          => Left(() => Failure(th))
        case FlatMap(sub2, cont2) => Left(() => FlatMap(sub2, { x: Any => FlatMap(cont2(x), cont) }))
        case RedeemWith(sub2, rec2, bind2) =>
          Left(() => RedeemWith(sub2, x => FlatMap(rec2(x), cont), { x: Any => FlatMap(bind2(x), cont) }))
      }
    case RedeemWith(sub, rec, bind) =>
      sub match {
        case Pure(v) => Left(() => bind(v))
        case Suspended(call) =>
          Left(() =>
            call() match {
              case Failure(th) => rec(th)
              case io          => RedeemWith(io, rec, bind)
            }
          )
        case Failure(th) => Left(() => rec(th))
        case FlatMap(sub2, cont2) =>
          Left(() =>
            RedeemWith(
              sub2,
              rec,
              { x: Any =>
                cont2(x) match {
                  case Failure(th) => rec(th)
                  case io          => RedeemWith(io, rec, bind)
                }
              }
            )
          )
        case RedeemWith(sub2, rec2, bind2) =>
          Left(() => RedeemWith(sub2, x => FlatMap(rec2(x), bind), { x: Any => FlatMap(bind2(x), bind) }))
      }
  }
  @tailrec
  final def unsafeRunSync(): A = syncStep match {
    case Left(ns) => ns().unsafeRunSync()
    case Right(v) => v
  }
}

object IO {
  private final case class Pure[A](v: A) extends IO[A]
  private final case class Suspended[A](call: () => IO[A]) extends IO[A]
  private final case class FlatMap[A, B](sub: IO[A], cont: A => IO[B]) extends IO[B]
  private final case class Failure[A](th: Throwable) extends IO[A]
  private final case class RedeemWith[A, B](sub: IO[A], recover: Throwable => IO[B], bind: A => IO[B]) extends IO[B]

  def apply[A](body: => A): IO[A] = delay(body)
  def suspend[A](thunk: => IO[A]): IO[A] = Suspended(() => thunk)
  def delay[A](body: => A): IO[A] = suspend(Pure(body))
  def pure[A](a: A): IO[A] = Pure(a)
  def fromEither[A](e: Either[Throwable, A]): IO[A] = e match {
    case Left(ex) => Failure(ex)
    case Right(v) => Pure(v)
  }
  def fromOption[A](option: Option[A])(orElse: => Throwable): IO[A] =
    fromEither(option.toRight(orElse))
  def fromTry[A](t: Try[A]): IO[A] =
    fromEither(t.toEither)
  def none[A]: IO[Option[A]] = Pure(None)
  def raiseError[A](e: Throwable): IO[A] = Failure(e)
  def raiseUnless(cond: Boolean)(e: => Throwable): IO[Unit] = if (cond) unit else raiseError(e)
  def raiseWhen(cond: Boolean)(e: => Throwable): IO[Unit] = raiseUnless(!cond)(e)
  def unlessA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = if (cond) unit else Suspended(() => action)
  def whenA(cond: Boolean)(action: => IO[Unit]): IO[Unit] = unlessA(!cond)(action)
  val unit: IO[Unit] = pure(())
}
