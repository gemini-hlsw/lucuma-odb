// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats._

import scala.annotation.tailrec

/** Option-like data type that distinguishes null values from absent ones. */
sealed trait Nullable[+A] extends Product with Serializable {
  import Nullable._

  def fold[B](ifNull: => B, ifAbsent: => B, ifPresent: A => B): B =
    this match {
      case Null       => ifNull
      case Absent     => ifAbsent
      case NonNull(a) => ifPresent(a)
    }

  def map[B](f: A => B): Nullable[B] = fold(Null, Absent, a => NonNull(f(a)))
  def flatMap[B](f: A => Nullable[B]): Nullable[B] = fold(Null, Absent, f)
  def orElse[B >: A](nb: Nullable[B]): Nullable[B] = fold(nb, nb, NonNull(_))
  def toOption: Option[A] = fold(None, None, Some(_))

}

object Nullable {

  case object Null                 extends Nullable[Nothing]
  case object Absent               extends Nullable[Nothing]
  case class  NonNull[A](value: A) extends Nullable[A]

  implicit val NullableInstances: Monad[Nullable] with SemigroupK[Nullable] =
    new Monad[Nullable] with SemigroupK[Nullable] {
      override def map[A, B](fa: Nullable[A])(fab: A => B) = fa.map(fab)
      def flatMap[A, B](fa: Nullable[A])(f: A => Nullable[B]) = fa.flatMap(f)
      def pure[A](x: A) = NonNull(x)
      def combineK[A](x: Nullable[A], y: Nullable[A]): Nullable[A] = x orElse y
      @tailrec def tailRecM[A, B](a: A)(f: A => Nullable[Either[A,B]]): Nullable[B] =
        f(a) match {
          case Absent                => Absent
          case Null                  => Null
          case NonNull(Right(value)) => NonNull(value)
          case NonNull(Left(value))  => tailRecM(value)(f)
        }
    }

}