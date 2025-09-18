// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.*

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

  /** Fold if non-Absent. */
  def foldPresent[B](ifPresent: Option[A] => B): Option[B] =
    fold(Some(ifPresent(None)), None, a => Some(ifPresent(Some(a))))

  def map[B](f:          A => B): Nullable[B]           = fold(Null, Absent, a => NonNull(f(a)))
  def flatMap[B](f:      A => Nullable[B]): Nullable[B] = fold(Null, Absent, f)
  def orElse[B >: A](nb: Nullable[B]): Nullable[B]      = fold(nb, nb, NonNull(_))
  def toOption: Option[A]                               = fold(None, None, Some(_))

  def toOptionOption: Option[Option[A]] =
    fold(Some(None), None, a => Some(Some(a)))

  def isNull: Boolean =
    fold(ifNull = true, ifAbsent = false, _ => false)

  def isAbsent: Boolean =
    fold(ifNull = false, ifAbsent = true, _ => false)

  def isPresent: Boolean =
    fold(ifNull = false, ifAbsent = false, _ => true)

}

object Nullable {

  case object Null                extends Nullable[Nothing]
  case object Absent              extends Nullable[Nothing]
  case class NonNull[A](value: A) extends Nullable[A]

  implicit val NullableInstances: Monad[Nullable] & SemigroupK[Nullable] =
    new Monad[Nullable] with SemigroupK[Nullable] {
      override def map[A, B](fa: Nullable[A])(fab: A => B): Nullable[B]              = fa.map(fab)
      def flatMap[A, B](fa:      Nullable[A])(f:   A => Nullable[B]): Nullable[B]    = fa.flatMap(f)
      def pure[A](x:             A): Nullable[A]                                     = NonNull(x)
      def combineK[A](x:         Nullable[A], y:   Nullable[A]): Nullable[A]         = x.orElse(y)
      @tailrec def tailRecM[A, B](a: A)(f: A => Nullable[Either[A, B]]): Nullable[B] =
        f(a) match {
          case Absent                => Absent
          case Null                  => Null
          case NonNull(Right(value)) => NonNull(value)
          case NonNull(Left(value))  => tailRecM(value)(f)
        }
    }

  def orNull[A](opt: Option[A]): Nullable[A] =
    opt.fold(Null: Nullable[A])(a => NonNull(a))

  def orAbsent[A](opt: Option[A]): Nullable[A] =
    opt.fold(Absent: Nullable[A])(a => NonNull(a))

}
