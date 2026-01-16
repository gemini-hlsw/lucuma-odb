// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*

/**
 * Like an Either, but with three different cases and no error / success
 * connotation or right bias.
 */
sealed trait OneOf3[+A, +B, +C]:

  def fold[R](
    fa: A => R,
    fb: B => R,
    fc: C => R
  ): R =
    this match
      case OneOf3.First(a)  => fa(a)
      case OneOf3.Second(b) => fb(b)
      case OneOf3.Third(c)  => fc(c)

  def first: Option[A] =
    fold(a => a.some, _ => none[A], _ => none[A])

  def second: Option[B] =
    fold(_ => none[B], b => b.some, _ => none[B])

  def third: Option[C] =
    fold(_ => none[C], _ => none[C], c => c.some)

  def toEither: Either[Either[A, B], C] =
    fold(a => a.asLeft[B].asLeft[C], b => b.asRight[A].asLeft[C], c => c.asRight[Either[A, B]])

object OneOf3:

  final case class First[+A](value: A)  extends OneOf3[A, Nothing, Nothing]
  final case class Second[+B](value: B) extends OneOf3[Nothing, B, Nothing]
  final case class Third[+C](value: C)  extends OneOf3[Nothing, Nothing, C]

  given [A: Eq, B: Eq, C: Eq]: Eq[OneOf3[A, B, C]] with
    def eqv(x: OneOf3[A, B, C], y: OneOf3[A, B, C]): Boolean =
      (x, y) match
        case (First(a0), First(a1))   => a0 === a1
        case (Second(b0), Second(b1)) => b0 === b1
        case (Third(c0), Third(c1))   => c0 === c1
        case (_, _)                   => false

  def first[A](a: A):  OneOf3[A, Nothing, Nothing] = First(a)
  def second[B](b: B): OneOf3[Nothing, B, Nothing] = Second(b)
  def third[C](c: C):  OneOf3[Nothing, Nothing, C] = Third(c)