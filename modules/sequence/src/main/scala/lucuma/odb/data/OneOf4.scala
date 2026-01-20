// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.Eq
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.option.*

/**
 * Like an Either, but with four different cases and no error / success
 * connotation or right bias.
 */
sealed trait OneOf4[+A, +B, +C, +D]:

  def fold[R](
    fa: A => R,
    fb: B => R,
    fc: C => R,
    fd: D => R
  ): R =
    this match
      case OneOf4.First(a)  => fa(a)
      case OneOf4.Second(b) => fb(b)
      case OneOf4.Third(c)  => fc(c)
      case OneOf4.Fourth(d) => fd(d)

  def first: Option[A] =
    fold(a => a.some, _ => none[A], _ => none[A], _ => none[A])

  def second: Option[B] =
    fold(_ => none[B], b => b.some, _ => none[B], _ => none[B])

  def third: Option[C] =
    fold(_ => none[C], _ => none[C], c => c.some, _ => none[C])

  def fourth: Option[D] =
    fold(_ => none[D], _ => none[D], _ => none[D], d => d.some)

  def toEither: Either[Either[A, B], Either[C, D]] =
    fold(
      a => a.asLeft[B].asLeft[Either[C, D]],
      b => b.asRight[A].asLeft[Either[C, D]],
      c => c.asLeft[D].asRight[Either[A, B]],
      d => d.asRight[C].asRight[Either[A, B]]
    )

object OneOf4:

  final case class First[+A](value: A)  extends OneOf4[A, Nothing, Nothing, Nothing]
  final case class Second[+B](value: B) extends OneOf4[Nothing, B, Nothing, Nothing]
  final case class Third[+C](value: C)  extends OneOf4[Nothing, Nothing, C, Nothing]
  final case class Fourth[+D](value: D) extends OneOf4[Nothing, Nothing, Nothing, D]

  given [A: Eq, B: Eq, C: Eq, D: Eq]: Eq[OneOf4[A, B, C, D]] with
    def eqv(x: OneOf4[A, B, C, D], y: OneOf4[A, B, C, D]): Boolean =
      (x, y) match
        case (First(a0), First(a1))   => a0 === a1
        case (Second(b0), Second(b1)) => b0 === b1
        case (Third(c0), Third(c1))   => c0 === c1
        case (Fourth(d0), Fourth(d1)) => d0 === d1
        case (_, _)                   => false

  def first[A](a: A):  OneOf4[A, Nothing, Nothing, Nothing] = First(a)
  def second[B](b: B): OneOf4[Nothing, B, Nothing, Nothing] = Second(b)
  def third[C](c: C):  OneOf4[Nothing, Nothing, C, Nothing] = Third(c)
  def fourth[D](d: D): OneOf4[Nothing, Nothing, Nothing, D] = Fourth(d)