// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.smartgcal.parsers

import cats.Monad
import cats.syntax.functor.*

/**
 * Availability is a small ADT, isomorphic with Option, that is used for parsing
 * smart gcal files in which one or more entries refer to obsolete values.
 * Here an `Obsolete` instance is like `None` and `Current` is like `Some`. We
 * use this to skip entries that refer to obsolete values while still generating
 * an error for unknown entries.
 *
 * For example the `u_G0309` GMOS North filter is obsolete.  Entries that refer
 * to it should not generate an error and yet do not parse to any existing
 * filter value.
 */
sealed trait Availability[+A]:
  import Availability.Current
  import Availability.Obsolete

  def fold[B](ifObsolete: => B, ifCurrent: A => B): B =
    this match
      case Obsolete   => ifObsolete
      case Current(a) => ifCurrent(a)

  def isObsolete: Boolean =
    fold(true, _ => false)

  def isCurrent: Boolean =
    !isObsolete

  def map[B](f: A => B): Availability[B] =
    this match
      case Obsolete   => Obsolete
      case Current(a) => Current(f(a))

  def flatMap[B](f: A => Availability[B]): Availability[B] =
    this match
      case Obsolete   => Obsolete
      case Current(a) => f(a)

  def toOption: Option[A] =
    this match
      case Obsolete   => None
      case Current(a) => Some(a)

  @inline final def getOrElse[B >: A](default: => B): B =
    this match
      case Obsolete   => default
      case Current(a) => a


object Availability:

  case object Obsolete extends Availability[Nothing]

  case class Current[A](a: A) extends Availability[A]

  extension [A](a: A)
    def current:  Availability[A] = Current(a)

  given Monad[Availability] with
    def pure[A](a: A): Availability[A] = Current(a)

    def flatMap[A, B](fa: Availability[A])(f: A => Availability[B]): Availability[B] =
      fa.flatMap(f)

    def tailRecM[A, B](a: A)(f: A => Availability[Either[A, B]]): Availability[B] =
      f(a) match
        case Obsolete           => Obsolete
        case Current(Left(a1))  => tailRecM(a1)(f)
        case Current(Right(b))  => Current(b)