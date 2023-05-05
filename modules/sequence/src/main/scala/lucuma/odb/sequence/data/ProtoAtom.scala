// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.Applicative
import cats.Eq
import cats.Eval
import cats.Traverse
import cats.data.NonEmptyList
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.traverse.*

case class ProtoAtom[A](
  description: Option[String],
  steps:       NonEmptyList[A]
)


object ProtoAtom {

  def one[A](description: Option[String], step: A): ProtoAtom[A] =
    ProtoAtom(description, NonEmptyList.one(step))

  def of[A](description: Option[String], head: A, tail: A*): ProtoAtom[A] =
    ProtoAtom(description, NonEmptyList.of(head, tail*))

  given [A](using Eq[A]): Eq[ProtoAtom[A]] =
    Eq.by { a => (
      a.description,
      a.steps
    )}

  given Traverse[ProtoAtom] with {
    override def traverse[G[_]: Applicative, A, B](fa: ProtoAtom[A])(f: A => G[B]): G[ProtoAtom[B]] =
      fa.steps.traverse(f).map(ProtoAtom(fa.description, _))

    override def foldLeft[A, B](fa: ProtoAtom[A], b: B)(f: (B, A) => B): B =
      fa.steps.foldLeft(b) { (bʹ, a) => f(bʹ, a) }

    override def foldRight[A, B](fa: ProtoAtom[A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.steps.foldRight(lb) { (a, lbʹ) => f(a, lbʹ) }
  }

}