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

case class ProtoSequence[A](
  atoms: NonEmptyList[ProtoAtom[A]]
)

object ProtoSequence {

  def one[A](atom: ProtoAtom[A]): ProtoSequence[A] =
    ProtoSequence(NonEmptyList.one(atom))

  def of[A](head: ProtoAtom[A], tail: ProtoAtom[A]*): ProtoSequence[A] =
    ProtoSequence(NonEmptyList.of(head, tail*))

  given [A](using Eq[A]): Eq[ProtoSequence[A]] =
    Eq.by(_.atoms)

  given Traverse[ProtoSequence] with {
    override def traverse[G[_]: Applicative, A, B](fa:  ProtoSequence[A])(f: A => G[B]): G[ProtoSequence[B]] =
      fa.atoms.traverse(_.traverse(f)).map(ProtoSequence(_))

    override def foldLeft[A, B](fa: ProtoSequence[A], b: B)(f: (B, A) => B): B =
      fa.atoms.foldLeft(b) { (bʹ, atom) => atom.foldLeft(bʹ)(f) }

    override def foldRight[A, B](fa: ProtoSequence[A], lb:  Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.atoms.foldRight(lb) { (atom, lbʹ) => atom.foldRight(lbʹ)(f) }
  }

}