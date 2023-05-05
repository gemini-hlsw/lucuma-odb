// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.sequence.data

import cats.Applicative
import cats.Eq
import cats.Eval
import cats.Traverse
import cats.syntax.apply.*
import cats.syntax.foldable.*
import cats.syntax.traverse.*

case class ProtoExecutionConfig[S, D](
  static:      S,
  acquisition: ProtoSequence[D],
  science:     ProtoSequence[D]
) {

  def mapSequences[E](f: ProtoSequence[D] => ProtoSequence[E]): ProtoExecutionConfig[S, E] =
    ProtoExecutionConfig[S, E](static, f(acquisition), f(science))

}

object ProtoExecutionConfig {

  given [S, D](using Eq[S], Eq[D]): Eq[ProtoExecutionConfig[S, D]] =
    Eq.by { x => (
      x.static,
      x.acquisition,
      x.science
    )}

  given [X]: Traverse[ProtoExecutionConfig[X, *]] with {
    override def traverse[G[_]: Applicative, A, B](fa: ProtoExecutionConfig[X, A])(f: A => G[B]): G[ProtoExecutionConfig[X, B]] =
      (fa.acquisition.traverse(f), fa.science.traverse(f)).mapN((ab, sb) =>
        ProtoExecutionConfig(fa.static, ab, sb)
      )

    override def foldLeft[A, B](fa: ProtoExecutionConfig[X, A], b: B)(f: (B, A) => B): B =
      fa.science.foldLeft(fa.acquisition.foldLeft(b)(f))(f)

    override def foldRight[A, B](fa: ProtoExecutionConfig[X, A], lb: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] =
      fa.acquisition.foldRight(fa.science.foldRight(lb)(f))(f)

  }
}