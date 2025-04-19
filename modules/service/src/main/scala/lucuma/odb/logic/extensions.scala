// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.Monad
import cats.data.NonEmptyList
import cats.syntax.applicative.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*

import scala.collection.mutable.ListBuffer

extension [A](as: List[A]) {

  /**
   * Map over a list, but keeping track of a state when calling the mapping
   * function whose results are in monadic effect type F.
   *
   * @param initialState initial state value
   * @param f mapping (with state) function
   * @tparam F effect type
   * @tparam B resulting element type
   * @tparam S state type
   *
   * @return final state and mapped list
   */
  def mapAccumulateM[F[_]: Monad, B, S](initialState: S)(f: (S, A) => F[(S, B)]): F[(S, List[B])] =
    as.foldLeft((initialState, ListBuffer.empty[B]).pure[F]) { case (fac, a) =>
      fac.flatMap { case (s, bs) =>
        f(s, a).map { case (sʹ, b) => (sʹ, bs.addOne(b)) }
      }
    }.map(_.map(_.toList))

}

extension [A](as: NonEmptyList[A]) {

  def mapAccumulateM[F[_]: Monad, B, S](initialState: S)(f: (S, A) => F[(S, B)]): F[(S, NonEmptyList[B])] =
    f(initialState, as.head).flatMap { (sʹ, b) =>
      as.tail.mapAccumulateM[F, B, S](sʹ)(f).map(_.map(NonEmptyList(b, _)))
    }

}
