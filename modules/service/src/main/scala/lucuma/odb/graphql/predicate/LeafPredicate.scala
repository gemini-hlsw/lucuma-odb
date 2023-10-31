// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import cats.Eq
import cats.Order
import grackle.Path
import grackle.Predicate
import grackle.Predicate._

class LeafPredicates[A](path: Path) {

  def eql(a: A)(using Eq[A]): Predicate =
    Eql(path, Const(a))

  def gtEql(a: A)(using Order[A]): Predicate =
    GtEql(path, Const(a))

  def in(as: List[A])(using Eq[A]): Predicate =
    In(path, as)

  def isNull(b: Boolean): Predicate =
    IsNull(path, b)

  def notDistinctFrom(a: Option[A])(using Eq[A]): Predicate =
    a match
      case None => isNull(true)
      case Some(value) => eql(value)

}
