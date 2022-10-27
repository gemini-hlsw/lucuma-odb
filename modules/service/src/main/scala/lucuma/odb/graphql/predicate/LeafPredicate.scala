// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicate

import cats.Eq
import cats.Order
import edu.gemini.grackle.Path
import edu.gemini.grackle.Predicate
import edu.gemini.grackle.Predicate._

class LeafPredicates[A](path: Path) {

  def eql(a: A)(using Eq[A]): Predicate =
    Eql(path, Const(a))

  def gtEql(a: A)(using Order[A]): Predicate =
    GtEql(path, Const(a))

  def in(as: List[A])(using Eq[A]): Predicate =
    if (as.isEmpty) False else In(path, as) // N.B. `In` can't handle an empty list

}