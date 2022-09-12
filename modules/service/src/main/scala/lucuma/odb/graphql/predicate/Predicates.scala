// Copyright (c) 2016-2022 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.predicates

/**
 * Module of predicates for top-level types. Start here (with whatever your result type is) when
 * constructing filters, etc.
 */
object Predicates {

  val target = new TargetPredicates(Nil)
  val program = new ProgramPredicates(Nil)
  val observation = new ObservationPredicates(Nil)

}