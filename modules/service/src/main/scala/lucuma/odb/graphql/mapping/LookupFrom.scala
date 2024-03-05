// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mapping

import grackle.Path
import grackle.Type

trait LookupFrom[F[_]] extends BaseMapping[F] {

  private lazy val VisitTypes: List[Type] =
    List(
      VisitType,
      GmosNorthVisitType,
      GmosSouthVisitType
    )

  private def defineLookup(tpes: List[Type], path: Seq[String], mapping: ObjectMapping): List[(Path, ObjectMapping)] =
    tpes.toList.map { tpe =>
      (path.foldLeft(Path.from(tpe)) { case (t, p) => t / p }, mapping)
    }

  def lookupFromVisit(mapping: ObjectMapping, path: String*): List[(Path, ObjectMapping)] =
    defineLookup(VisitTypes, path, mapping)

}
