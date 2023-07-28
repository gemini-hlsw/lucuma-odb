// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.util.TimeSpan


sealed trait GroupTree extends Product with Serializable

object GroupTree {

  sealed trait Parent extends GroupTree {
    def minRequired: Option[NonNegShort]
    def ordered:     Boolean
    def children:    List[GroupTree.Child]
  }

  sealed trait Child extends GroupTree

  case class Root(
    programId: Program.Id,
    children:  List[Child]
  ) extends Parent {
    override def minRequired: Option[NonNegShort] = None
    override def ordered: Boolean                 = false
  }

  case class Branch(
    groupId:     Group.Id,
    minRequired: Option[NonNegShort],
    ordered:     Boolean,
    children:    List[Child],
    name:        Option[NonEmptyString],
    description: Option[NonEmptyString],
    minInterval: Option[TimeSpan],
    maxInterval: Option[TimeSpan]
  ) extends Parent with Child

  case class Leaf(
    observationId: Observation.Id
  ) extends Child

}