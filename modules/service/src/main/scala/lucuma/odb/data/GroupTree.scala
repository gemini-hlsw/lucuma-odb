// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.data

import cats.syntax.eq.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.core.enums.CalibrationRole
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.util.TimeSpan

/**
 * A simple in-memory group representation.
 */
sealed trait GroupTree extends Product with Serializable {

  def findGroup(groupId: Group.Id): Option[GroupTree] = {
    @scala.annotation.tailrec
    def go(remaining: List[GroupTree]): Option[GroupTree] =
      remaining match {
        case Nil => None
        case GroupTree.Leaf(_) :: tail => go(tail)
        case GroupTree.Root(_, c) :: tail => go(c ::: tail)
        case (b @ GroupTree.Branch(groupId = g, children = c)) :: tail =>
          if (g === groupId) b.some else go(c ::: tail)
      }
    go(List(this))
  }

}

object GroupTree {

  /**
   * GroupTree nodes that can contain other group tree nodes.
   */
  sealed trait Parent extends GroupTree {
    def minRequired: Option[NonNegShort]
    def ordered:     Boolean
    def children:    List[GroupTree.Child]
  }

  /**
   * GroupTree nodes that may be contained in other group tree nodes.
   */
  sealed trait Child extends GroupTree

  /**
   * The GroupTree root corresponds to a program.  Programs require all
   * immediate children to be executed (i.e., they form an implicit AND group)
   * but order does not matter.
   */
  case class Root(
    programId: Program.Id,
    children:  List[Child]
  ) extends Parent {
    override def minRequired: Option[NonNegShort] = None
    override def ordered: Boolean                 = false
  }

  /**
   * GroupTree branches correspond to Group, which can be children of a program
   * or other groups.
   */
  case class Branch(
    groupId:          Group.Id,
    minRequired:      Option[NonNegShort],
    ordered:          Boolean,
    children:         List[Child],
    name:             Option[NonEmptyString],
    description:      Option[NonEmptyString],
    minInterval:      Option[TimeSpan],
    maxInterval:      Option[TimeSpan],
    system:           Boolean,
    calibrationRoles: List[CalibrationRole]
  ) extends Parent with Child

  /**
   * GroupTree leaves are observations.
   */
  case class Leaf(
    observationId: Observation.Id
  ) extends Child

  extension (tree: GroupTree) {

    /**
     * Finds all groups that match a predicate, along with their direct observation children.
     */
    def findGroupsWithObservations(
      predicate: Branch => Boolean
    ): List[(Group.Id, List[Observation.Id])] = {
      @scala.annotation.tailrec
      def go(
        remaining: List[GroupTree],
        acc: List[(Group.Id, List[Observation.Id])]
      ): List[(Group.Id, List[Observation.Id])] =
        remaining match
          case Nil => acc.reverse
          case Leaf(_) :: tail =>
            go(tail, acc)
          case Root(_, children) :: tail =>
            go(children ::: tail, acc)
          case (b @ Branch(groupId = gid, children = children)) :: tail =>
            val observations = children.collect:
              case Leaf(obsId) => obsId
            val newAcc =
              if predicate(b) then (gid, observations) :: acc
              else acc
            go(children ::: tail, newAcc)
      go(List(tree), Nil)
    }

    /**
     * Finds the first group containing an observation that matches a predicate.
     */
    def findGroupContaining(
      oid: Observation.Id,
      predicate: Branch => Boolean
    ): Option[Group.Id] = {
      @scala.annotation.tailrec
      def go(remaining: List[GroupTree]): Option[Group.Id] =
        remaining match
          case Nil => None
          case Leaf(_) :: tail =>
            go(tail)
          case Root(_, children) :: tail =>
            go(children ::: tail)
          case (b @ Branch(groupId = gid, children = children)) :: tail =>
            val hasObs = children.exists:
              case Leaf(obsId) => obsId === oid
              case _           => false
            if predicate(b) && hasObs then
              Some(gid)
            else
              go(children ::: tail)
      go(List(tree))
    }
  }

}
