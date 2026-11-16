// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.syntax

import cats.syntax.either.*
import cats.syntax.option.*
import lucuma.core.enums.GroupConstraintViolationCode
import lucuma.core.model.Group
import lucuma.core.model.Observation
import lucuma.core.model.ObservingNight
import lucuma.core.model.group.GroupConstraintViolation.Waivable
import lucuma.core.syntax.string.*

// Waivable violations as their stored, transported and input forms give them:
// a code, and the element or the night where the violation happened.
trait ToGroupConstraintViolationOps:

  extension (self: GroupConstraintViolationCode)

    /** Whether violations of the code can be waived. */
    def isWaivable: Boolean =
      self match
        case GroupConstraintViolationCode.Order                       |
             GroupConstraintViolationCode.MinimumInterval             |
             GroupConstraintViolationCode.MaximumInterval             |
             GroupConstraintViolationCode.SameNight                   |
             GroupConstraintViolationCode.ObservationsPerNightMinimum |
             GroupConstraintViolationCode.ObservationsPerNightMaximum => true
        case _                                                        => false

    /** The code, if violations of it can be waived, or else why not. */
    def checkWaivable: Either[String, GroupConstraintViolationCode] =
      Either.cond(self.isWaivable, self, cantBeWaived(self))

  extension (self: Waivable.type)

    /**
     * The violation of `code` at the location given, if violations of the
     * code can be waived and the location is exactly the one the code needs.
     */
    def fromLocation(
      code:    GroupConstraintViolationCode,
      element: Option[Either[Group.Id, Observation.Id]],
      night:   Option[ObservingNight]
    ): Either[String, Waivable] =
      val name = code.tag.toScreamingSnakeCase

      def atElement(make: Either[Group.Id, Observation.Id] => Waivable): Either[String, Waivable] =
        (element, night) match
          case (Some(e), None) => make(e).asRight
          case _               => s"$name violations are identified by their element, without a night.".asLeft

      def onNight(make: ObservingNight => Waivable): Either[String, Waivable] =
        (element, night) match
          case (None, Some(n)) => make(n).asRight
          case _               => s"$name violations are identified by their night, without an element.".asLeft

      code match
        case GroupConstraintViolationCode.Order                       => atElement(Waivable.Order(_))
        case GroupConstraintViolationCode.MinimumInterval             => atElement(Waivable.MinimumInterval(_))
        case GroupConstraintViolationCode.MaximumInterval             => atElement(Waivable.MaximumInterval(_))
        case GroupConstraintViolationCode.SameNight                   => onNight(Waivable.SameNight(_))
        case GroupConstraintViolationCode.ObservationsPerNightMinimum => onNight(Waivable.ObservationsPerNightMinimum(_))
        case GroupConstraintViolationCode.ObservationsPerNightMaximum => onNight(Waivable.ObservationsPerNightMaximum(_))
        case _                                                        => cantBeWaived(code).asLeft

  extension (self: Waivable)

    /** The element where the violation happened, for an element's violation. */
    def element: Option[Either[Group.Id, Observation.Id]] =
      self match
        case Waivable.Order(e)           => e.some
        case Waivable.MinimumInterval(e) => e.some
        case Waivable.MaximumInterval(e) => e.some
        case _                           => none

    /** The night when the violation happened, for a night's violation. */
    def night: Option[ObservingNight] =
      self match
        case Waivable.SameNight(n)                   => n.some
        case Waivable.ObservationsPerNightMinimum(n) => n.some
        case Waivable.ObservationsPerNightMaximum(n) => n.some
        case _                                       => none

  private def cantBeWaived(code: GroupConstraintViolationCode): String =
    s"${code.tag.toScreamingSnakeCase} violations can't be waived."

object groupConstraintViolation extends ToGroupConstraintViolationOps
