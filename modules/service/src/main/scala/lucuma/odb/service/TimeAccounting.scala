// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Eq
import cats.kernel.CommutativeMonoid

import lucuma.core.enums.ChargeClass
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.Step
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp

object TimeAccounting {

  case class Event(
    timestamp: Timestamp,
    context:   Context
  )

  case class Context(
    visitId: Visit.Id,
    step:    Option[StepContext]
  )

  object Context {
    given Eq[Context] =
      Eq.by { a => (a.visitId, a.step) }
  }

  case class StepContext(
    atomId:      Atom.Id,
    stepId:      Step.Id,
    chargeClass: ChargeClass
  )

  object StepContext {
    given Eq[StepContext] =
      Eq.by { a => (a.atomId, a.stepId, a.chargeClass) }
  }

  case class Charge(
    categorized:   CategorizedTime,
    uncategorized: TimeSpan
  ) {

    def +|(that: Charge): Charge =
      Charge(
        categorized   +| that.categorized,
        uncategorized +| that.uncategorized
      )

    def sumCharge(c: Context, t: TimeSpan): Charge =
      c.step.map(_.chargeClass).fold(copy(uncategorized = uncategorized +| t)) { chargeClass =>
        copy(categorized = categorized.sumCharge(chargeClass, t))
      }

    def sumCharge(c: Option[ChargeClass], t: TimeSpan): Charge =
      c.fold(copy(uncategorized = uncategorized +| t)) { chargeClass =>
        copy(categorized = categorized.sumCharge(chargeClass, t))
      }

    def uncategorizedAs(c: ChargeClass): CategorizedTime =
      categorized.sumCharge(c, uncategorized)

  }

  object Charge {
    val Zero: Charge =
      Charge(CategorizedTime.Zero, TimeSpan.Zero)

    given Eq[Charge] =
      Eq.by { a => (a.categorized, a.uncategorized) }

    given CommutativeMonoid[Charge] =
      CommutativeMonoid.instance(Zero, _ +| _)
  }


}
