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
import monocle.Focus
import monocle.Lens

/**
 * TimeAccounting data, used only in the implementation of the
 * TimeAccountingService.
 */
object TimeAccounting {

  /**
   * Information of interest (for time accounting) in execution events. A series
   * of events may share the same context. For example, a dataset has several
   * stages each resulting in a separate event being stored in the database.  We
   * will group contiguous events with the same context to form a
   * (TimestampInterval -> Context) entry in the TimeAccountingState.
   */
  case class Event(
    timestamp: Timestamp,
    context:   Context
  )

  object Event {
    given Eq[Event] =
      Eq.by { a => (a.timestamp, a.context) }

    val timestamp: Lens[Event, Timestamp] =
      Focus[Event](_.timestamp)

    val context: Lens[Event, Context] =
      Focus[Event](_.context)
  }

  /**
   * The context for an event or a period of time accounting time.  Time
   * accounting is performed for each visit so there's always a `Visit.Id`, and
   * for time spent executing steps there will be a `StepContext` as well.
   */
  case class Context(
    visitId: Visit.Id,
    step:    Option[StepContext]
  )

  object Context {
    given Eq[Context] =
      Eq.by { a => (a.visitId, a.step) }

    val visitId: Lens[Context, Visit.Id] =
      Focus[Context](_.visitId)

    val step: Lens[Context, Option[StepContext]] =
      Focus[Context](_.step)
  }

  /**
   * Step context, describing which charge class will receive the time required
   * to execute the step.  We keep up with the atom so that we can discount time
   * for an entire atom when necessary.
   */
  case class StepContext(
    atomId:      Atom.Id,
    stepId:      Step.Id,
    chargeClass: ChargeClass
  )

  object StepContext {
    given Eq[StepContext] =
      Eq.by { a => (a.atomId, a.stepId, a.chargeClass) }

    val atomId: Lens[StepContext, Atom.Id] =
      Focus[StepContext](_.atomId)

    val stepId: Lens[StepContext, Step.Id] =
      Focus[StepContext](_.stepId)

    val chargeClass: Lens[StepContext, ChargeClass] =
      Focus[StepContext](_.chargeClass)
  }

  /**
   * A time accounting charge includes time for which we have a definite charge
   * class (e.g., all steps) and time not associated with any charge class
   * (e.g., time between steps and atoms).  The uncategorized time is ultimately
   * charged according to the observation's observe class.
   */
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
