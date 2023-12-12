// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Eq
import lucuma.core.enums.ChargeClass
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
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
   * accounting is performed for each visit so there's always a `Visit.Id`.
   * The `ChargeClass` will come either from the associated step (if any) or
   * else the default charge class for the visit. For time spent executing steps
   * there will be a `StepContext` as well.
   */
  case class Context(
    visitId:     Visit.Id,
    chargeClass: ChargeClass,
    step:        Option[StepContext]
  )

  object Context {
    given Eq[Context] =
      Eq.by { a => (a.visitId, a.chargeClass, a.step) }

    val visitId: Lens[Context, Visit.Id] =
      Focus[Context](_.visitId)

    val chargeClass: Lens[Context, ChargeClass] =
      Focus[Context](_.chargeClass)

    val step: Lens[Context, Option[StepContext]] =
      Focus[Context](_.step)
  }

  /**
   * Step context. We keep up with the atom so that we can discount time for an
   * entire atom when necessary.
   */
  case class StepContext(
    atomId:      Atom.Id,
    stepId:      Step.Id
  )

  object StepContext {
    given Eq[StepContext] =
      Eq.by { a => (a.atomId, a.stepId) }

    val atomId: Lens[StepContext, Atom.Id] =
      Focus[StepContext](_.atomId)

    val stepId: Lens[StepContext, Step.Id] =
      Focus[StepContext](_.stepId)
  }

}
