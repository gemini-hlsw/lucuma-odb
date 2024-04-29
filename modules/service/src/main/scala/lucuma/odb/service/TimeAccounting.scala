// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Eq
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

  val CategorizedTimeMax: CategorizedTime =
    CategorizedTime(
      ChargeClass.NonCharged -> TimeSpan.Max,
      ChargeClass.Partner    -> TimeSpan.Max,
      ChargeClass.Program    -> TimeSpan.Max
    )

  object comment {
    val NoData: String   = "Time spent during a visit in which no dataset sets were collected."
    val PreDusk: String  = "Time spent observing pre-dusk (nautical twilight)."
    val PostDawn: String = "Time spent observing post-dawn (nautical twilight)."
    val Qa: String       = "Time spent observing atoms with one or more datasets that don't pass QA."
  }

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
   * there will be an `AtomContext` as well.
   */
  case class Context(
    visitId:     Visit.Id,
    chargeClass: ChargeClass,
    atom:        Option[AtomContext]
  )

  object Context {
    given Eq[Context] =
      Eq.by { a => (a.visitId, a.chargeClass, a.atom) }

    val visitId: Lens[Context, Visit.Id] =
      Focus[Context](_.visitId)

    val chargeClass: Lens[Context, ChargeClass] =
      Focus[Context](_.chargeClass)

    val atom: Lens[Context, Option[AtomContext]] =
      Focus[Context](_.atom)
  }


  /**
   * Atom context. We keep up with the atom so that we can discount time for an
   * entire atom when necessary.
   */
  case class AtomContext(
    atomId: Atom.Id,
    stepId: Option[Step.Id]
  )

  object AtomContext {
    given Eq[AtomContext] =
      Eq.by { a => (a.atomId, a.stepId) }

    val atomId: Lens[AtomContext, Atom.Id] =
      Focus[AtomContext](_.atomId)

    val stepId: Lens[AtomContext, Option[Step.Id]] =
      Focus[AtomContext](_.stepId)
  }

}
