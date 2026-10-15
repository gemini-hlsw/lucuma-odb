// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Eq
import lucuma.core.enums.ChargeClass
import lucuma.core.enums.SequenceType
import lucuma.core.enums.SetupKind
import lucuma.core.enums.SlewStage
import lucuma.core.model.Visit
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.SetupTimeBreakdown
import lucuma.core.model.sequence.Step
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
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
      ChargeClass.Program    -> TimeSpan.Max
    )

  object comment {
    val NoData: String   = "Time spent during a visit in which no dataset sets were collected."
    val Overlap: String  = "Time spent observing after an overlapping chargeable observation started."
    val PreDusk: String  = "Time spent observing pre-dusk (nautical twilight)."
    val PostDawn: String = "Time spent observing post-dawn (nautical twilight)."
    val Qa: String       = "Time spent observing atoms with one or more datasets that don't pass QA."
  }

  /**
   * Information of interest (for time accounting) in execution events. A series
   * of events may share the same context. For example, a dataset has several
   * stages each resulting in a separate event being stored in the database.  We
   * will group contiguous events with the same context to form a
   * (TimestampInterval -> Context) entry in the TimeAccountingState.  Slew
   * events carry their stage, from which the `slewing` flag of the context is
   * determined when the event stream is processed.
   */
  case class Event(
    timestamp: Timestamp,
    context:   Context,
    slewStage: Option[SlewStage]
  )

  object Event {
    given Eq[Event] =
      Eq.by { a => (a.timestamp, a.context, a.slewStage) }

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
   * there will be an `AtomContext` as well.  Time spent between a slew start
   * and the slew end (or the next atom event) is marked `slewing`.
   */
  case class Context(
    visitId:     Visit.Id,
    chargeClass: ChargeClass,
    atom:        Option[AtomContext],
    slewing:     Boolean
  )

  object Context {
    given Eq[Context] =
      Eq.by { a => (a.visitId, a.chargeClass, a.atom, a.slewing) }

    val visitId: Lens[Context, Visit.Id] =
      Focus[Context](_.visitId)

    val chargeClass: Lens[Context, ChargeClass] =
      Focus[Context](_.chargeClass)

    val atom: Lens[Context, Option[AtomContext]] =
      Focus[Context](_.atom)

    val slewing: Lens[Context, Boolean] =
      Focus[Context](_.slewing)
  }


  /**
   * Atom context. We keep up with the atom so that we can discount time for an
   * entire atom when necessary, and with its sequence type so that we can
   * distinguish acquisition from science.
   */
  case class AtomContext(
    atomId:       Atom.Id,
    stepId:       Option[Step.Id],
    sequenceType: SequenceType
  )

  object AtomContext {
    given Eq[AtomContext] =
      Eq.by { a => (a.atomId, a.stepId, a.sequenceType) }

    val atomId: Lens[AtomContext, Atom.Id] =
      Focus[AtomContext](_.atomId)

    val stepId: Lens[AtomContext, Option[Step.Id]] =
      Focus[AtomContext](_.stepId)

    val sequenceType: Lens[AtomContext, SequenceType] =
      Focus[AtomContext](_.sequenceType)
  }

  /**
   * A setup found in a visit: a full setup or a reacquisition, the interval it
   * spans, and the time charged for it.
   */
  case class Setup(
    kind:     SetupKind,
    interval: TimestampInterval,
    charge:   SetupTimeBreakdown
  )

  object Setup {
    given Eq[Setup] =
      Eq.by { a => (a.kind, a.interval, a.charge) }
  }

}
