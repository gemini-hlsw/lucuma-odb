// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.Monad
import cats.data.EitherT
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.NonNegInt
import lucuma.core.model.Observation
import lucuma.core.model.sequence.StepViolation
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.SmartGcalExpander
import lucuma.odb.sequence.SmartGcalExpander.Calibration
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.daytimecal.CalibrationSet
import lucuma.odb.sequence.daytimecal.Config
import lucuma.odb.sequence.daytimecal.DaytimeCalibration

/**
 * Resolves the smart rows of a daytime calibration through the instrument's
 * Smart GCAL expander: a smart set's single smart step becomes the steps the
 * lookup returns.  A smart set without a Smart GCAL definition is left out and
 * reported.  The steps of every set end up in a single atom.
 */
object DaytimeCalibrationExpansion:

  /**
   * One set's contribution to the sequence.
   *
   * @param missing the calibrations left out for want of a definition
   * @param atom    the set's steps, absent when its lookup yields none
   */
  case class Expansion[D](
    missing: List[StepViolation],
    atom:    Option[ProtoAtom[ProtoStep[D]]]
  )

  /** One set's steps with its smart step resolved; an error names the set. */
  def expandSet[F[_]: Monad, S, D](
    expander: SmartGcalExpander[F, S, D],
    static:   S,
    oid:      Observation.Id,
    index:    Int,
    set:      CalibrationSet[D]
  ): F[Either[OdbError, Expansion[D]]] =
    DaytimeCalibration.protoAtom(oid, index, set).traverse: atom =>
      expander.expandAtom(static, atom, Calibration.DaytimeSet(set.kind)).map: (missing, expanded) =>
        // Numbered from one, as the user sees the rows.
        Expansion(missing.map(v => v.copy(message = s"Calibration set ${index + 1}: ${v.message}")), expanded)

  /**
   * The single atom holding the steps of every set, in order, with the
   * calibrations left out.  Sets that leave the sequence with no step at all
   * are an error.
   */
  def expandAtoms[F[_]: Monad, S, D](
    expander: SmartGcalExpander[F, S, D],
    static:   S,
    oid:      Observation.Id,
    sets:     List[CalibrationSet[D]]
  ): F[Either[OdbError, (List[StepViolation], List[ProtoAtom[ProtoStep[D]]])]] =
    sets.zipWithIndex
      .traverse((set, index) => EitherT(expandSet(expander, static, oid, index, set)))
      .subflatMap: es =>
        val missing = es.flatMap(_.missing)
        val atoms   = es.flatMap(_.atom)
        if sets.isEmpty then (missing, atoms).asRight
        else
          SmartGcalExpander
            .remaining(missing, atoms)
            .bimap(m => OdbError.SequenceUnavailable(oid, m.some), (m, as) => (m, DaytimeCalibration.singleAtom(as.flatMap(_.steps.toList))))
      .value

  /**
   * The step count of each set, in order, read off the same expansion that
   * builds its steps: zero when its lookup defines no steps, none when the set
   * cannot be expanded or has no definition.
   */
  def setCounts[F[_]: Monad, S, D](
    expander: SmartGcalExpander[F, S, D],
    static:   S,
    oid:      Observation.Id,
    sets:     List[CalibrationSet[D]]
  ): F[List[Option[NonNegInt]]] =
    sets.zipWithIndex.traverse: (set, index) =>
      expandSet(expander, static, oid, index, set).map:
        _.toOption.filter(_.missing.isEmpty).flatMap(e => NonNegInt.from(e.atom.foldMap(_.steps.length)).toOption)

  /** `setCounts` for a whole daytime calibration, dispatching on its instrument. */
  def setCounts[F[_]: Monad](
    instrument: Option[DaytimeInstrument[F]],
    oid:        Observation.Id,
    cfg:        Config
  ): F[List[Option[NonNegInt]]] =
    instrument match
      case Some(di) => setCounts(di.expander, di.static, oid, di.sets(cfg))
      case None     => cfg.sets.map(_.explicitCount.flatMap(n => NonNegInt.from(n.value).toOption)).pure[F]
