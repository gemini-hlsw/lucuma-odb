// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.Monad
import cats.data.EitherT
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.model.Observation
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.SmartGcalExpander
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.daytimecal.CalibrationSet
import lucuma.odb.sequence.daytimecal.Config
import lucuma.odb.sequence.daytimecal.DaytimeCalibration

/**
 * Resolves the smart rows of a daytime calibration through the instrument's
 * Smart GCAL expander: a smart set's single smart step becomes the steps the
 * lookup returns, inside the set's own atom.
 */
object DaytimeCalibrationExpansion:

  /** One set's atom with its smart step resolved; the error names the set. */
  def expandSet[F[_]: Monad, S, D](
    expander: SmartGcalExpander[F, S, D],
    static:   S,
    oid:      Observation.Id,
    index:    Int,
    set:      CalibrationSet[D]
  ): F[Either[OdbError, ProtoAtom[ProtoStep[D]]]] =
    DaytimeCalibration.protoAtom(oid, index, set) match
      case Right(atom) if set.kind.isSmart =>
        expander.expandAtom(static, atom).map(_.leftMap(DaytimeCalibration.lookupFailed(oid, index, set.kind, _)))
      case other                           =>
        other.pure[F]

  def expandAtoms[F[_]: Monad, S, D](
    expander: SmartGcalExpander[F, S, D],
    static:   S,
    oid:      Observation.Id,
    sets:     List[CalibrationSet[D]]
  ): F[Either[OdbError, List[ProtoAtom[ProtoStep[D]]]]] =
    sets.zipWithIndex.traverse((set, index) => EitherT(expandSet(expander, static, oid, index, set))).value

  /**
   * Each set's outcome, in order: the number of steps it contributes, or the
   * expander's complaint for a smart set that is not mapped.  A manual set
   * that cannot build its atom reports that error instead.
   */
  def resolveSets[F[_]: Monad, S, D](
    expander: SmartGcalExpander[F, S, D],
    static:   S,
    oid:      Observation.Id,
    sets:     List[CalibrationSet[D]]
  ): F[List[Either[String, PosInt]]] =
    sets.zipWithIndex.traverse: (set, index) =>
      DaytimeCalibration.protoAtom(oid, index, set) match
        case Right(atom) if set.kind.isSmart => expander.expandAtom(static, atom).map(_.map(a => PosInt.unsafeFrom(a.steps.length)))
        case Right(atom)                     => PosInt.unsafeFrom(atom.steps.length).asRight.pure[F]
        case Left(e)                         => e.message.asLeft.pure[F]

  /** `resolveSets` for a whole daytime calibration, dispatching on its instrument. */
  def resolve[F[_]: Monad](
    instrument: Option[DaytimeInstrument[F]],
    oid:        Observation.Id,
    cfg:        Config
  ): F[List[Either[String, PosInt]]] =
    instrument match
      case Some(di) => resolveSets(di.expander, di.static, oid, di.sets(cfg))
      case None     => cfg.sets.map(_.explicitCount.toRight(s"${cfg.instrument.longName} has no daytime calibrations")).pure[F]
