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
   * One set's outcome: the number of steps it contributes, or the expander's
   * complaint for a smart set that is not mapped.  A manual set that cannot
   * build its atom reports that error instead.  Needs no observation, so the
   * availability query can ask about a set before it is saved.
   */
  def resolveSet[F[_]: Monad, S, D](
    expander: SmartGcalExpander[F, S, D],
    static:   S,
    set:      CalibrationSet[D]
  ): F[Either[String, PosInt]] =
    DaytimeCalibration.atom(set) match
      case Right(atom) if set.kind.isSmart => expander.expandAtom(static, atom).map(_.map(a => PosInt.unsafeFrom(a.steps.length)))
      case Right(atom)                     => PosInt.unsafeFrom(atom.steps.length).asRight.pure[F]
      case Left(e)                         => e.asLeft.pure[F]

  /** `resolveSet` for each set of a daytime calibration, in order, dispatching on its instrument. */
  def resolve[F[_]: Monad](
    instrument: Option[DaytimeInstrument[F]],
    cfg:        Config
  ): F[List[Either[String, PosInt]]] =
    instrument match
      case Some(di) => di.sets(cfg).traverse(resolveSet(di.expander, di.static, _))
      case None     => cfg.sets.map(_.explicitCount.toRight(s"${cfg.instrument.longName} has no daytime calibrations")).pure[F]

  /** The step count of each set, in order: none when the set cannot be expanded. */
  def setCounts[F[_]: Monad](
    instrument: Option[DaytimeInstrument[F]],
    cfg:        Config
  ): F[List[Option[PosInt]]] =
    resolve(instrument, cfg).map(_.map(_.toOption))
