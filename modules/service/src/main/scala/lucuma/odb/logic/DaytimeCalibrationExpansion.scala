// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.Monad
import cats.data.EitherT
import cats.syntax.all.*
import eu.timepit.refined.types.numeric.PosInt
import lucuma.core.enums.Instrument
import lucuma.core.model.Observation
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.SmartGcalExpander
import lucuma.odb.sequence.data.ProtoAtom
import lucuma.odb.sequence.data.ProtoStep
import lucuma.odb.sequence.daytimecal.CalibrationSet
import lucuma.odb.sequence.daytimecal.Config
import lucuma.odb.sequence.daytimecal.DaytimeCalibration
import lucuma.odb.sequence.daytimecal.StaticConfigs as DaytimeStatics

/**
 * Resolves the smart rows of a daytime calibration through the instrument's
 * Smart GCAL expander: a smart set's single smart step becomes the steps the
 * lookup returns, inside the set's own atom.
 */
object DaytimeCalibrationExpansion:

  def expandAtoms[F[_]: Monad, S, D](
    expander: SmartGcalExpander[F, S, D],
    static:   S,
    oid:      Observation.Id,
    sets:     List[CalibrationSet[D]]
  ): F[Either[OdbError, List[ProtoAtom[ProtoStep[D]]]]] =
    EitherT
      .fromEither[F](DaytimeCalibration.protoAtoms(oid, sets))
      .flatMap: atoms =>
        atoms.zip(sets).zipWithIndex.traverse: (pair, index) =>
          val (atom, set) = pair
          if set.kind.isSmart then
            EitherT(expander.expandAtom(static, atom))
              .leftMap(e => DaytimeCalibration.lookupFailed(oid, index, set.kind, e))
          else
            EitherT.rightT[F, OdbError](atom)
      .value

  /**
   * The exposure count of each set, in order: a manual set's explicit count, the
   * number of steps a smart set's lookup returns, or none when the lookup
   * finds nothing.
   */
  def setCounts[F[_]: Monad, S, D](
    expander: SmartGcalExpander[F, S, D],
    static:   S,
    oid:      Observation.Id,
    sets:     List[CalibrationSet[D]]
  ): F[List[Option[PosInt]]] =
    sets.zipWithIndex.traverse: (set, index) =>
      if set.kind.isSmart then
        DaytimeCalibration.protoStep(oid, index, set).toOption.flatTraverse: step =>
          expander.expandStep(static, step).map(_.toOption.flatMap(s => PosInt.from(s.length).toOption))
      else
        set.explicitCount.pure[F]

  /** `setCounts` for a whole daytime calibration, dispatching on its instrument. */
  def setCounts[F[_]: Monad](
    exp: SmartGcalImplementation.ForInstrument[F],
    oid: Observation.Id,
    cfg: Config
  ): F[List[Option[PosInt]]] =
    cfg.instrument match
      case Instrument.Flamingos2 => setCounts(exp.flamingos2, DaytimeStatics.Flamingos2, oid, cfg.flamingos2Sets)
      case Instrument.GmosNorth  => setCounts(exp.gmosNorth,  DaytimeStatics.GmosNorth,  oid, cfg.gmosNorthSets)
      case Instrument.GmosSouth  => setCounts(exp.gmosSouth,  DaytimeStatics.GmosSouth,  oid, cfg.gmosSouthSets)
      case _                     => cfg.sets.map(_.explicitCount).pure[F]
