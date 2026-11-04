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
   * The step count of each set, in order, read off the same expansion that
   * builds its atom: none when the set cannot be expanded.
   */
  def setCounts[F[_]: Monad, S, D](
    expander: SmartGcalExpander[F, S, D],
    static:   S,
    oid:      Observation.Id,
    sets:     List[CalibrationSet[D]]
  ): F[List[Option[PosInt]]] =
    sets.zipWithIndex.traverse: (set, index) =>
      expandSet(expander, static, oid, index, set).map(_.toOption.flatMap(a => PosInt.from(a.steps.length).toOption))

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
      case Instrument.Gnirs      => setCounts(exp.gnirs,      DaytimeStatics.Gnirs,      oid, cfg.gnirsSets)
      case Instrument.Igrins2    => setCounts(exp.igrins2,    DaytimeStatics.Igrins2,    oid, cfg.igrins2Sets)
      case _                     => cfg.sets.map(_.explicitCount).pure[F]
