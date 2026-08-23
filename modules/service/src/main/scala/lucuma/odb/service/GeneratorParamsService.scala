// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Eq
import cats.Order
import cats.data.NonEmptyList
import cats.data.Validated
import cats.data.ValidatedNel
import cats.derived.*
import cats.effect.Concurrent
import cats.syntax.applicative.*
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.eq.*
import cats.syntax.flatMap.*
import cats.syntax.foldable.*
import cats.syntax.functor.*
import cats.syntax.functorFilter.*
import cats.syntax.option.*
import cats.syntax.traverse.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.DeclaredExecutionState
import lucuma.core.enums.ExecutionState
import lucuma.core.enums.Flamingos2ReadMode
import lucuma.core.enums.GnirsFilter
import lucuma.core.enums.GnirsReadMode
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.SchedulingMode
import lucuma.core.enums.ScienceBand
import lucuma.core.math.RadialVelocity
import lucuma.core.model.ConstraintSet
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.SourceProfile
import lucuma.core.model.Target
import lucuma.core.model.UnnormalizedSED
import lucuma.core.model.User
import lucuma.core.util.Timestamp
import lucuma.itc.ItcGhostDetector
import lucuma.itc.client.Flamingos2CustomMask
import lucuma.itc.client.Flamingos2FpuMask
import lucuma.itc.client.GmosCustomMask
import lucuma.itc.client.GmosFpu
import lucuma.itc.client.ImagingParameters
import lucuma.itc.client.InstrumentMode
import lucuma.itc.client.ItcConstraintsInput.*
import lucuma.itc.client.SpectroscopyParameters
import lucuma.itc.client.TargetInput
import lucuma.odb.json.sourceprofile.given
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.ItcInput
import lucuma.odb.sequence.data.ItcInputDerivation
import lucuma.odb.sequence.data.MissingParam
import lucuma.odb.sequence.data.MissingParamSet
import lucuma.odb.sequence.exchange
import lucuma.odb.sequence.flamingos2
import lucuma.odb.sequence.ghost
import lucuma.odb.sequence.gnirs
import lucuma.odb.sequence.igrins2
import lucuma.odb.sequence.imaging.ImagingSequence
import lucuma.odb.sequence.visitor
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.circe.codec.json.*
import skunk.codec.boolean.bool
import skunk.codec.numeric.int8
import skunk.implicits.*

import GeneratorParamsService.Error
import Services.Syntax.*

enum ObservationSelection derives Order:
  case All
  case Science
  case Calibration

trait GeneratorParamsService[F[_]] {

  def selectExecutionStates(
    observationIds: List[Observation.Id]
  )(using Transaction[F]): F[Map[Observation.Id, ExecutionState]]

  def selectOne(
    programId:     Program.Id,
    observationId: Observation.Id
  )(using Transaction[F]): F[Either[Error, GeneratorParams]]

  def selectMany(
    programId:      Program.Id,
    observationIds: List[Observation.Id]
  )(using Transaction[F]): F[Map[Observation.Id, Either[Error, GeneratorParams]]]

  def selectMany(
    observationIds: List[Observation.Id]
  )(using Transaction[F]): F[Map[Observation.Id, Either[Error, GeneratorParams]]]

  def selectAll(
    programId: Program.Id,
    selection: ObservationSelection = ObservationSelection.All
  )(using Transaction[F]): F[Map[Observation.Id, Either[Error, GeneratorParams]]]

}

object GeneratorParamsService {

  sealed trait Error extends Product with Serializable:
    def format: String

  object Error:
    case class MisconfiguredObservation(observationId: Observation.Id, msg: String) extends Error:
      def format: String =
        s"Observation '$observationId' is misconfigured: $msg."

    case class MissingObservation(programId: Program.Id, observationId: Observation.Id) extends Error:
      def format: String =
        s"Observation '$observationId' in program '$programId' not found."

    case class MissingData(params: MissingParamSet) extends Error:
      def format: String = params.format

    case object ConflictingData extends Error:
      def format: String =
        "Conflicting data, all stars in the asterism must use the same observing mode and parameters."

    given Eq[Error] with
      def eqv(x: Error, y: Error): Boolean =
        (x, y) match
          case (MisconfiguredObservation(o0, m0), MisconfiguredObservation(o1, m1)) => (o0 === o1) && (m0 === m1)
          case (MissingObservation(p0, o0), MissingObservation(p1, o1))             => (p0 === p1) && (o0 === o1)
          case (MissingData(p0), MissingData(p1))                                   => p0 === p1
          case (ConflictingData, ConflictingData)                                   => true
          case _                                                                    => false

  def instantiate[F[_]: Concurrent](using Services[F]): GeneratorParamsService[F] =
    new GeneratorParamsService[F] {

      import lucuma.odb.sequence.gmos

      val customSedIdOptional = SourceProfile.unnormalizedSED.some.andThen(UnnormalizedSED.userDefinedAttachment).andThen(UnnormalizedSED.UserDefinedAttachment.attachmentId)

      override def selectExecutionStates(
        oids: List[Observation.Id]
      )(using Transaction[F]): F[Map[Observation.Id, ExecutionState]] =
        NonEmptyList
          .fromList(oids)
          .fold(Map.empty[Observation.Id, ExecutionState].pure[F]): which =>
            val af = Statements.selectExecutionStates(which)
            session
              .prepareR(af.fragment.query(observation_id *: execution_state))
              .use(_.stream(af.argument, chunkSize = 1024).compile.to(List))
              .map(_.toMap)

      override def selectOne(
        pid: Program.Id,
        oid: Observation.Id
      )(using Transaction[F]): F[Either[Error, GeneratorParams]] =
        selectMany(pid, List(oid)).map(_.getOrElse(oid, Error.MissingObservation(pid, oid).asLeft))

      override def selectMany(
        pid:  Program.Id,
        oids: List[Observation.Id]
      )(using Transaction[F]): F[Map[Observation.Id, Either[Error, GeneratorParams]]] =
        doSelect(selectManyParams(pid, oids))

      override def selectMany(
        oids: List[Observation.Id]
      )(using Transaction[F]): F[Map[Observation.Id, Either[Error, GeneratorParams]]] =
        doSelect(selectManyParams(oids))

      override def selectAll(
        pid:       Program.Id,
        selection: ObservationSelection
      )(using Transaction[F]): F[Map[Observation.Id, Either[Error, GeneratorParams]]] =
        doSelect(selectAllParams(pid, selection))

      private def doSelect(
        params: F[List[ParamsRow]]
      )(using Transaction[F]): F[Map[Observation.Id, Either[Error, GeneratorParams]]] =
        for
          paramsRows <- params
          oms         = paramsRows.collect { case ParamsRow(observationId = oid, observingMode = Some(om)) => (oid, om) }.distinct
          m          <- Services.asSuperUser(observingModeServices.selectObservingMode(oms))
        yield
          NonEmptyList.fromList(paramsRows).fold(Map.empty): paramsRowsNel =>
            ObsParams.fromParamsRows(paramsRowsNel).map: (obsId, obsParams) =>
              obsId -> toObsGeneratorParams(obsParams, m.get(obsId))

      private def selectManyParams(
        pid:  Program.Id,
        oids: List[Observation.Id]
      ): F[List[ParamsRow]] =
        NonEmptyList
          .fromList(oids)
          .fold(List.empty[ParamsRow].pure[F]) { oids =>
            executeSelect(Statements.selectManyParams(user, pid, oids))
          }

      private def selectManyParams(
        oids: List[Observation.Id]
      ): F[List[ParamsRow]] =
        NonEmptyList
          .fromList(oids)
          .fold(List.empty[ParamsRow].pure[F]) { oids =>
            executeSelect(Statements.selectManyParams(oids))
          }

      private def selectAllParams(
        pid:       Program.Id,
        selection: ObservationSelection
      ): F[List[ParamsRow]] =
        executeSelect(Statements.selectAllParams(user, pid, /*minStatus,*/ selection))

      private def executeSelect(af: AppliedFragment): F[List[ParamsRow]] =
        session
          .prepareR(af.fragment.query(Statements.params))
          .use(_.stream(af.argument, chunkSize = 64).compile.to(List))
          .flatMap(addCustomSedTimestamps)

      // If the user uploads a new custom sed in place of an existing one, that needs to
      // invalidate the cache. So, we include the timestamp of the attachment (if any) in
      // the hash.
      private def addCustomSedTimestamps(params: List[ParamsRow]): F[List[ParamsRow]] =
        NonEmptyList.fromList(params.map(p => p.sourceProfile.flatMap(customSedIdOptional.getOption)).flattenOption)
          .fold(params.pure)(attIds =>
            Services.asSuperUser(attachmentMetadataService.getUpdatedAt(attIds)).map(map =>
              params.map(p =>
                val aid = p.sourceProfile.flatMap(customSedIdOptional.getOption)
                aid.fold(p)(id => p.copy(customSedTimestamp = map.get(id)))
              )
            )
          )

      private def observingMode(
        params:          NonEmptyList[TargetParams],
        config:          Option[ObservingMode],
        calibrationRole: Option[CalibrationRole]
      ): Either[Error, ObservingMode] =
        // A daytime pinhole flat is an internal GCAL calibration with no target.
        // It needs no asterism (and no ITC), so skip the target requirement; its
        // sequence is a single smart day flat that ignores target information.
        val targetCheck =
          if calibrationRole.contains(CalibrationRole.DaytimePinhole) then
            ().asRight[NonEmptyList[MissingParam]]
          else
            params
              .traverse: p =>
                for
                  t <- p.targetId.toRightNel(MissingParam.forObservation("target"))
                  _ <- p.sourceProfile.toRightNel(MissingParam.forTarget(t, "source profile"))
                yield ()
              .void

        val configCheck =
          for
            _ <- targetCheck
            c <- config.toRightNel(MissingParam.forObservation("observing mode"))
          yield c

        configCheck.leftMap(nel => Error.MissingData(MissingParamSet.fromParams(nel)))

      private def toObsGeneratorParams(
        obsParams: ObsParams,
        config:    Option[ObservingMode]
      ): Either[Error, GeneratorParams] =

        def spectroscopyGeneratorParams(
          obsMode:              ObservingMode,
          acqMode:              InstrumentMode,
          sciMode:              InstrumentMode,
          gnirsAcqAutoClassify: Boolean = false
        ): GeneratorParams =

          val consInput   = obsParams.constraints.toInput
          val acquisition = ImagingParameters(consInput, acqMode)
          val science     = SpectroscopyParameters(consInput, sciMode)

          val itcInput    = (
             obsParams.targets.traverse(itcTargetParams),
             // the db guarantees at most one BO
             obsParams.blindOffset.traverse(itcTargetParams)
            ).mapN { case (regularTargetInputs, blindOffsetTargetInput) =>
              ItcInput.Spectroscopy(
                acquisition,
                science,
                regularTargetInputs,
                blindOffsetTargetInput,
                obsParams.signalToNoiseTargetId,
                gnirsAcqAutoClassify
              )
            }
            .leftMap(MissingParamSet.fromParams)
            .toEither

          GeneratorParams(ItcInputDerivation.fromEither(itcInput), obsParams.scienceBand, obsMode, obsParams.calibrationRole, obsParams.declaredState, obsParams.executionState, obsParams.stepCount, obsParams.schedulingMode.isSplittable)

        /**
         * Modes with no acquisition sequence are costed on science alone.
         */
        def scienceOnlySpectroscopyGeneratorParams(
          obsMode: ObservingMode,
          sciMode: InstrumentMode
        ): GeneratorParams =

          val science  = SpectroscopyParameters(obsParams.constraints.toInput, sciMode)

          val itcInput =
            obsParams.targets
              .traverse(itcTargetParams)
              .map(ItcInput.ScienceOnlySpectroscopy(science, _, obsParams.signalToNoiseTargetId))
              .leftMap(MissingParamSet.fromParams)
              .toEither

          GeneratorParams(ItcInputDerivation.fromEither(itcInput), obsParams.scienceBand, obsMode, obsParams.calibrationRole, obsParams.declaredState, obsParams.executionState, obsParams.stepCount, obsParams.schedulingMode.isSplittable)

        /**
         * GNIRS spectroscopy takes spectra at one or more central wavelengths,
         * each its own ITC calculation, so it gets a list of science modes where
         * the other spectroscopy modes have exactly one.
         */
        def gnirsSpectroscopyGeneratorParams(
          obsMode:                   ObservingMode,
          acqMode:                   InstrumentMode,
          sciModes:                  NonEmptyList[InstrumentMode],
          gnirsAcqAutoClassify:      Boolean,
          gnirsAcqAutoSignalToNoise: Boolean
        ): GeneratorParams =

          val consInput   = obsParams.constraints.toInput
          val acquisition = ImagingParameters(consInput, acqMode)
          val science     = sciModes.map(SpectroscopyParameters(consInput, _))

          val itcInput    = (
             obsParams.targets.traverse(itcTargetParams),
             // the db guarantees at most one BO
             obsParams.blindOffset.traverse(itcTargetParams)
            ).mapN { case (regularTargetInputs, blindOffsetTargetInput) =>
              ItcInput.GnirsSpectroscopy(
                acquisition,
                science,
                regularTargetInputs,
                blindOffsetTargetInput,
                obsParams.signalToNoiseTargetId,
                gnirsAcqAutoClassify,
                gnirsAcqAutoSignalToNoise
              )
            }
            .leftMap(MissingParamSet.fromParams)
            .toEither

          GeneratorParams(ItcInputDerivation.fromEither(itcInput), obsParams.scienceBand, obsMode, obsParams.calibrationRole, obsParams.declaredState, obsParams.executionState, obsParams.stepCount, obsParams.schedulingMode.isSplittable)

        // Shared by long slit and MOS.  Signal-to-noise is solved by the ITC, so
        // the read mode it is given is ignored; only Time & Count needs a real one.
        def flamingos2ScienceReadMode(c: flamingos2.spectroscopy.Config): Flamingos2ReadMode =
          c.exposureTimeMode match
            case ExposureTimeMode.SignalToNoiseMode(_, _)       =>
              Flamingos2ReadMode.Bright
            case ExposureTimeMode.TimeAndCountMode(time = time) =>
              c.explicitReadMode.getOrElse(Flamingos2ReadMode.forExposureTime(time))

        observingMode(obsParams.targets, config, obsParams.calibrationRole).flatMap:

          // Exchange Modes (no ITC, like visitors)
          case exc: exchange.Config =>
            GeneratorParams(
              ItcInputDerivation.NotApplicable,
              obsParams.scienceBand,
              exc,
              obsParams.calibrationRole,
              obsParams.declaredState,
              obsParams.executionState,
              obsParams.stepCount,
              obsParams.schedulingMode.isSplittable
            ).asRight

          case f2: flamingos2.longslit.Config =>
            val sciMode   = InstrumentMode.Flamingos2Spectroscopy(f2.exposureTimeMode,
                                                                  f2.disperser,
                                                                  f2.filter,
                                                                  flamingos2ScienceReadMode(f2),
                                                                  Flamingos2FpuMask.builtin(f2.fpu)
            )

            spectroscopyGeneratorParams(
              obsMode = f2,
              acqMode = InstrumentMode.Flamingos2Imaging(
                f2.acquisition.exposureTimeMode,
                f2.acquisition.filter,
                Flamingos2ReadMode.Bright // Default to Bright, may support overrides in the future
              ),
              sciMode = sciMode
            ).asRight

          case f2m: flamingos2.mos.Config =>
            val sciMode = InstrumentMode.Flamingos2Spectroscopy(
              f2m.exposureTimeMode,
              f2m.disperser,
              f2m.filter,
              flamingos2ScienceReadMode(f2m),
              Flamingos2FpuMask.customMask(Flamingos2CustomMask(f2m.customMask.slitWidth))
            )

            spectroscopyGeneratorParams(
              obsMode = f2m,
              acqMode = InstrumentMode.Flamingos2Imaging(
                f2m.acquisition.exposureTimeMode,
                f2m.acquisition.filter,
                Flamingos2ReadMode.Bright // Default to Bright, may support overrides in the future
              ),
              sciMode = sciMode
            ).asRight

          case f2 @ flamingos2.imaging.Config(filters = fs) =>
            // An input per filter.
            val inputs = fs.map: f =>
              ImagingParameters(
                obsParams.constraints.toInput,
                InstrumentMode.Flamingos2Imaging(f.exposureTimeMode, f.filter, f2.readMode)
              )

            val itcInput =
              obsParams
                .targets
                .traverse(itcTargetParams)
                .map(ItcInput.Imaging(inputs, _, obsParams.signalToNoiseTargetId))
                .leftMap(MissingParamSet.fromParams)
                .toEither

            GeneratorParams(ItcInputDerivation.fromEither(itcInput), obsParams.scienceBand, f2, obsParams.calibrationRole, obsParams.declaredState, obsParams.executionState, obsParams.stepCount, obsParams.schedulingMode.isSplittable).asRight

          case gnm @ gnirs.imaging.Config(filters = fs) =>
            // An input per filter. In S/N mode the read mode is derived per step from
            // the ITC exposure time, so the value passed here is ignored by the ITC.
            val inputs = fs.map: f =>
              val readMode = f.exposureTimeMode match
                case ExposureTimeMode.SignalToNoiseMode(_, _)       => GnirsReadMode.Bright
                case ExposureTimeMode.TimeAndCountMode(time = time) => gnm.explicitReadMode.getOrElse(GnirsReadMode.forExposureTime(time))
              ImagingParameters(
                obsParams.constraints.toInput,
                InstrumentMode.GnirsImaging(
                  exposureTimeMode = f.exposureTimeMode,
                  filter           = f.filter,
                  camera           = gnm.camera,
                  readMode         = readMode,
                  wellDepth        = gnm.wellDepth,
                  coadds           = f.coadds
                )
              )

            // The acquisition images the field in the first (wavelength-ordered) science
            // filter — or in the explicitly chosen acquisition filter — to classify the
            // target brightness; the two-pass acquisition ITC then resolves the type
            // (Very Bright / Bright / Faint) and re-images through H2 for Very Bright.
            val firstFilter: GnirsFilter =
              given Order[GnirsFilter] = ImagingSequence.wavelengthOrder(gnm.variant)(_.centralWavelength)
              fs.map(_.filter).sorted.head

            val acqFilter: GnirsFilter =
              gnm.acquisition.explicitFilter.getOrElse(firstFilter)

            val acquisition =
              ImagingParameters(
                obsParams.constraints.toInput,
                InstrumentMode.GnirsImaging(
                  exposureTimeMode = gnm.acquisition.itcExposureTimeMode,
                  filter           = acqFilter,
                  camera           = gnm.camera,
                  readMode         = GnirsReadMode.Bright,
                  wellDepth        = gnm.wellDepth,
                  coadds           = gnm.acquisition.coadds
                )
              )

            // Two-pass acquisition ITC whenever the acquisition mode and filter are both
            // auto: only then does the resolved filter depend on the ITC-derived
            // brightness classification (Very Bright → H2), creating the circularity.
            val acqAutoClassify: Boolean =
              gnm.acquisition.explicitAcqMode.isEmpty &&
              gnm.acquisition.explicitFilter.isEmpty

            // The acquisition S/N is derived from the same classification, which also
            // requires the classification pass even when the filter is explicit.
            val acqAutoSignalToNoise: Boolean =
              gnm.acquisition.autoSignalToNoise

            val itcInput =
              obsParams
                .targets
                .traverse(itcTargetParams)
                .map(ItcInput.Imaging(inputs, _, obsParams.signalToNoiseTargetId, acquisition.some, gnirsAcqAutoClassify = acqAutoClassify, gnirsAcqAutoSignalToNoise = acqAutoSignalToNoise))
                .leftMap(MissingParamSet.fromParams)
                .toEither

            GeneratorParams(ItcInputDerivation.fromEither(itcInput), obsParams.scienceBand, gnm, obsParams.calibrationRole, obsParams.declaredState, obsParams.executionState, obsParams.stepCount, obsParams.schedulingMode.isSplittable).asRight

          case gh @ ghost.ifu.Config(stepCnt, resolutionMode, red, blue, _, _, _, _) =>
            (
              ExposureTimeMode.timeAndCount.getOption(red.value.exposureTimeMode),
              ExposureTimeMode.timeAndCount.getOption(blue.value.exposureTimeMode)
            )
            .tupled
            .toRight(Error.MisconfiguredObservation(obsParams.observationId, "GHOST requires TimeAndCount exposure time modes"))
            .map: (redEtm, blueEtm) =>
              val sciMode = InstrumentMode.GhostSpectroscopy(
                stepCnt,
                resolutionMode,
                ItcGhostDetector(redEtm, red.value.readMode, red.value.binning),
                ItcGhostDetector(blueEtm, blue.value.readMode, blue.value.binning)
              )

              scienceOnlySpectroscopyGeneratorParams(gh, sciMode)

          case gn @ gmos.longslit.Config.GmosNorth(grating = g, filter = f, fpu = u, common = c, acquisition = a) =>
            val sciMode = InstrumentMode.GmosNorthSpectroscopy(
              c.exposureTimeMode,
              c.centralWavelength,
              g,
              f,
              GmosFpu.North.builtin(u),
              gn.ccdMode.some,
              gn.roi.some
            )
            spectroscopyGeneratorParams(
              obsMode = gn,
              acqMode = InstrumentMode.GmosNorthImaging(
                exposureTimeMode = a.exposureTimeMode,
                filter  = a.explicitFilter.getOrElse(a.defaultFilter),
                ccdMode = sciMode.ccdMode
              ),
              sciMode  = sciMode
            ).asRight

          case gs @ gmos.longslit.Config.GmosSouth(grating = g, filter = f, fpu = u, common = c, acquisition = a) =>
            val sciMode = InstrumentMode.GmosSouthSpectroscopy(
              c.exposureTimeMode,
              c.centralWavelength,
              g,
              f,
              GmosFpu.South.builtin(u),
              gs.ccdMode.some,
              gs.roi.some
            )
            spectroscopyGeneratorParams(
              obsMode = gs,
              acqMode = InstrumentMode.GmosSouthImaging(
                exposureTimeMode = a.exposureTimeMode,
                filter  = a.explicitFilter.getOrElse(a.defaultFilter),
                ccdMode = sciMode.ccdMode
              ),
              sciMode  = sciMode
            ).asRight

          case gnm @ gmos.mos.Config.GmosNorth(grating = g, filter = f, customMask = m, common = c) =>
            val sciMode = InstrumentMode.GmosNorthSpectroscopy(
              c.exposureTimeMode,
              c.centralWavelength,
              g,
              f,
              GmosFpu.North.customMask(GmosCustomMask(m.slitWidth)),
              gnm.ccdMode.some,
              gnm.roi.some
            )

            scienceOnlySpectroscopyGeneratorParams(gnm, sciMode).asRight

          case gsm @ gmos.mos.Config.GmosSouth(grating = g, filter = f, customMask = m, common = c) =>
            val sciMode = InstrumentMode.GmosSouthSpectroscopy(
              c.exposureTimeMode,
              c.centralWavelength,
              g,
              f,
              GmosFpu.South.customMask(GmosCustomMask(m.slitWidth)),
              gsm.ccdMode.some,
              gsm.roi.some
            )

            scienceOnlySpectroscopyGeneratorParams(gsm, sciMode).asRight

          case gni @ gmos.ifu.Config.GmosNorth(grating = g, filter = f, acquisition = a, common = c) =>
            val sciMode = InstrumentMode.GmosNorthSpectroscopy(
              c.exposureTimeMode,
              c.centralWavelength,
              g,
              f,
              GmosFpu.North.builtin(gni.builtinFpu),
              gni.ccdMode.some,
              gni.roi.some,
              ifuAnalysis = gni.ifuAnalysis.some
            )
            spectroscopyGeneratorParams(
              obsMode = gni,
              acqMode = InstrumentMode.GmosNorthImaging(
                exposureTimeMode = a.exposureTimeMode,
                filter  = a.filter,
                ccdMode = sciMode.ccdMode
              ),
              sciMode  = sciMode
            ).asRight

          case gsi @ gmos.ifu.Config.GmosSouth(grating = g, filter = f, acquisition = a, common = c) =>
            val sciMode = InstrumentMode.GmosSouthSpectroscopy(
              c.exposureTimeMode,
              c.centralWavelength,
              g,
              f,
              GmosFpu.South.builtin(gsi.builtinFpu),
              gsi.ccdMode.some,
              gsi.roi.some,
              ifuAnalysis = gsi.ifuAnalysis.some
            )
            spectroscopyGeneratorParams(
              obsMode = gsi,
              acqMode = InstrumentMode.GmosSouthImaging(
                exposureTimeMode = a.exposureTimeMode,
                filter  = a.filter,
                ccdMode = sciMode.ccdMode
              ),
              sciMode  = sciMode
            ).asRight

          case gn @ gmos.imaging.Config.GmosNorth(_, fs, _) =>
            // An input per filter.
            val inputs = fs.map: f =>
              ImagingParameters(
                obsParams.constraints.toInput,
                InstrumentMode.GmosNorthImaging(f.exposureTimeMode, f.filter, gn.ccdMode.some)
              )

            val itcInput =
              obsParams
                .targets
                .traverse(itcTargetParams)
                .map(ItcInput.Imaging(inputs, _, obsParams.signalToNoiseTargetId))
                .leftMap(MissingParamSet.fromParams)
                .toEither

            GeneratorParams(ItcInputDerivation.fromEither(itcInput), obsParams.scienceBand, gn, obsParams.calibrationRole, obsParams.declaredState, obsParams.executionState, obsParams.stepCount, obsParams.schedulingMode.isSplittable).asRight

          case gs @ gmos.imaging.Config.GmosSouth(_, fs, _) =>
            // An input per filter.
            val inputs = fs.map: f =>
              ImagingParameters(
                obsParams.constraints.toInput,
                InstrumentMode.GmosSouthImaging(f.exposureTimeMode, f.filter, gs.ccdMode.some)
              )

            val itcInput =
              obsParams
                .targets
                .traverse(itcTargetParams)
                .map(ItcInput.Imaging(inputs, _, obsParams.signalToNoiseTargetId))
                .leftMap(MissingParamSet.fromParams)
                .toEither

            GeneratorParams(ItcInputDerivation.fromEither(itcInput), obsParams.scienceBand, gs, obsParams.calibrationRole, obsParams.declaredState, obsParams.executionState, obsParams.stepCount, obsParams.schedulingMode.isSplittable).asRight

          case gn: gnirs.spectroscopy.Config =>
            // Acquisition (imaging) filter for the ITC: the explicit acquisition
            // filter if set, otherwise the default for the spectroscopy wavelength.
            // This must use the same wavelength as Acquisition.scala's filter
            // selection, or the acquisition would be sized for a different filter
            // than the sequence actually uses.
            val acqFilter =
              gn.acquisition.explicitFilter
                .getOrElse(GnirsFilter.fromAcquisitionWavelength(gn.primaryCentralWavelength))

            // One science mode per central wavelength: each is a separate
            // configuration and so a separate ITC calculation.
            val sciModes = gn.wavelengths.map: w =>
              val sciReadMode = w.exposureTimeMode match
                                  case ExposureTimeMode.SignalToNoiseMode(_, _) =>
                                    GnirsReadMode.Bright // In practice this will be ignored by the ITC, which derives the read mode itself in S/N mode
                                  case ExposureTimeMode.TimeAndCountMode(time = time) =>
                                    gn.explicitReadMode.getOrElse(GnirsReadMode.forExposureTime(time))

              InstrumentMode.GnirsSpectroscopy(
                exposureTimeMode  = w.exposureTimeMode,
                centralWavelength = w.centralWavelength,
                filter            = gn.filter,
                fpu               = gn.fpu,
                prism             = gn.prism,
                grating           = gn.grating,
                camera            = gn.camera,
                readMode          = sciReadMode,
                wellDepth         = gn.wellDepth,
                coadds            = w.coadds
              )

            // Two-pass acquisition ITC whenever the acquisition mode and filter are
            // both auto: only then does the resolved filter depend on the ITC-derived
            // brightness classification (Very Bright → H2), creating the circularity.
            // This holds for both S/N and time-and-count acquisition ETMs — the
            // classification must not depend on the user's acquisition ETM.
            val acqAutoClassify: Boolean =
              gn.acquisition.explicitAcqMode.isEmpty &&
              gn.acquisition.explicitFilter.isEmpty

            // The acquisition S/N is derived from the same classification, which also
            // requires the classification pass even when the filter is explicit.
            val acqAutoSignalToNoise: Boolean =
              gn.acquisition.autoSignalToNoise

            gnirsSpectroscopyGeneratorParams(
              obsMode = gn,
              acqMode = InstrumentMode.GnirsImaging(
                exposureTimeMode = gn.acquisition.itcExposureTimeMode,
                filter           = acqFilter,
                camera           = gn.acquisitionCamera,
                readMode         = GnirsReadMode.Bright,
                wellDepth        = gn.wellDepth,
                coadds           = gn.acquisition.coadds
              ),
              sciModes = sciModes,
              gnirsAcqAutoClassify = acqAutoClassify,
              gnirsAcqAutoSignalToNoise = acqAutoSignalToNoise
            ).asRight

          case ig: igrins2.longslit.Config =>
            val sciMode = InstrumentMode.Igrins2Spectroscopy(ig.scienceExposureTimeMode)

            scienceOnlySpectroscopyGeneratorParams(ig, sciMode).asRight

          // Visitor Modes
          case vis: visitor.Config =>
            GeneratorParams(
              ItcInputDerivation.NotApplicable,
              obsParams.scienceBand,
              vis,
              obsParams.calibrationRole,
              obsParams.declaredState,
              obsParams.executionState,
              obsParams.stepCount,
              obsParams.schedulingMode.isSplittable
            ).asRight


      private def itcTargetParams(targetParams: TargetParams): ValidatedNel[MissingParam, ItcInput.TargetDefinition] = {
        // If emission line, SED not required, otherwhise must be defined
        def hasITCRequiredSEDParam(sp: SourceProfile): Boolean =
          SourceProfile.unnormalizedSED.getOption(sp).flatten.isDefined ||
          SourceProfile.integratedEmissionLinesSpectralDefinition.getOption(sp).isDefined ||
          SourceProfile.surfaceEmissionLinesSpectralDefinition.getOption(sp).isDefined

        val sourceProf   = targetParams.sourceProfile.map(_.gaiaFree)
        val brightnesses =
          sourceProf.flatMap: sp =>
            SourceProfile.integratedBrightnesses.getOption(sp).orElse(SourceProfile.surfaceBrightnesses.getOption(sp))
              .map(_.nonEmpty)
        val wavelengthLines =
          sourceProf.flatMap: sp =>
            SourceProfile.integratedWavelengthLines.getOption(sp).orElse(SourceProfile.surfaceWavelengthLines.getOption(sp))
              .map(_.nonEmpty)
        val validBrightness = brightnesses.orElse(wavelengthLines).getOrElse(false)
        val sed = sourceProf.filter(hasITCRequiredSEDParam)
        val validCustomSed = sourceProf.flatMap(customSedIdOptional.getOption).isEmpty || targetParams.customSedTimestamp.isDefined

        targetParams.targetId.toValidNel(MissingParam.forObservation("target")).andThen: tid =>
          (sourceProf.toValidNel(MissingParam.forTarget(tid, "source profile")),
           sed.toValidNel(MissingParam.forTarget(tid, "SED")),
           Validated.condNel(validBrightness, (), MissingParam.forTarget(tid, "brightness measure")),
           Validated.condNel(validCustomSed, (), MissingParam.forTarget(tid, "custom SED attachment"))
          ).mapN: (sp,_, _, _) =>
            ItcInput.TargetDefinition(
              tid,
              TargetInput(sp, targetParams.radialVelocity.getOrElse(RadialVelocity.Zero)),
              targetParams.customSedTimestamp
            )
      }

    }

  case class ParamsRow(
    observationId:         Observation.Id,
    calibrationRole:       Option[CalibrationRole],
    constraints:           ConstraintSet,
    exposureTimeMode:      Option[ExposureTimeMode],
    observingMode:         Option[ObservingModeType],
    scienceBand:           Option[ScienceBand],
    blindTargetId:         Option[Target.Id],
    blindRadialVelocity:   Option[RadialVelocity],
    blindSourceProfile:    Option[SourceProfile],
    targetId:              Option[Target.Id],
    radialVelocity:        Option[RadialVelocity],
    sourceProfile:         Option[SourceProfile],
    isSignalToNoiseTarget: Boolean,
    declaredState:         Option[DeclaredExecutionState],
    executionState:        ExecutionState,
    stepCount:             Long,
    schedulingMode:        SchedulingMode,
    customSedTimestamp:    Option[Timestamp] = none
  )

  case class TargetParams(
    targetId:           Option[Target.Id],
    radialVelocity:     Option[RadialVelocity],
    sourceProfile:      Option[SourceProfile],
    customSedTimestamp: Option[Timestamp],
  )

  case class ObsParams(
    observationId:         Observation.Id,
    calibrationRole:       Option[CalibrationRole],
    constraints:           ConstraintSet,
    exposureTimeMode:      Option[ExposureTimeMode],
    observingMode:         Option[ObservingModeType],
    scienceBand:           Option[ScienceBand],
    blindOffset:           Option[TargetParams],
    targets:               NonEmptyList[TargetParams],
    signalToNoiseTargetId: Option[Target.Id],
    declaredState:         Option[DeclaredExecutionState],
    executionState:        ExecutionState,
    stepCount:             Long,
    schedulingMode:        SchedulingMode
  )

  object ObsParams {
    def fromParamsRows(ps: NonEmptyList[ParamsRow]): Map[Observation.Id, ObsParams] =
      ps.groupBy(_.observationId).view.mapValues: oParams =>
        ObsParams(
          oParams.head.observationId,
          oParams.head.calibrationRole,
          oParams.head.constraints,
          oParams.head.exposureTimeMode,
          oParams.head.observingMode,
          oParams.head.scienceBand,
          oParams.head.blindTargetId.map(btid => TargetParams(btid.some, oParams.head.blindRadialVelocity, oParams.head.blindSourceProfile, None)),
          oParams.map: r =>
            TargetParams(r.targetId, r.radialVelocity, r.sourceProfile, r.customSedTimestamp),
          oParams.collectFirst { case r if r.isSignalToNoiseTarget => r.targetId }.flatten,
          oParams.head.declaredState,
          oParams.head.executionState,
          oParams.head.stepCount,
          oParams.head.schedulingMode
        )
      .toMap
  }



  object Statements {

    def selectExecutionStates(which: NonEmptyList[Observation.Id]): AppliedFragment =
      void"""
        SELECT
          c_observation_id,
          c_execution_state
        FROM v_generator_params
        WHERE c_observation_id IN (""" |+|
          which.map(sql"$observation_id").intercalate(void", ") |+|
        void")"

    import ProgramUserService.Statements.existsUserReadAccess

    private val source_profile: Decoder[SourceProfile] =
      jsonb.emap { sp =>
        sp.as[SourceProfile].leftMap(f => s"Could not decode SourceProfile: ${f.message}")
      }

    val params: Decoder[ParamsRow] =
      (observation_id          *:
       calibration_role.opt    *:
       constraint_set          *:
       exposure_time_mode.opt  *:
       observing_mode_type.opt *:
       science_band.opt        *:
       target_id.opt           *:
       radial_velocity.opt     *:
       source_profile.opt      *:
       target_id.opt           *:
       radial_velocity.opt     *:
       source_profile.opt      *:
       bool                    *:
       declared_execution_state.opt *:
       execution_state         *:
       int8                    *:
       scheduling_mode
      ).map( (oid, role, cs, etm, om, sb, btid, brv, bsp, tid, rv, sp, snt, dc, es, sc, req) =>
        ParamsRow(oid, role, cs, etm, om, sb, btid, brv, bsp, tid, rv, sp, snt, dc, es, sc, req, None))

    private def ParamColumns(tab: String): String =
      s"""
        $tab.c_observation_id,
        $tab.c_calibration_role,
        $tab.c_image_quality,
        $tab.c_cloud_extinction,
        $tab.c_sky_background,
        $tab.c_water_vapor,
        $tab.c_air_mass_min,
        $tab.c_air_mass_max,
        $tab.c_hour_angle_min,
        $tab.c_hour_angle_max,
        $tab.c_exposure_time_mode,
        $tab.c_signal_to_noise_at,
        $tab.c_signal_to_noise,
        $tab.c_exposure_time,
        $tab.c_exposure_count,
        $tab.c_observing_mode_type,
        $tab.c_science_band,
        $tab.c_blind_offset_target_id,
        $tab.c_blind_rv,
        $tab.c_blind_source_profile,
        $tab.c_target_id,
        $tab.c_sid_rv,
        $tab.c_source_profile,
        $tab.c_is_signal_to_noise_target,
        $tab.c_declared_state,
        $tab.c_execution_state,
        $tab.c_step_count,
        $tab.c_scheduling_mode
      """

    def selectManyParams(
      user:      User,
      programId: Program.Id,
      which:     NonEmptyList[Observation.Id]
    ): AppliedFragment =
      sql"""
        SELECT
          #${ParamColumns("gp")}
        FROM v_generator_params gp
        WHERE
      """(Void) |+|
        sql"""gp.c_program_id = $program_id""".apply(programId) |+|
        void""" AND gp.c_observation_id IN (""" |+|
          which.map(sql"$observation_id").intercalate(void", ") |+|
        void")" |+|
        existsUserReadAccess(user, programId).fold(AppliedFragment.empty) { af => void""" AND """ |+| af }

    def selectManyParams(
      which: NonEmptyList[Observation.Id]
    ): AppliedFragment =
      sql"""
        SELECT
          #${ParamColumns("gp")}
        FROM v_generator_params gp
        WHERE
      """(Void) |+|
        void"""gp.c_observation_id IN (""" |+|
          which.map(sql"$observation_id").intercalate(void", ") |+|
        void")"

    def selectAllParams(
      user:      User,
      programId: Program.Id,
      // minStatus: ObsStatus,
      selection: ObservationSelection
    ): AppliedFragment = {
      val selector = selection match
        case ObservationSelection.All         => void""
        case ObservationSelection.Science     => void" AND ob.c_calibration_role is null "
        case ObservationSelection.Calibration => void" AND ob.c_calibration_role is not null "

      sql"""
        SELECT
          #${ParamColumns("gp")}
        FROM v_generator_params gp
        INNER JOIN t_observation ob ON gp.c_observation_id = ob.c_observation_id
        WHERE
      """(Void) |+|
        sql"""gp.c_program_id = $program_id""".apply(programId)              |+|
        void""" AND ob.c_existence = 'present' """                           |+|
        void""" AND ob.c_workflow_user_state is distinct from 'inactive' """ |+|
        selector                                                             |+|
        existsUserReadAccess(user, programId).fold(AppliedFragment.empty) { af => void""" AND """ |+| af }
    }
  }

}
