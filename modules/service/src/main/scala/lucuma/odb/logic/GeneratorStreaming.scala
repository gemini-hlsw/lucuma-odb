// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.data.EitherT
import cats.effect.Async
import cats.syntax.applicative.*
import cats.syntax.either.*
import cats.syntax.flatMap.*
import cats.syntax.functor.*
import cats.syntax.option.*
import fs2.Pure
import fs2.Stream
import lucuma.core.enums.GnirsAcquisitionType
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig as Flamingos2Dynamic
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig as Flamingos2Static
import lucuma.core.model.sequence.ghost.GhostDynamicConfig as GhostDynamic
import lucuma.core.model.sequence.ghost.GhostStaticConfig as GhostStatic
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth as GmosNorthDynamic
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth as GmosSouthDynamic
import lucuma.core.model.sequence.gmos.StaticConfig.GmosNorth as GmosNorthStatic
import lucuma.core.model.sequence.gmos.StaticConfig.GmosSouth as GmosSouthStatic
import lucuma.core.model.sequence.gnirs.GnirsDynamicConfig as GnirsDynamic
import lucuma.core.model.sequence.gnirs.GnirsStaticConfig as GnirsStatic
import lucuma.core.model.sequence.igrins2.Igrins2DynamicConfig as Igrins2Dynamic
import lucuma.core.model.sequence.igrins2.Igrins2StaticConfig as Igrins2Static
import lucuma.itc.IntegrationTime
import lucuma.odb.data.Itc
import lucuma.odb.data.ItcAcquisition
import lucuma.odb.data.ItcScience
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.ObservingMode.Syntax.*
import lucuma.odb.sequence.data.StreamingExecutionConfig
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.sequence.util.SequenceIds
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import skunk.Transaction

import java.util.UUID

/**
 * GeneratorStreaming produces StreamingExecutionConfig values for each of the
 * observing modes.  Two varieties are offered for each mode.  The 'generate'
 * method (e.g., `generateFlamingos2LongSlit`) produces a sequence from scratch
 * ignoring any partial or previous execution of the sequence.  The
 * 'selectOrGenerate' method will read the unexecuted portion of the sequence
 * from the database if present, or else generate the sequence from scratch.
 */
sealed trait GeneratorStreaming[F[_]]:

  def selectOrGenerateFlamingos2LongSlit(
    context: GeneratorContext
  )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, Flamingos2Static, Flamingos2Dynamic]]]

  def generateFlamingos2LongSlit(
    context: GeneratorContext
  ): F[Either[OdbError, StreamingExecutionConfig[F, Flamingos2Static, Flamingos2Dynamic]]]

  def selectOrGenerateFlamingos2Imaging(
    context: GeneratorContext
  )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, Flamingos2Static, Flamingos2Dynamic]]]

  def generateFlamingos2Imaging(
    context: GeneratorContext
  ): F[Either[OdbError, StreamingExecutionConfig[F, Flamingos2Static, Flamingos2Dynamic]]]

  def selectOrGenerateGhost(
    context: GeneratorContext
  )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, GhostStatic, GhostDynamic]]]

  def generateGhost(
    context: GeneratorContext
  ): F[Either[OdbError, StreamingExecutionConfig[F, GhostStatic, GhostDynamic]]]

  def selectOrGenerateGmosNorthImaging(
    context: GeneratorContext
  )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]]]

  def generateGmosNorthImaging(
    context: GeneratorContext
  ): F[Either[OdbError, StreamingExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]]]


  def selectOrGenerateGmosNorthLongSlit(
    context: GeneratorContext
  )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]]]

  def generateGmosNorthLongSlit(
    context: GeneratorContext
  ): F[Either[OdbError, StreamingExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]]]


  def selectOrGenerateGmosSouthImaging(
    context: GeneratorContext
  )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]]]

  def generateGmosSouthImaging(
    context: GeneratorContext
  ): F[Either[OdbError, StreamingExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]]]


  def selectOrGenerateGmosSouthLongSlit(
    context: GeneratorContext
  )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]]]

  def generateGmosSouthLongSlit(
    context: GeneratorContext
  ): F[Either[OdbError, StreamingExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]]]

  def selectOrGenerateIgrins2LongSlit(
    context: GeneratorContext
  )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, Igrins2Static, Igrins2Dynamic]]]

  def generateIgrins2LongSlit(
    context: GeneratorContext
  ): F[Either[OdbError, StreamingExecutionConfig[F, Igrins2Static, Igrins2Dynamic]]]

  def selectOrGenerateGnirsSpectroscopy(
    context: GeneratorContext
  )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, GnirsStatic, GnirsDynamic]]]

  def generateGnirsSpectroscopy(
    context: GeneratorContext
  ): F[Either[OdbError, StreamingExecutionConfig[F, GnirsStatic, GnirsDynamic]]]

  def selectOrGenerateGnirsImaging(
    context: GeneratorContext
  )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, GnirsStatic, GnirsDynamic]]]

  def generateGnirsImaging(
    context: GeneratorContext
  ): F[Either[OdbError, StreamingExecutionConfig[F, GnirsStatic, GnirsDynamic]]]

object GeneratorStreaming:

  def requireGhostItc(
    oid: Observation.Id,
    itc: Either[OdbError, Itc]
  ): Either[OdbError, ItcScience.GhostIfu] =
    itc.flatMap: i =>
      ItcScience.ghostIfu.getOption(i.science).toRight:
        OdbError.InvalidObservation(oid, s"Expecting a GHOST IFU result for this observation".some)

  def requireImagingItc[A](
    name: String,
    oid:  Observation.Id,
    itc:  Either[OdbError, Itc],
    img:  ItcScience => Option[A]
  ): Either[OdbError, A] =
    itc.flatMap: i =>
      img(i.science).toRight:
        OdbError.InvalidObservation(oid, s"Expecting $name ITC results for this observation".some)

  // Science integration time for a spectroscopy observation.  The whole ITC
  // result must be present and its science part must be spectroscopy.
  def spectroscopyScienceTime(
    oid: Observation.Id,
    itc: Either[OdbError, Itc]
  ): Either[OdbError, IntegrationTime] =
    itc.flatMap: i =>
      ItcScience.spectroscopy.getOption(i.science)
        .toRight(OdbError.InvalidObservation(oid, s"Expecting a spectroscopy ITC result for this observation".some))
        .map(_.science.focus.value)

  // Acquisition integration time for a mode that has an acquisition sequence.  A
  // Failed acquisition (a cached deterministic ITC failure) or a NotApplicable
  // one (no acquisition sequence) is surfaced as a Left, confining the failure
  // to the acquisition sub-sequence.
  def acquisitionTime(
    oid: Observation.Id,
    itc: Either[OdbError, Itc]
  ): Either[OdbError, IntegrationTime] =
    itc.flatMap:
      _.acquisition match
        case ItcAcquisition.Available(times, _) => times.focus.value.asRight
        case ItcAcquisition.Failed(msg)         => OdbError.ItcError(msg.some).asLeft
        case ItcAcquisition.NotApplicable       =>
          OdbError.InvalidObservation(oid, s"Expecting an acquisition ITC result for this observation".some).asLeft

  // The pinned GNIRS acquisition type, when the acquisition result carries one.
  def gnirsAcqType(
    itc: Either[OdbError, Itc]
  ): Option[GnirsAcquisitionType] =
    itc.toOption.flatMap:
      _.acquisition match
        case ItcAcquisition.Available(_, t) => t
        case _                              => none

  def instantiate[F[_]: Async: Services](
    commitHash: CommitHash,
    calculator: TimeEstimateCalculatorImplementation.ForInstrumentMode
  ): GeneratorStreaming[F] =

    extension (context: GeneratorContext)
      def namespace: UUID =
        SequenceIds.namespace(commitHash, context.oid, context.params)

    new GeneratorStreaming[F]:

      private val exp = SmartGcalImplementation.fromService(smartGcalService)

      private def extractMode[A](
        expected: String,
        context:  GeneratorContext
      )(f: ObservingMode => Option[A]): EitherT[F, OdbError, A] =
        val mode = context.params.observingMode
        EitherT.fromOption[F](f(mode), GeneratorError.unexpectedMode(context.oid, expected, mode))

      private def selectOrGenerate[S, D](
        static:   S,
        lookup:   (SequenceType, S) => (Transaction[F]) ?=> F[Option[Stream[F, Atom[D]]]],
        generate: F[Either[OdbError, StreamingExecutionConfig[F, S, D]]]
      )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, S, D]]] =
        def result(
          acquisition: Option[Stream[F, Atom[D]]],
          science:     Option[Stream[F, Atom[D]]]
        ): F[Either[OdbError, StreamingExecutionConfig[F, S, D]]] =
          (acquisition, science) match
            case (None,    None   ) => generate
            case (Some(a), None   ) => generate.map(_.map(_.copy(acquisition = a)))
            case (None,    Some(s)) => generate.map(_.map(_.copy(science     = s)))
            case (Some(a), Some(s)) => StreamingExecutionConfig(static, a, s).asRight[OdbError].pure[F]

        for
          a <- lookup(SequenceType.Acquisition, static)
          s <- lookup(SequenceType.Science,     static)
          r <- result(a, s)
        yield r

      private def collapseIfNecessary[S, D](
        context: GeneratorContext,
        gen:     StreamingExecutionConfig[Pure, S, D]
      ): EitherT[F, OdbError, StreamingExecutionConfig[F, S, D]] =
        EitherT.fromEither:
          (
            if context.params.isSplittable then gen.asRight
            else gen.unsplit(context.oid)
          ).map(_.covary[F])

      override def selectOrGenerateFlamingos2LongSlit(
        context: GeneratorContext
      )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, Flamingos2Static, Flamingos2Dynamic]]] =
        selectOrGenerate(
          lucuma.odb.sequence.flamingos2.longslit.LongSlit.Static,
          sequenceService.selectFlamingos2Sequence(context.oid, _, _),
          generateFlamingos2LongSlit(context)
        )

      override def generateFlamingos2LongSlit(
        context: GeneratorContext
      ): F[Either[OdbError, StreamingExecutionConfig[F, Flamingos2Static, Flamingos2Dynamic]]] =
        import lucuma.odb.sequence.flamingos2.longslit.LongSlit
        (for
          cfg <- extractMode(ObservingMode.Flamingos2LongSlitName, context)(_.asFlamingos2LongSlit)
          acq  = acquisitionTime(context.oid, context.itcRes)
          sci  = spectroscopyScienceTime(context.oid, context.itcRes)
          rol  = context.params.calibrationRole
          gen <- EitherT(LongSlit.instantiate(context.oid, calculator.flamingos2Step, context.namespace, exp.flamingos2, cfg, acq, sci, rol))
          res <- collapseIfNecessary(context, gen)
        yield res).value

      override def selectOrGenerateFlamingos2Imaging(
        context: GeneratorContext
      )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, Flamingos2Static, Flamingos2Dynamic]]] =
        (for
          cfg <- extractMode(ObservingMode.Flamingos2ImagingName, context)(_.asFlamingos2Imaging)
          res <- EitherT(selectOrGenerate(
            cfg.staticConfig,
            sequenceService.selectFlamingos2Sequence(context.oid, _, _),
            generateFlamingos2Imaging(context)
          ))
        yield res).value

      override def generateFlamingos2Imaging(
        context: GeneratorContext
      ): F[Either[OdbError, StreamingExecutionConfig[F, Flamingos2Static, Flamingos2Dynamic]]] =
        import lucuma.odb.sequence.flamingos2.imaging.Imaging
        (for
          cfg <- extractMode(ObservingMode.Flamingos2ImagingName, context)(_.asFlamingos2Imaging)
          itc  = requireImagingItc(ObservingMode.Flamingos2ImagingName, context.oid, context.itcRes, ItcScience.flamingos2Imaging.getOption)
          gen <- EitherT(Imaging.flamingos2(calculator.flamingos2Step, context.namespace, cfg, itc))
          res <- collapseIfNecessary(context, gen)
        yield res).value

      override def selectOrGenerateGhost(
        context: GeneratorContext
      )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, GhostStatic, GhostDynamic]]] =
        (for
          cfg <- extractMode(ObservingMode.GhostIfuName, context)(_.asGhostIfu)
          stc <- EitherT(sequenceService.selectOrComputeGhostStatic(context.oid))
          res <- EitherT:
                   selectOrGenerate(
                     stc,
                     sequenceService.selectGhostSequence(context.oid, _, _),
                     generateGhost(context)
                   )
        yield res).value

      override def generateGhost(
        context: GeneratorContext
      ): F[Either[OdbError, StreamingExecutionConfig[F, GhostStatic, GhostDynamic]]] =
        import lucuma.odb.sequence.ghost.ifu.Ifu
        (for
          cfg <- extractMode(ObservingMode.GhostIfuName, context)(_.asGhostIfu)
          stc <- EitherT(ghostIfuService.computeStatic(context.oid))
          itc  = requireGhostItc(context.oid, context.itcRes)
          gen <- EitherT(Ifu.instantiate(calculator.ghostStep, stc, context.namespace, exp.ghost, cfg, itc))
          res <- collapseIfNecessary(context, gen)
        yield res).value

      override def selectOrGenerateIgrins2LongSlit(
        context: GeneratorContext
      )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, Igrins2Static, Igrins2Dynamic]]] =
        import lucuma.odb.sequence.igrins2.longslit.LongSlit
        (for
          cfg <- extractMode(ObservingMode.Igrins2LongSlitName, context)(_.asIgrins2LongSlit)
          res <- EitherT(selectOrGenerate(
                   LongSlit.staticFrom(cfg),
                   sequenceService.selectIgrins2Sequence(context.oid, _, _),
                   generateIgrins2LongSlit(context)
                 ))
        yield res).value

      override def generateIgrins2LongSlit(
        context: GeneratorContext
      ): F[Either[OdbError, StreamingExecutionConfig[F, Igrins2Static, Igrins2Dynamic]]] =
        import lucuma.odb.sequence.igrins2.longslit.LongSlit
        (for
          cfg <- extractMode(ObservingMode.Igrins2LongSlitName, context)(_.asIgrins2LongSlit)
          sci  = spectroscopyScienceTime(context.oid, context.itcRes)
          rol  = context.params.calibrationRole
          gen <- EitherT.fromEither(LongSlit.instantiate(context.oid, calculator.igrins2Step, context.namespace, cfg, sci, rol))
          res <- collapseIfNecessary(context, gen)
        yield res).value

      override def selectOrGenerateGnirsSpectroscopy(
        context: GeneratorContext
      )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, GnirsStatic, GnirsDynamic]]] =
        import lucuma.odb.sequence.gnirs.spectroscopy.Spectroscopy
        (for
          cfg <- extractMode(ObservingMode.GnirsSpectroscopyName, context)(_.asGnirsSpectroscopy)
          res <- EitherT(selectOrGenerate(
                   Spectroscopy.staticFrom(cfg),
                   sequenceService.selectGnirsSequence(context.oid, _, _),
                   generateGnirsSpectroscopy(context)
                 ))
        yield res).value

      override def generateGnirsSpectroscopy(
        context: GeneratorContext
      ): F[Either[OdbError, StreamingExecutionConfig[F, GnirsStatic, GnirsDynamic]]] =
        import lucuma.odb.sequence.gnirs.spectroscopy.Spectroscopy
        (for
          cfg <- extractMode(ObservingMode.GnirsSpectroscopyName, context)(_.asGnirsSpectroscopy)
          acq  = acquisitionTime(context.oid, context.itcRes)
          typ  = gnirsAcqType(context.itcRes)
          sci  = spectroscopyScienceTime(context.oid, context.itcRes)
          rol  = context.params.calibrationRole
          gen <- EitherT(Spectroscopy.instantiate(context.oid, calculator.gnirsStep, context.namespace, exp.gnirs, cfg, acq, typ, sci, rol))
          res <- collapseIfNecessary(context, gen)
        yield res).value

      override def selectOrGenerateGnirsImaging(
        context: GeneratorContext
      )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, GnirsStatic, GnirsDynamic]]] =
        (for
          cfg <- extractMode(ObservingMode.GnirsImagingName, context)(_.asGnirsImaging)
          res <- EitherT(selectOrGenerate(
                   cfg.staticConfig,
                   sequenceService.selectGnirsSequence(context.oid, _, _),
                   generateGnirsImaging(context)
                 ))
        yield res).value

      override def generateGnirsImaging(
        context: GeneratorContext
      ): F[Either[OdbError, StreamingExecutionConfig[F, GnirsStatic, GnirsDynamic]]] =
        import lucuma.odb.sequence.gnirs.imaging.Imaging
        (for
          cfg <- extractMode(ObservingMode.GnirsImagingName, context)(_.asGnirsImaging)
          itc  = requireImagingItc(ObservingMode.GnirsImagingName, context.oid, context.itcRes, ItcScience.gnirsImaging.getOption)
          gen <- EitherT(Imaging.gnirs(calculator.gnirsStep, context.namespace, cfg, itc))
          res <- collapseIfNecessary(context, gen)
        yield res).value

      override def selectOrGenerateGmosNorthImaging(
        context: GeneratorContext
      )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]]] =
        (for
          cfg <- extractMode(ObservingMode.GmosNorthImagingName, context)(_.asGmosNorthImaging)
          res <- EitherT(selectOrGenerate(
            cfg.staticConfig,
            sequenceService.selectGmosNorthSequence(context.oid, _, _),
            generateGmosNorthImaging(context)
          ))
        yield res).value

      override def generateGmosNorthImaging(
        context: GeneratorContext
      ): F[Either[OdbError, StreamingExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]]] =
        import lucuma.odb.sequence.gmos.imaging.Imaging
        (for
          cfg <- extractMode(ObservingMode.GmosNorthImagingName, context)(_.asGmosNorthImaging)
          itc  = requireImagingItc(ObservingMode.GmosNorthImagingName, context.oid, context.itcRes, ItcScience.gmosNorthImaging.getOption)
          gen <- EitherT(Imaging.gmosNorth(calculator.gmosNorthStep, context.namespace, cfg, itc))
          res <- collapseIfNecessary(context, gen)
        yield res).value

      override def selectOrGenerateGmosNorthLongSlit(
        context: GeneratorContext
      )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]]] =
        selectOrGenerate(
          lucuma.odb.sequence.gmos.InitialConfigs.GmosNorthStatic,
          sequenceService.selectGmosNorthSequence(context.oid, _, _),
          generateGmosNorthLongSlit(context)
        )

      override def generateGmosNorthLongSlit(
        context: GeneratorContext
      ): F[Either[OdbError, StreamingExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]]] =
        import lucuma.odb.sequence.gmos.longslit.LongSlit
        (for
          cfg <- extractMode(ObservingMode.GmosNorthLongSlitName, context)(_.asGmosNorthLongSlit)
          acq  = acquisitionTime(context.oid, context.itcRes)
          sci  = spectroscopyScienceTime(context.oid, context.itcRes)
          rol  = context.params.calibrationRole
          gen <- EitherT(LongSlit.gmosNorth(context.oid, calculator.gmosNorthStep, context.namespace, exp.gmosNorth, cfg, acq, sci, rol))
          res <- collapseIfNecessary(context, gen)
        yield res).value


      override def selectOrGenerateGmosSouthImaging(
        context: GeneratorContext
      )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]]] =
        (for
          cfg <- extractMode(ObservingMode.GmosSouthImagingName, context)(_.asGmosSouthImaging)
          res <- EitherT(selectOrGenerate(
            cfg.staticConfig,
            sequenceService.selectGmosSouthSequence(context.oid, _, _),
            generateGmosSouthImaging(context)
          ))
        yield res).value

      override def generateGmosSouthImaging(
        context: GeneratorContext
      ): F[Either[OdbError, StreamingExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]]] =
        import lucuma.odb.sequence.gmos.imaging.Imaging
        (for
          cfg <- extractMode(ObservingMode.GmosSouthImagingName, context)(_.asGmosSouthImaging)
          itc  = requireImagingItc(ObservingMode.GmosSouthImagingName, context.oid, context.itcRes, ItcScience.gmosSouthImaging.getOption)
          gen <- EitherT(Imaging.gmosSouth(calculator.gmosSouthStep, context.namespace, cfg, itc))
          res <- collapseIfNecessary(context, gen)
        yield res).value


      override def selectOrGenerateGmosSouthLongSlit(
        context: GeneratorContext
      )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]]] =
        selectOrGenerate(
          lucuma.odb.sequence.gmos.InitialConfigs.GmosSouthStatic,
          sequenceService.selectGmosSouthSequence(context.oid, _, _),
          generateGmosSouthLongSlit(context)
        )

      override def generateGmosSouthLongSlit(
        context: GeneratorContext
      ): F[Either[OdbError, StreamingExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]]] =
        import lucuma.odb.sequence.gmos.longslit.LongSlit
        (for
          cfg <- extractMode(ObservingMode.GmosSouthLongSlitName, context)(_.asGmosSouthLongSlit)
          acq  = acquisitionTime(context.oid, context.itcRes)
          sci  = spectroscopyScienceTime(context.oid, context.itcRes)
          rol  = context.params.calibrationRole
          gen <- EitherT(LongSlit.gmosSouth(context.oid, calculator.gmosSouthStep, context.namespace, exp.gmosSouth, cfg, acq, sci, rol))
          res <- collapseIfNecessary(context, gen)
        yield res).value