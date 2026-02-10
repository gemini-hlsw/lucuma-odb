// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.data.EitherT
import cats.data.OptionT
import cats.effect.Async
import cats.syntax.either.*
import cats.syntax.option.*
import lucuma.core.model.Observation
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig as Flamingos2Dynamic
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig as Flamingos2Static
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth as GmosNorthDynamic
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth as GmosSouthDynamic
import lucuma.core.model.sequence.gmos.StaticConfig.GmosNorth as GmosNorthStatic
import lucuma.core.model.sequence.gmos.StaticConfig.GmosSouth as GmosSouthStatic
import lucuma.odb.data.Itc
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

  // TODO: can we remove the effect?  I don't think we need it:
  //  Either[OdbError, StreamingExecutionConfig[Pure, Flamingos2Static, Flamingos2Dynamic]]
  def generateFlamingos2LongSlit(
    context: GeneratorContext
  ): F[Either[OdbError, StreamingExecutionConfig[F, Flamingos2Static, Flamingos2Dynamic]]]


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


object GeneratorStreaming:

  def requireImagingItc[A](
    name: String,
    oid:  Observation.Id,
    itc:  Either[OdbError, Itc],
    img:  Itc => Option[A]
  ): Either[OdbError, A] =
    itc.flatMap: i =>
      img(i).toRight:
        OdbError.InvalidObservation(oid, s"Expecting $name ITC results for this observation".some)

  def requireSpectroscopyItc(
    oid: Observation.Id,
    itc: Either[OdbError, Itc]
  ): Either[OdbError, Itc.Spectroscopy] =
    itc.flatMap: i =>
      Itc.spectroscopy.getOption(i).toRight:
        OdbError.InvalidObservation(oid, s"Expecting a spectroscopy ITC result for this observation".some)

  def instantiate[F[_]: Async: Services](
    commitHash: CommitHash,
    calculator: TimeEstimateCalculatorImplementation.ForInstrumentMode
  ): GeneratorStreaming[F] =

    extension (context: GeneratorContext)
      def namespace: UUID =
        SequenceIds.namespace(commitHash, context.oid, context.params)

    new GeneratorStreaming[F]:

      private val exp = SmartGcalImplementation.fromService(smartGcalService)

      private def select[S, D](
        lookup: (Transaction[F]) ?=> F[Option[StreamingExecutionConfig[F, S, D]]]
      )(using Transaction[F]): OptionT[F, Either[OdbError, StreamingExecutionConfig[F, S, D]]] =
        OptionT(lookup).map(_.asRight[OdbError])

      private def extractMode[A](
        expected: String,
        context:  GeneratorContext
      )(f: ObservingMode => Option[A]): EitherT[F, OdbError, A] =
        val mode = context.params.observingMode
        EitherT.fromOption[F](f(mode), GeneratorError.unexpectedMode(context.oid, expected, mode))

      override def selectOrGenerateFlamingos2LongSlit(
        context: GeneratorContext
      )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, Flamingos2Static, Flamingos2Dynamic]]] =
        select(sequenceService.selectFlamingos2ExecutionConfig(context.oid))
          .getOrElseF(generateFlamingos2LongSlit(context))

      override def generateFlamingos2LongSlit(
        context: GeneratorContext
      ): F[Either[OdbError, StreamingExecutionConfig[F, Flamingos2Static, Flamingos2Dynamic]]] =
        import lucuma.odb.sequence.flamingos2.longslit.LongSlit
        (for
          cfg <- extractMode(ObservingMode.Flamingos2LongSlitName, context)(_.asFlamingos2LongSlit)
          itc  = requireSpectroscopyItc(context.oid, context.itcRes)
          gen <- EitherT(LongSlit.instantiate(context.oid, calculator.flamingos2, context.namespace, exp.flamingos2, cfg, itc, context.params.acqResetTime))
        yield gen.streamingExecutionConfig[F]).value


      override def selectOrGenerateGmosNorthImaging(
        context: GeneratorContext
      )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]]] =
        select(sequenceService.selectGmosNorthExecutionConfig(context.oid))
          .getOrElseF(generateGmosNorthImaging(context))

      override def generateGmosNorthImaging(
        context: GeneratorContext
      ): F[Either[OdbError, StreamingExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]]] =
        import lucuma.odb.sequence.gmos.imaging.Imaging
        (for
          cfg <- extractMode(ObservingMode.GmosNorthImagingName, context)(_.asGmosNorthImaging)
          itc  = requireImagingItc(ObservingMode.GmosNorthImagingName, context.oid, context.itcRes, Itc.gmosNorthImaging.getOption)
          gen <- EitherT(Imaging.gmosNorth(calculator.gmosNorth, context.namespace, cfg, itc))
        yield gen.streamingExecutionConfig[F]).value


      override def selectOrGenerateGmosNorthLongSlit(
        context: GeneratorContext
      )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]]] =
        select(sequenceService.selectGmosNorthExecutionConfig(context.oid))
          .getOrElseF(generateGmosNorthLongSlit(context))

      override def generateGmosNorthLongSlit(
        context: GeneratorContext
      ): F[Either[OdbError, StreamingExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]]] =
        import lucuma.odb.sequence.gmos.longslit.LongSlit
        (for
          cfg <- extractMode(ObservingMode.GmosNorthLongSlitName, context)(_.asGmosNorthLongSlit)
          itc  = requireSpectroscopyItc(context.oid, context.itcRes)
          rol  = context.params.calibrationRole
          rst  = context.params.acqResetTime
          gen <- EitherT(LongSlit.gmosNorth(context.oid, calculator.gmosNorth, context.namespace, exp.gmosNorth, cfg, itc, rol, rst))
        yield gen.streamingExecutionConfig[F]).value


      override def selectOrGenerateGmosSouthImaging(
        context: GeneratorContext
      )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]]] =
        select(sequenceService.selectGmosSouthExecutionConfig(context.oid))
          .getOrElseF(generateGmosSouthImaging(context))

      override def generateGmosSouthImaging(
        context: GeneratorContext
      ): F[Either[OdbError, StreamingExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]]] =
        import lucuma.odb.sequence.gmos.imaging.Imaging
        (for
          cfg <- extractMode(ObservingMode.GmosSouthImagingName, context)(_.asGmosSouthImaging)
          itc  = requireImagingItc(ObservingMode.GmosSouthImagingName, context.oid, context.itcRes, Itc.gmosSouthImaging.getOption)
          gen <- EitherT(Imaging.gmosSouth(calculator.gmosSouth, context.namespace, cfg, itc))
        yield gen.streamingExecutionConfig[F]).value


      override def selectOrGenerateGmosSouthLongSlit(
        context: GeneratorContext
      )(using Transaction[F]): F[Either[OdbError, StreamingExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]]] =
        select(sequenceService.selectGmosSouthExecutionConfig(context.oid))
          .getOrElseF(generateGmosSouthLongSlit(context))

      override def generateGmosSouthLongSlit(
        context: GeneratorContext
      ): F[Either[OdbError, StreamingExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]]] =
        import lucuma.odb.sequence.gmos.longslit.LongSlit
        (for
          cfg <- extractMode(ObservingMode.GmosSouthLongSlitName, context)(_.asGmosSouthLongSlit)
          itc  = requireSpectroscopyItc(context.oid, context.itcRes)
          rol  = context.params.calibrationRole
          rst  = context.params.acqResetTime
          gen <- EitherT(LongSlit.gmosSouth(context.oid, calculator.gmosSouth, context.namespace, exp.gmosSouth, cfg, itc, rol, rst))
        yield gen.streamingExecutionConfig[F]).value