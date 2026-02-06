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
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import skunk.Transaction

import java.util.UUID

/**
 * GeneratorStreaming produces StreamingExecutionConfig values for each of the
 * observing modes, hiding the detail of whether it is read from the database or
 * generated from observation configuration.
 */
sealed trait GeneratorStreaming[F[_]]:

  def flamingos2LongSlit(
    context: GeneratorContext
  )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, StreamingExecutionConfig[F, Flamingos2Static, Flamingos2Dynamic]]]

  def gmosNorthImaging(
    context: GeneratorContext
  )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, StreamingExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]]]

  def gmosNorthLongSlit(
    context: GeneratorContext
  )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, StreamingExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]]]

  def gmosSouthImaging(
    context: GeneratorContext
  )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, StreamingExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]]]

  def gmosSouthLongSlit(
    context: GeneratorContext
  )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, StreamingExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]]]

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
        lookup: (Transaction[F], Services.ServiceAccess) ?=> F[Option[StreamingExecutionConfig[F, S, D]]]
      )(using NoTransaction[F], Services.ServiceAccess): OptionT[F, Either[OdbError, StreamingExecutionConfig[F, S, D]]] =
        OptionT(services.transactionally(lookup)).map(_.asRight[OdbError])

      private def extractMode[A](
        expected: String,
        context:  GeneratorContext
      )(f: ObservingMode => Option[A]): EitherT[F, OdbError, A] =
        val mode = context.params.observingMode
        EitherT.fromOption[F](f(mode), GeneratorError.unexpectedMode(context.oid, expected, mode))

      override def flamingos2LongSlit(
        context: GeneratorContext
      )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, StreamingExecutionConfig[F, Flamingos2Static, Flamingos2Dynamic]]] =
        select(sequenceService.streamingFlamingos2ExecutionConfig(context.oid))
          .getOrElseF:
            import lucuma.odb.sequence.flamingos2.longslit.LongSlit
            (for
              cfg <- extractMode(ObservingMode.Flamingos2LongSlitName, context)(_.asFlamingos2LongSlit)
              itc  = requireSpectroscopyItc(context.oid, context.itcRes)
              gen <- EitherT(LongSlit.instantiate(context.oid, calculator.flamingos2, context.namespace, exp.flamingos2, cfg, itc, context.params.acqResetTime))
            yield gen.streamingExecutionConfig[F]).value

      override def gmosNorthImaging(
        context: GeneratorContext
      )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, StreamingExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]]] =
        select(sequenceService.streamingGmosNorthExecutionConfig(context.oid))
          .getOrElseF:
            import lucuma.odb.sequence.gmos.imaging.Imaging
            (for
              cfg <- extractMode(ObservingMode.GmosNorthImagingName, context)(_.asGmosNorthImaging)
              itc  = requireImagingItc(ObservingMode.GmosNorthImagingName, context.oid, context.itcRes, Itc.gmosNorthImaging.getOption)
              gen <- EitherT(Imaging.gmosNorth(calculator.gmosNorth, context.namespace, cfg, itc))
            yield gen.streamingExecutionConfig[F]).value

      override def gmosNorthLongSlit(
        context: GeneratorContext
      )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, StreamingExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]]] =
        select(sequenceService.streamingGmosNorthExecutionConfig(context.oid))
          .getOrElseF:
            import lucuma.odb.sequence.gmos.longslit.LongSlit
            (for
              cfg <- extractMode(ObservingMode.GmosNorthLongSlitName, context)(_.asGmosNorthLongSlit)
              itc  = requireSpectroscopyItc(context.oid, context.itcRes)
              rol  = context.params.calibrationRole
              rst  = context.params.acqResetTime
              gen <- EitherT(LongSlit.gmosNorth(context.oid, calculator.gmosNorth, context.namespace, exp.gmosNorth, cfg, itc, rol, rst))
            yield gen.streamingExecutionConfig[F]).value

      override def gmosSouthImaging(
        context: GeneratorContext
      )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, StreamingExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]]] =
        select(sequenceService.streamingGmosSouthExecutionConfig(context.oid))
          .getOrElseF:
            import lucuma.odb.sequence.gmos.imaging.Imaging
            (for
              cfg <- extractMode(ObservingMode.GmosSouthImagingName, context)(_.asGmosSouthImaging)
              itc  = requireImagingItc(ObservingMode.GmosSouthImagingName, context.oid, context.itcRes, Itc.gmosSouthImaging.getOption)
              gen <- EitherT(Imaging.gmosSouth(calculator.gmosSouth, context.namespace, cfg, itc))
            yield gen.streamingExecutionConfig[F]).value

      override def gmosSouthLongSlit(
        context: GeneratorContext
      )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, StreamingExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]]] =
        select(sequenceService.streamingGmosSouthExecutionConfig(context.oid))
          .getOrElseF:
            import lucuma.odb.sequence.gmos.longslit.LongSlit
            (for
              cfg <- extractMode(ObservingMode.GmosSouthLongSlitName, context)(_.asGmosSouthLongSlit)
              itc  = requireSpectroscopyItc(context.oid, context.itcRes)
              rol  = context.params.calibrationRole
              rst  = context.params.acqResetTime
              gen <- EitherT(LongSlit.gmosSouth(context.oid, calculator.gmosSouth, context.namespace, exp.gmosSouth, cfg, itc, rol, rst))
            yield gen.streamingExecutionConfig[F]).value