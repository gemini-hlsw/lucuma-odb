// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.data.EitherT
import cats.data.OptionT
import cats.effect.Async
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.option.*
import lucuma.core.data.Zipper
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.core.model.sequence.flamingos2.Flamingos2DynamicConfig as Flamingos2Dynamic
import lucuma.core.model.sequence.flamingos2.Flamingos2StaticConfig as Flamingos2Static
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosNorth as GmosNorthDynamic
import lucuma.core.model.sequence.gmos.DynamicConfig.GmosSouth as GmosSouthDynamic
import lucuma.core.model.sequence.gmos.StaticConfig.GmosNorth as GmosNorthStatic
import lucuma.core.model.sequence.gmos.StaticConfig.GmosSouth as GmosSouthStatic
import lucuma.odb.data.Itc
import lucuma.odb.data.Md5Hash
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.ObservingMode
import lucuma.odb.sequence.ObservingMode.Syntax.*
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.data.StreamingExecutionConfig
import lucuma.odb.sequence.syntax.hash.*
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.sequence.util.HashBytes
import lucuma.odb.sequence.util.SequenceIds
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import skunk.Transaction

import java.security.MessageDigest
import java.util.UUID

/**
 * ExecutionConfigGenerator produces StreamingExecutionConfig values for each
 * of the observing modes, hiding the detail of whether it is read from the
 * database or generated from observation configuration.
 */
sealed trait ExecutionConfigGenerator[F[_]]:

  def flamingos2LongSlit(
    observationId: Observation.Id
  )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, StreamingExecutionConfig[F, Flamingos2Static, Flamingos2Dynamic]]]

  def gmosNorthImaging(
    observationId: Observation.Id
  )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, StreamingExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]]]

  def gmosNorthLongSlit(
    observationId: Observation.Id
  )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, StreamingExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]]]

  def gmosSouthImaging(
    observationId: Observation.Id
  )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, StreamingExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]]]

  def gmosSouthLongSlit(
    observationId: Observation.Id
  )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, StreamingExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]]]

object ExecutionConfigGenerator:

  // The digest calculation needs to go through every step in the sequence.
  // The ITC sometimes returns `Int.MaxValue`, which leads to timeouts.  This is
  // a reasonable upper limit on the number of atoms in a sequence.
  val SequenceAtomLimit = 1000

  object Error:
    def sequenceUnavailable(oid: Observation.Id, message: String): OdbError =
      OdbError.SequenceUnavailable(oid, s"Could not generate a sequence for $oid: $message".some)

    def sequenceTooLong(oid: Observation.Id): OdbError =
      sequenceUnavailable(oid, s"The generated sequence is too long (more than $SequenceAtomLimit atoms).")

    def programNotFound(oid: Observation.Id): OdbError =
      OdbError.InvalidObservation(oid, s"Program for observation $oid not found.".some)

    def unexpectedMode(oid: Observation.Id, expected: String, actual: ObservingMode): OdbError =
      OdbError.InvalidObservation(oid, s"Expected observation $oid as $expected, but was ${actual.name}".some)

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
  ): ExecutionConfigGenerator[F] =

    case class Context(
      pid:    Program.Id,
      oid:    Observation.Id,
      itcRes: Either[OdbError, Itc],
      params: GeneratorParams
    ):
      def namespace: UUID =
        SequenceIds.namespace(commitHash, oid, params)

      val hash: Md5Hash =
        val md5 = MessageDigest.getInstance("MD5")

        // Generator Params
        md5.update(params.hashBytes)

        def addResultSet(z: Zipper[Itc.Result]): Unit =
          md5.update(z.focus.value.exposureTime.hashBytes)
          md5.update(z.focus.value.exposureCount.hashBytes)

        def addImagingResultSet[A: HashBytes](kv: (A, Zipper[Itc.Result])): Unit =
          md5.update(kv._1.hashBytes)
          addResultSet(kv._2)

        // ITC
        itcRes.foreach: itc =>
          itc match
            case Itc.Spectroscopy(acq, sci) =>
              addResultSet(acq)
              addResultSet(sci)
            case Itc.GmosNorthImaging(m)   =>
              m.toNel.toList.foreach(addImagingResultSet)
            case Itc.GmosSouthImaging(m)   =>
              m.toNel.toList.foreach(addImagingResultSet)

        // Commit Hash
        md5.update(commitHash.hashBytes)

        Md5Hash.unsafeFromByteArray(md5.digest())

    object Context:

      def lookup(
        oid: Observation.Id
      )(using NoTransaction[F]): EitherT[F, OdbError, Context] =
        val itc = itcService

        def lookupPid(using Transaction[F]): EitherT[F, OdbError, Program.Id] =
          EitherT:
            observationService
              .selectProgram(oid)
              .map(_.toOption.toRight(Error.programNotFound(oid)))

        val opc: F[Either[OdbError, (Program.Id, GeneratorParams, Option[Itc])]] =
          services.transactionally:
            (for
              pid <- lookupPid
              p   <- EitherT(generatorParamsService.selectOne(pid, oid).map(_.leftMap(e => Error.sequenceUnavailable(oid, e.format))))
              c   <- EitherT.liftF(itc.selectOne(pid, oid, p))
            yield (pid, p, c)).value

        def callItc(pid: Program.Id, p: GeneratorParams): EitherT[F, OdbError, Itc] =
          EitherT(itc.callRemote(pid, oid, p))

        for
          pc <- EitherT(opc)
          (pid, params, cached) = pc
          // This will be confusing, but the idea here is that if the observation
          // definition is missing target information we just record that in the
          // Context.  On the other hand if there is an error calling the ITC then
          // we cannot create the Context.
          as <- params.itcInput.fold(
            m => EitherT.pure(Error.sequenceUnavailable(oid, s"Missing parameters: ${m.format}").asLeft),
            _ => cached.fold(callItc(pid, params))(EitherT.pure(_)).map(_.asRight)
          )
        yield Context(pid, oid, as, params)

    new ExecutionConfigGenerator[F]:

      private val exp = SmartGcalImplementation.fromService(smartGcalService)

      private def select[S, D](
        lookup: (Transaction[F], Services.ServiceAccess) ?=> F[Option[StreamingExecutionConfig[F, S, D]]]
      )(using NoTransaction[F], Services.ServiceAccess): OptionT[F, Either[OdbError, StreamingExecutionConfig[F, S, D]]] =
        OptionT(services.transactionally(lookup)).map(_.asRight[OdbError])

      private def extractMode[A](
        observationId: Observation.Id,
        expected:      String,
        context:       Context
      )(f: ObservingMode => Option[A]): EitherT[F, OdbError, A] =
        val mode = context.params.observingMode
        EitherT.fromOption[F](f(mode), Error.unexpectedMode(observationId, expected, mode))

      override def flamingos2LongSlit(
        observationId: Observation.Id
      )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, StreamingExecutionConfig[F, Flamingos2Static, Flamingos2Dynamic]]] =
        select(sequenceService.streamingFlamingos2ExecutionConfig(observationId))
          .getOrElseF:
            import lucuma.odb.sequence.flamingos2.longslit.LongSlit
            (for
              ctx <- Context.lookup(observationId)
              cfg <- extractMode(observationId, ObservingMode.Flamingos2LongSlitName, ctx)(_.asFlamingos2LongSlit)
              itc  = requireSpectroscopyItc(observationId, ctx.itcRes)
              gen <- EitherT(LongSlit.instantiate(observationId, calculator.flamingos2, ctx.namespace, exp.flamingos2, cfg, itc, ctx.params.acqResetTime))
            yield gen.streamingExecutionConfig[F]).value

      override def gmosNorthImaging(
        observationId: Observation.Id
      )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, StreamingExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]]] =
        select(sequenceService.streamingGmosNorthExecutionConfig(observationId))
          .getOrElseF:
            import lucuma.odb.sequence.gmos.imaging.Imaging
            (for
              ctx <- Context.lookup(observationId)
              cfg <- extractMode(observationId, ObservingMode.GmosNorthImagingName, ctx)(_.asGmosNorthImaging)
              itc  = requireImagingItc(ObservingMode.GmosNorthImagingName, observationId, ctx.itcRes, Itc.gmosNorthImaging.getOption)
              gen <- EitherT(Imaging.gmosNorth(calculator.gmosNorth, ctx.namespace, cfg, itc))
            yield gen.streamingExecutionConfig[F]).value

      override def gmosNorthLongSlit(
        observationId: Observation.Id
      )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, StreamingExecutionConfig[F, GmosNorthStatic, GmosNorthDynamic]]] =
        select(sequenceService.streamingGmosNorthExecutionConfig(observationId))
          .getOrElseF:
            import lucuma.odb.sequence.gmos.longslit.LongSlit
            (for
              ctx <- Context.lookup(observationId)
              cfg <- extractMode(observationId, ObservingMode.GmosNorthLongSlitName, ctx)(_.asGmosNorthLongSlit)
              itc  = requireSpectroscopyItc(observationId, ctx.itcRes)
              rol  = ctx.params.calibrationRole
              rst  = ctx.params.acqResetTime
              gen <- EitherT(LongSlit.gmosNorth(observationId, calculator.gmosNorth, ctx.namespace, exp.gmosNorth, cfg, itc, rol, rst))
            yield gen.streamingExecutionConfig[F]).value

      override def gmosSouthImaging(
        observationId: Observation.Id
      )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, StreamingExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]]] =
        select(sequenceService.streamingGmosSouthExecutionConfig(observationId))
          .getOrElseF:
            import lucuma.odb.sequence.gmos.imaging.Imaging
            (for
              ctx <- Context.lookup(observationId)
              cfg <- extractMode(observationId, ObservingMode.GmosSouthImagingName, ctx)(_.asGmosSouthImaging)
              itc  = requireImagingItc(ObservingMode.GmosSouthImagingName, observationId, ctx.itcRes, Itc.gmosSouthImaging.getOption)
              gen <- EitherT(Imaging.gmosSouth(calculator.gmosSouth, ctx.namespace, cfg, itc))
            yield gen.streamingExecutionConfig[F]).value

      override def gmosSouthLongSlit(
        observationId: Observation.Id
      )(using NoTransaction[F], Services.ServiceAccess): F[Either[OdbError, StreamingExecutionConfig[F, GmosSouthStatic, GmosSouthDynamic]]] =
        select(sequenceService.streamingGmosSouthExecutionConfig(observationId))
          .getOrElseF:
            import lucuma.odb.sequence.gmos.longslit.LongSlit
            (for
              ctx <- Context.lookup(observationId)
              cfg <- extractMode(observationId, ObservingMode.GmosSouthLongSlitName, ctx)(_.asGmosSouthLongSlit)
              itc  = requireSpectroscopyItc(observationId, ctx.itcRes)
              rol  = ctx.params.calibrationRole
              rst  = ctx.params.acqResetTime
              gen <- EitherT(LongSlit.gmosSouth(observationId, calculator.gmosSouth, ctx.namespace, exp.gmosSouth, cfg, itc, rol, rst))
            yield gen.streamingExecutionConfig[F]).value