// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.data.EitherT
import cats.effect.Concurrent
import cats.syntax.either.*
import cats.syntax.functor.*
import cats.syntax.option.*
import lucuma.core.data.Zipper
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.data.Itc
import lucuma.odb.data.Md5Hash
import lucuma.odb.data.OdbError
import lucuma.odb.sequence.data.GeneratorParams
import lucuma.odb.sequence.syntax.hash.*
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.sequence.util.HashBytes
import lucuma.odb.service.NoTransaction
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import skunk.Transaction

import java.security.MessageDigest

case class GeneratorContext(
  oid:        Observation.Id,
  itcRes:     Either[OdbError, Itc],
  params:     GeneratorParams,
  commitHash: CommitHash
):
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

object GeneratorContext:

  def lookup[F[_]: Concurrent](
    oid:        Observation.Id,
    commitHash: CommitHash,
    itcResults: Option[Either[OdbError, Itc]] = None
  )(using NoTransaction[F], Services[F]): F[Either[OdbError, GeneratorContext]] =
    val itc = itcService

    import GeneratorError.*

    def lookupPid(using Transaction[F]): EitherT[F, OdbError, Program.Id] =
      EitherT:
        observationService
          .selectProgram(oid)
          .map(_.toOption.toRight(programNotFound(oid)))

    val opc: F[Either[OdbError, (Program.Id, GeneratorParams, Option[Itc])]] =
      services.transactionally:
        (for
          pid <- lookupPid
          p   <- EitherT(generatorParamsService.selectOne(pid, oid).map(_.leftMap(e => sequenceUnavailable(oid, e.format))))
          c   <- itcResults match {
                    case Some(r) => EitherT.fromEither(r).map(_.some)
                    case None    => EitherT.liftF(itc.selectOne(pid, oid, p))
                 }
        yield (pid, p, c)).value

    def callItc(pid: Program.Id, p: GeneratorParams): EitherT[F, OdbError, Itc] =
      EitherT(itc.callRemote(pid, oid, p))

    (for
      pc <- EitherT(opc)
      (pid, params, cached) = pc
      // This will be confusing, but the idea here is that if the observation
      // definition is missing target information we just record that in the
      // Context.  On the other hand if there is an error calling the ITC then
      // we cannot create the Context.
      as <- params.itcInput.fold(
        m => EitherT.pure(sequenceUnavailable(oid, s"Missing parameters: ${m.format}").asLeft),
        _ => cached.fold(callItc(pid, params))(EitherT.pure(_)).map(_.asRight)
      )
    yield GeneratorContext(oid, as, params, commitHash)).value