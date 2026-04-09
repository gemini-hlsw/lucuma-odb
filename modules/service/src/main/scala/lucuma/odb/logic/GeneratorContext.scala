// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.logic

import cats.data.EitherT
import cats.data.OptionT
import cats.effect.Concurrent
import cats.syntax.applicative.*
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
        case Itc.GhostIfu(r, b)           =>
          addResultSet(r)
          addResultSet(b)
        case Itc.GmosNorthImaging(m)      =>
          m.toNel.toList.foreach(addImagingResultSet)
        case Itc.GmosSouthImaging(m)      =>
          m.toNel.toList.foreach(addImagingResultSet)
        case Itc.Igrins2Spectroscopy(sci) =>
          addResultSet(sci)
        case Itc.Spectroscopy(acq, sci)   =>
          addResultSet(acq)
          addResultSet(sci)

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

    // Attempt a cached ITC result lookup.
    def fetchCached(
      pid:    Program.Id,
      params: GeneratorParams
    )(using Transaction[F]): F[Option[Either[OdbError, Itc]]] =
      OptionT(itc.selectOne(pid, oid, params)).map(_.asRight).value

    val opc: F[Either[OdbError, (Program.Id, GeneratorParams, Option[Either[OdbError, Itc]])]] =
      services.transactionally:
        (for
          pid <- lookupPid
          p   <- EitherT(generatorParamsService.selectOne(pid, oid).map(_.leftMap(e => sequenceUnavailable(oid, e.format))))
          c   <- EitherT.liftF:
                   itcResults match
                     case Some(r) => r.some.pure[F]       // use provided results when given
                     case None    => fetchCached(pid, p)  // attempt a lookup when not given
        yield (pid, p, c)).value

    def callItc(pid: Program.Id, p: GeneratorParams): EitherT[F, OdbError, Itc] =
      EitherT(itc.callRemote(pid, oid, p))

    (for
      pc <- EitherT(opc)
      (pid, params, cachedItcResults) = pc
      // This will be confusing, but the idea here is that if the observation
      // definition is missing target information we just record that in the
      // Context.  On the other hand if there is an error calling the ITC then
      // we shortcircuit / cannot create the GeneratorContext at all.  So,
      // we use EitherT for short-circuiting control flow, but the payload is
      // also an Either value which may be a Left (for example when the target
      // is missing a SED).
      as <- params.itcInput.fold(
        m => EitherT.pure(sequenceUnavailable(oid, s"Missing parameters: ${m.format}").asLeft),
        _ => cachedItcResults.fold(callItc(pid, params).map(_.asRight))(EitherT.pure(_))
      )
    yield GeneratorContext(oid, as, params, commitHash)).value
