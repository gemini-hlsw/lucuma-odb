// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.Concurrent
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import eu.timepit.refined.types.string.NonEmptyString
import grackle.Result
import grackle.ResultT
import io.circe.Json
import io.circe.JsonObject
import lucuma.core.enums.Partner
import lucuma.core.model.Program
import lucuma.core.model.StandardRole
import lucuma.core.util.Enumerated
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.data.SummaryStyle
import lucuma.odb.service.Services.ServiceAccess
import lucuma.odb.service.Services.SuperUserAccess
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.all.*
import skunk.syntax.all.*

import scala.concurrent.duration.*

import Services.Syntax.*

/**
 * Proposal-summary PDF jobs (`t_summary_job`): one (program, partner) pair per
 * job.  The web side enqueues; the daemon takes the `next` job, renders it and
 * calls `finalize` or `fail`.  `next` and `finalize` manage their own
 * transactions, since the payload query and the upload cannot run inside one.
 */
trait PdfSummaryJobService[F[_]]:

  /** One job per partner with a split, or a single partnerless job; a `pending` duplicate is skipped. */
  def enqueue(pid: Program.Id)(using Transaction[F], SuperUserAccess): F[Unit]

  /** The `regenerateProposalSummaries` mutation: authorize, then enqueue. */
  def regenerate(pid: Program.Id)(using NoTransaction[F], Services.PiAccess): F[Result[Unit]]

  /**
   * Sweep stale `rendering` jobs, then claim the oldest due `pending` job and
   * build its payload.  `None` when the queue is empty.  Needs a `Services`
   * with a GraphQL mapping.
   */
  def next(using NoTransaction[F], ServiceAccess): F[Option[PdfSummaryJobService.Prepared]]

  /**
   * Upload the PDF, replace the partner's summary attachment and delete the
   * job.  If the job was swept meanwhile the upload is discarded instead.
   */
  def finalize(prepared: PdfSummaryJobService.Prepared, pdf: fs2.Stream[F, Byte])(using NoTransaction[F], ServiceAccess): F[Unit]

  /** `failed` when permanent or out of attempts, else back to `pending` with backoff. */
  def fail(job: PdfSummaryJobService.Claimed, error: String, permanent: Boolean)(using Transaction[F], ServiceAccess): F[Unit]


object PdfSummaryJobService:

  // Render attempts before a job is given up on.
  val MaxAttempts: Int = 3

  // A render still running after this long is assumed dead.
  val StaleRender: FiniteDuration = 30.minutes

  /** A `rendering` job. */
  case class Claimed(
    id:        Long,
    programId: Program.Id,
    partner:   Option[Partner],
    style:     SummaryStyle,
    attempts:  Int
  )

  /** A claimed job with everything the daemon needs to render and file it. */
  case class Prepared(
    job:        Claimed,
    payload:    Json,
    fileName:   NonEmptyString,
    remotePath: NonEmptyString
  )

  def noProposal(pid: Program.Id): OdbError =
    OdbError.InvalidArgument(s"Program $pid has no proposal to summarize.".some)

  def instantiate[F[_]: {Concurrent, UUIDGen, Services}](s3FileService: S3FileService[F]): PdfSummaryJobService[F] =
    new PdfSummaryJobService[F]:

      private case class Queried(
        program:      Json,
        observations: List[Json],
        proposalRef:  Option[String]
      )

      private def queryProgram(pid: Program.Id): F[Either[String, Queried]] =
        val vars = JsonObject("programId" -> Json.fromString(pid.toString))
        services.runGraphQLQuery(PdfSummaryJobPayload.Query, none, vars.some).map:
          case Result.Success(json)     => extract(json)
          case Result.Warning(_, json)  => extract(json)
          case Result.Failure(ps)       => ps.toList.map(_.message).mkString("; ").asLeft
          case Result.InternalError(e)  => e.getMessage.asLeft

      private def extract(json: Json): Either[String, Queried] =
        val c = json.hcursor
        // A truncated observation list must not render.
        def complete(obs: io.circe.ACursor): Either[String, Unit] =
          obs.downField("hasMore").as[Boolean].leftMap(_.message)
            .filterOrElse(!_, s"more than ${PdfSummaryJobPayload.MaxObservations} observations").void

        ( c.downField("program").focus.filter(!_.isNull).toRight("program not found"),
          c.downField("observations").downField("matches").as[List[Json]].leftMap(_.message),
          complete(c.downField("observations")),
          complete(c.downField("program").downField("observations"))
        ).tupled.map: (program, obs, _, _) =>
          val ref =
            program
              .hcursor
              .downField("proposal")
              .downField("reference")
              .downField("label")
              .as[String]
              .toOption
          Queried(program, obs, ref)

      private def presignAttachments(pid: Program.Id)(using SuperUserAccess): F[List[PdfSummaryJobPayload.AttachmentUrl]] =
        session.execute(Statements.SelectProposalAttachments)(pid).flatMap: as =>
          as.traverse: (name, path) =>
            s3FileService.presignedUrl(path).map(PdfSummaryJobPayload.AttachmentUrl(name.value, _))

      private def partners(pid: Program.Id): F[List[Option[Partner]]] =
        session.execute(Statements.SelectPartners)(pid).map:
          case Nil => List(none)
          case ps  => ps.map(_.some)

      override def enqueue(pid: Program.Id)(using Transaction[F], SuperUserAccess): F[Unit] =
        partners(pid).flatMap(_.traverse_(partner =>
          session.execute(Statements.InsertJob)((pid, partner, SummaryStyle.forPartner(partner)))
        ))

      override def regenerate(pid: Program.Id)(using NoTransaction[F], Services.PiAccess): F[Result[Unit]] =
        def check(ok: Boolean, error: => OdbError): Result[Unit] =
          if ok then Result.unit else error.asFailure

        def allowed(using Transaction[F]): F[Boolean] = user.role match
          case StandardRole.Ngo(_, _) => false.pure[F]
          case _                      => programUserService.userHasWriteAccess(pid)

        services.transactionallyT:
          for
            _ <- ResultT(allowed.map(check(_, OdbError.NotAuthorized(user.id))))
            _ <- ResultT(session.unique(Statements.HasProposal)(pid).map(check(_, noProposal(pid))))
            _ <- ResultT.liftF(Services.asSuperUser(enqueue(pid)))
          yield ()
        .value

      private def prepare(job: Claimed)(using SuperUserAccess): F[Either[String, Prepared]] =
        queryProgram(job.programId).flatMap(_.traverse: q =>
          for
            atts <- presignAttachments(job.programId)
            uuid <- UUIDGen[F].randomUUID
          yield
            // File names are unique per program, and the partner is what tells
            // one program's summaries apart.
            val partner  = job.partner.foldMap(p => s"-${Enumerated[Partner].tag(p)}")
            // TODO: Review if the naming patter is ok for science
            val fileName = NonEmptyString.unsafeFrom(s"${q.proposalRef.getOrElse(job.programId.toString)}-summary$partner.pdf")
            val payload  = PdfSummaryJobPayload.build(q.program, q.observations, atts)
            Prepared(job, payload, fileName, s3FileService.filePath(job.programId, uuid, fileName))
        )

      override def next(using NoTransaction[F], ServiceAccess): F[Option[Prepared]] =
        val claim: F[Option[Claimed]] =
          services.transactionally:
            session.execute(Statements.FailStale)((MaxAttempts, StaleRender.toSeconds)) *>
              session.execute(Statements.RependStale)(StaleRender.toSeconds) *>
              session.option(Statements.Claim)
        // An unbuildable payload fails the job and moves on, so None means empty.
        claim.flatMap(_.flatTraverse: job =>
          Services.asSuperUser(prepare(job)).flatMap:
            case Right(prepared) => prepared.some.pure[F]
            case Left(msg)       =>
              services.transactionally(fail(job, s"Could not build the payload: $msg", permanent = true)) *> next
        )

      override def finalize(prepared: Prepared, pdf: fs2.Stream[F, Byte])(using NoTransaction[F], ServiceAccess): F[Unit] =
        val job = prepared.job
        Services.asSuperUser:
          for
            size     <- s3FileService.upload(prepared.remotePath, pdf)
            // The replaced summary, or this upload if the job was swept.
            obsolete <- services.transactionally:
                          session.option(Statements.LockRendering)(job.id).flatMap:
                            case None    => prepared.remotePath.some.pure[F]
                            case Some(_) =>
                              for
                                old  <- session.option(Statements.DeleteSummaryAttachment)((job.programId, job.partner))
                                desc  = NonEmptyString.unsafeFrom(s"Proposal summary (${job.style.rendererName})")
                                _    <- session.unique(Statements.InsertSummaryAttachment)((job.programId, prepared.fileName, desc, size, prepared.remotePath, job.partner, job.style))
                                _    <- session.execute(Statements.DeleteJob)(job.id)
                              yield old
            _        <- obsolete.traverse_(s3FileService.delete(_).handleError(_ => ()))
          yield ()

      override def fail(job: Claimed, error: String, permanent: Boolean)(using Transaction[F], ServiceAccess): F[Unit] =
        if permanent || job.attempts >= MaxAttempts then
          session.execute(Statements.MarkFailed)((error, job.id)).void
        else
          session.execute(Statements.Reschedule)((error, job.id)).void

  object Statements:

    val HasProposal: Query[Program.Id, Boolean] =
      sql"""
        SELECT EXISTS (SELECT 1 FROM t_proposal WHERE c_program_id = $program_id)
      """.query(bool)

    val SelectPartners: Query[Program.Id, Partner] =
      sql"""
        SELECT DISTINCT c_partner
        FROM t_partner_split
        WHERE c_program_id = $program_id AND c_percent > 0
        ORDER BY c_partner
      """.query(partner)

    val SelectProposalAttachments: Query[Program.Id, (NonEmptyString, NonEmptyString)] =
      sql"""
        SELECT c_file_name, c_remote_path
        FROM t_attachment
        WHERE c_program_id = $program_id
          AND c_attachment_type IN ('science', 'team')
        ORDER BY c_attachment_type
      """.query(text_nonempty *: text_nonempty)

    // A no-op when a job for this partner is already waiting.
    val InsertJob: Command[(Program.Id, Option[Partner], SummaryStyle)] =
      sql"""
        INSERT INTO t_summary_job (c_program_id, c_partner, c_style)
        VALUES ($program_id, ${partner.opt}, $summary_style)
        ON CONFLICT (c_program_id, c_partner) WHERE c_state = 'pending' DO NOTHING
      """.command

    val Claim: Query[Void, Claimed] =
      sql"""
        UPDATE t_summary_job
        SET c_state      = 'rendering',
            c_started_at = now(),
            c_retry_at   = NULL,
            c_attempts   = c_attempts + 1
        WHERE c_summary_job_id = (
          SELECT c_summary_job_id
          FROM t_summary_job
          WHERE c_state = 'pending'
            AND (c_retry_at IS NULL OR c_retry_at <= now())
          ORDER BY c_created_at
          FOR UPDATE SKIP LOCKED
          LIMIT 1
        )
        RETURNING c_summary_job_id, c_program_id, c_partner, c_style, c_attempts
      """.query((int8 *: program_id *: partner.opt *: summary_style *: int4).to[Claimed])

    val LockRendering: Query[Long, Long] =
      sql"""
        SELECT c_summary_job_id
        FROM t_summary_job
        WHERE c_summary_job_id = $int8 AND c_state = 'rendering'
        FOR UPDATE
      """.query(int8)

    val DeleteSummaryAttachment: Query[(Program.Id, Option[Partner]), NonEmptyString] =
      sql"""
        DELETE FROM t_attachment
        WHERE c_program_id = $program_id
          AND c_attachment_type = 'summary'
          AND c_partner IS NOT DISTINCT FROM ${partner.opt}
        RETURNING c_remote_path
      """.query(text_nonempty)

    val InsertSummaryAttachment: Query[(Program.Id, NonEmptyString, NonEmptyString, Long, NonEmptyString, Option[Partner], SummaryStyle), lucuma.core.model.Attachment.Id] =
      sql"""
        INSERT INTO t_attachment (
          c_program_id,
          c_attachment_type,
          c_file_name,
          c_description,
          c_file_size,
          c_remote_path,
          c_partner,
          c_summary_style
        ) VALUES (
          $program_id,
          'summary',
          $text_nonempty,
          $text_nonempty,
          $int8,
          $text_nonempty,
          ${partner.opt},
          $summary_style
        )
        RETURNING c_attachment_id
      """.query(attachment_id)

    val DeleteJob: Command[Long] =
      sql"""
        DELETE FROM t_summary_job
        WHERE c_summary_job_id = $int8
      """.command

    val MarkFailed: Command[(String, Long)] =
      sql"""
        UPDATE t_summary_job
        SET c_state = 'failed', c_error = $text
        WHERE c_summary_job_id = $int8 AND c_state = 'rendering'
      """.command

    // Backoff: 1, 4, 16, ... minutes by attempt.
    val Reschedule: Command[(String, Long)] =
      sql"""
        UPDATE t_summary_job
        SET c_state    = 'pending',
            c_error    = $text,
            c_retry_at = now() + make_interval(mins => power(4, c_attempts - 1)::int)
        WHERE c_summary_job_id = $int8 AND c_state = 'rendering'
      """.command

    val FailStale: Command[(Int, Long)] =
      sql"""
        UPDATE t_summary_job
        SET c_state = 'failed', c_error = 'Rendering did not complete'
        WHERE c_state = 'rendering'
          AND c_attempts >= $int4
          AND c_started_at < now() - make_interval(secs => $int8)
      """.command

    val RependStale: Command[Long] =
      sql"""
        UPDATE t_summary_job
        SET c_state = 'pending'
        WHERE c_state = 'rendering'
          AND c_started_at < now() - make_interval(secs => $int8)
      """.command
