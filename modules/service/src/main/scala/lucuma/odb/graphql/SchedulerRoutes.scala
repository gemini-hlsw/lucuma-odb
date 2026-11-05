// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.Monad
import cats.Parallel
import cats.effect.*
import cats.effect.std.SecureRandom
import cats.effect.std.UUIDGen
import cats.implicits.*
import eu.timepit.refined.types.numeric.PosInt
import fs2.compression.Compression
import fs2.compression.DeflateParams
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.goa.GoaClient
import lucuma.catalog.telluric.TelluricTargetsClient
import lucuma.core.model.Access
import lucuma.core.model.AccessControlException
import lucuma.core.model.Observation
import lucuma.core.model.Target
import lucuma.core.model.User
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.AtomDigest
import lucuma.core.syntax.string.*
import lucuma.core.util.Gid
import lucuma.core.util.Timestamp
import lucuma.core.util.Uid
import lucuma.horizons.HorizonsClient
import lucuma.itc.client.ItcClient
import lucuma.odb.Config
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.sequence.util.CommitHash
import lucuma.odb.service.S3FileService
import lucuma.odb.service.Services
import lucuma.odb.service.Services.Syntax.*
import lucuma.odb.service.VisibilityChange
import lucuma.sso.client.SsoClient
import org.http4s.*
import org.http4s.client.Client
import org.http4s.dsl.Http4sDsl
import org.http4s.server.middleware.EntityLimiter
import org.http4s.server.middleware.GZip
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import org.typelevel.otel4s.trace.Tracer
import skunk.Session
import skunk.Transaction

import java.time.Instant

object SchedulerRoutes:

  // the normal constructor
  def apply[F[_]: Async: Parallel: Logger: LoggerFactory: Tracer: SecureRandom](
    pool:           Resource[F, Session[F]],
    ssoClient:      SsoClient[F, User],
    emailConfig:    Config.Email,
    commitHash:     CommitHash,
    tc:             TimeEstimateCalculatorImplementation.ForInstrumentMode,
    httpClient:     Client[F],
    itcClient:      ItcClient[F],
    gaiaClient:     GaiaClient[F],
    horizonsClient: HorizonsClient[F]
  ): HttpRoutes[F] =
    apply(
      [A] => (u: User) => (fa: Services[F] => F[A]) => pool.map(
        Services.forUser(
          u,
          None,
          emailConfig,
          commitHash,
          tc,
          httpClient,
          itcClient,
          gaiaClient,
          S3FileService.noop[F],
          horizonsClient,
          TelluricTargetsClient.noop[F],
          GoaClient.noop[F]
        )).use(fa),
      ssoClient,
      PageSizes.Default
    )

  // used by tests
  def apply[F[_]: Async](
    services:  Services[F],
    ssoClient: SsoClient[F, User],
    pageSizes: PageSizes = PageSizes.Default
  ): HttpRoutes[F] =
    apply(
      [A] => (_: User) => (fa: Services[F] => F[A]) => fa(services),
      ssoClient,
      pageSizes
    )

  private type AtomRow = (Observation.Id, Short, AtomDigest)

  private def tsv(row: AtomRow): String =
    List(
      Gid[Observation.Id].show(row._1),
      row._2.toString,
      Uid[Atom.Id].show(row._3.id),
      row._3.observeClass.tag.toScreamingSnakeCase,
      row._3.timeEstimate.sum.toMicroseconds.toString,
      row._3.stepTypes.map(_.tag.toScreamingSnakeCase).mkString("[", ", ", "]"),
      row._3.lampTypes.map(_.tag.toScreamingSnakeCase).mkString("[", ", ", "]"),
      row._3.stepIndex.value.toString,
      row._3.stepCount.value.toString
    ).intercalate("\t")

  private def visibilityTsv(c: VisibilityChange): String =
    c match
      case VisibilityChange.ForObservation(id, invalidatedAt) =>
        s"${Gid[Observation.Id].show(id)}\t${invalidatedAt.toInstant}"
      case VisibilityChange.ForTarget(id, invalidatedAt) =>
        s"${Gid[Target.Id].show(id)}\t${invalidatedAt.toInstant}"

  /**
   * Maximum accepted size of a request body.  An observation id line is ~20
   * bytes, so this leaves room for tens of thousands of ids.
   */
  val MaxAtomsRequestBytes: Long = 1024L * 1024L

  /**
   * Maximum accepted length of a line in an atoms request.  A valid observation
   * id is at most 18 characters, so anything longer is rejected without being
   * accumulated further.
   */
  val MaxAtomsRequestLineLength: Int = 64

  /** Maximum number of unparseable lines reported back in a BadRequest. */
  val MaxReportedErrors: Int = 5

  /** Maximum length, in code points, of each reported unparseable line. */
  val MaxReportedErrorLength: Int = 32

  // Character types that are escaped rather than echoed: controls, invisible
  // formatting (e.g., bidi overrides), lone surrogates, and code points with
  // no assigned or public meaning.
  private val UnprintableTypes: Set[Int] =
    Set(
      Character.CONTROL,
      Character.FORMAT,
      Character.SURROGATE,
      Character.UNASSIGNED,
      Character.PRIVATE_USE,
      Character.LINE_SEPARATOR,
      Character.PARAGRAPH_SEPARATOR
    ).map(_.toInt)

  /**
   * Makes an unparseable input line safe to echo in an error message: keeps at
   * most `MaxReportedErrorLength` code points (never splitting a surrogate
   * pair), writes unprintable code points as `\uXXXX` (or `\u{XXXXX}` beyond
   * the BMP), and doubles backslashes so that the escapes are unambiguous.
   */
  private def sanitize(line: String): String =
    val cps = line.codePoints.limit(MaxReportedErrorLength + 1L).toArray
    val sb  = new StringBuilder
    cps.iterator.take(MaxReportedErrorLength).foreach: cp =>
      if cp === '\\'.toInt then sb.append("\\\\")
      else if UnprintableTypes.contains(Character.getType(cp)) then
        sb.append(if cp <= 0xFFFF then f"\\u$cp%04x" else f"\\u{$cp%x}")
      else sb.appendAll(Character.toChars(cp))
    if cps.length > MaxReportedErrorLength then sb.append("...")
    sb.result()

  /**
   * Parses newline-separated observation ids, keeping only a bounded sample of
   * the invalid lines for the error message.  Fails fast on any line longer
   * than `MaxAtomsRequestLineLength`.
   */
  private def parseObservationIds[F[_]: Sync](
    body: fs2.Stream[F, Byte]
  ): F[Either[String, List[Observation.Id]]] =
    body
      .through(fs2.text.utf8.decode)
      .through(fs2.text.linesLimited(MaxAtomsRequestLineLength))
      .filter(_.nonEmpty)
      .compile
      .fold((List.empty[Observation.Id], Vector.empty[String], 0)):
        case ((ids, errs, errCount), line) =>
          Observation.Id.parse(line) match
            case Some(oid) => (oid :: ids, errs, errCount)
            case None      =>
              val errsʹ = if errs.sizeIs < MaxReportedErrors then errs :+ sanitize(line) else errs
              (ids, errsʹ, errCount + 1)
      .map: (ids, errs, errCount) =>
        if errCount === 0 then ids.reverse.asRight
        else
          val more = errCount - errs.size
          val suffix = if more > 0 then s" (and $more more)" else ""
          s"Unable to parse observation ids: ${errs.mkString(", ")}$suffix".asLeft
      .recover:
        case _: fs2.text.LineTooLongException =>
          s"Unable to parse observation ids: a line exceeds $MaxAtomsRequestLineLength characters".asLeft

  /**
   * How much is read per transaction.  Every page holds at most `rowLimit`
   * rows (atom digests or visibility changes).  Atom digests are also read
   * for at most `observationBatchSize` requested observations at a time,
   * which bounds the number of bind parameters in each query.
   */
  case class PageSizes(
    observationBatchSize: PosInt,
    rowLimit:             PosInt
  )

  object PageSizes:
    val Default: PageSizes =
      PageSizes(PosInt.unsafeFrom(1000), PosInt.unsafeFrom(10000))

  /**
   * Where the next page of atom digests starts: in the observations of `batch`
   * after the (observation, atom index) `after`, with the `rest` of the
   * batches still to come.
   */
  private case class AtomCursor(
    batch: List[Observation.Id],
    rest:  List[List[Observation.Id]],
    after: Option[(Observation.Id, Short)]
  )

  /** Where the next page of visibility changes starts. */
  private enum VisibilityCursor:
    case Observations(after: Option[Observation.Id])
    case Targets(after: Option[Target.Id])

  /**
   * A response body read one page at a time, each page fetched (and fully
   * materialized) by `fetch`, which also returns the state for the next page
   * if there is one.  The first page is fetched before the body is returned,
   * so a failure there becomes an error response rather than a truncated 200.
   * A failure on a later page still truncates the body.
   */
  private def pagedBody[F[_]: Monad, S, A](
    init:  S
  )(fetch: S => F[(List[A], Option[S])]): F[fs2.Stream[F, A]] =
    fetch(init).map: (first, next) =>
      fs2.Stream.emits(first) ++
        fs2.Stream
          .unfoldEval(next)(_.traverse(fetch))
          .flatMap(fs2.Stream.emits)

  /** The last row of a full page, from which the next page starts. */
  private def lastOfFull[A](page: List[A], limit: PosInt): Option[A] =
    page.lastOption.filter(_ => page.sizeIs >= limit.value)

  def apply[F[_]: Async](
    services:  [A] => User => (Services[F] => F[A]) => F[A],
    ssoClient: SsoClient[F, User],
    pageSizes: PageSizes
  ): HttpRoutes[F] =
    val dsl = Http4sDsl[F]
    import dsl._

    given Compression[F] = Compression.forSync[F]

    given QueryParamDecoder[Timestamp] =
      QueryParamDecoder[String].emap: s =>
        Either
          .catchNonFatal(Instant.parse(s))
          .leftMap(t => ParseFailure(s"Invalid timestamp '$s'", t.getMessage))
          .flatMap(i => Timestamp.fromInstant(i).toRight(ParseFailure(s"Timestamp out of range: '$s'", s)))

    object SinceMatcher extends QueryParamDecoderMatcher[Timestamp]("since")

    def superUserTransaction[A](user: User)(fa: (Transaction[F], Services[F], Services.ServiceAccess) ?=> F[A]): F[A] =
      services(user): s =>
        Services.asSuperUser:
          s.transactionally(fa)

    def encode(lines: fs2.Stream[F, String]): fs2.Stream[F, Byte] =
      lines.intersperse("\n").through(fs2.text.utf8.encode)

    val routes = HttpRoutes.of[F]:
      case req @ POST -> Root / "scheduler" / "atoms" =>
        // Authenticate before touching the request body so that unauthenticated
        // clients cannot make us read (or echo back) arbitrary input.
        ssoClient.require(req) { user =>
          user.verifyAccess(Access.Service) *>
          parseObservationIds(req.body).flatMap:
            case Left(msg)      =>
              BadRequest(msg)
            case Right(validIds) =>
              // The ids are read in batches, and each batch in pages of rows,
              // each page in its own short transaction, so that memory and bind
              // parameters stay bounded however many ids or atoms there are.
              def fetch(cursor: AtomCursor): F[(List[AtomRow], Option[AtomCursor])] =
                val limit = pageSizes.rowLimit
                superUserTransaction(user)(sequenceService.selectAtomDigests(cursor.batch, cursor.after, limit))
                  .map: page =>
                    val next =
                      lastOfFull(page, limit).fold(
                        cursor.rest match
                          case b :: bs => AtomCursor(b, bs, None).some
                          case Nil     => none
                      )((oid, idx, _) => cursor.copy(after = (oid, idx).some).some)
                    (page, next)

              validIds.distinct.grouped(pageSizes.observationBatchSize.value).toList match
                case Nil     => Ok()
                case b :: bs => pagedBody(AtomCursor(b, bs, None))(fetch).flatMap(rows => Ok(encode(rows.map(tsv))))
        }.handleErrorWith:
           case _: AccessControlException       => Forbidden()
           case EntityLimiter.EntityTooLarge(_) => PayloadTooLarge(s"Request body exceeds $MaxAtomsRequestBytes bytes")
           case other                           => other.raiseError[F, Response[F]]

      case req @ GET -> Root / "scheduler" / "visibility-changes" :? SinceMatcher(since) =>
        // All observations first, then all targets, each paged by id.  Every
        // page shares an upper bound fixed when the request starts, so nothing
        // stamped while paging is returned and the newest time in the response
        // can't pass a change to a row that was already paged.
        def fetch(user: User, until: Timestamp)(cursor: VisibilityCursor): F[(List[VisibilityChange], Option[VisibilityCursor])] =
          val limit = pageSizes.rowLimit

          cursor match
            case VisibilityCursor.Observations(after) =>
              superUserTransaction(user)(visibilityService.selectObservationChanges(since, until, after, limit))
                .map: page =>
                  val next = lastOfFull(page, limit).fold(VisibilityCursor.Targets(None))(c => VisibilityCursor.Observations(c.id.some))
                  (page, next.some)
            case VisibilityCursor.Targets(after)      =>
              superUserTransaction(user)(visibilityService.selectTargetChanges(since, until, after, limit))
                .map: page =>
                  (page, lastOfFull(page, limit).map(c => VisibilityCursor.Targets(c.id.some)))

        ssoClient.require(req) { user =>
          for
            _       <- user.verifyAccess(Access.Service)
            until   <- superUserTransaction(user)(timeService.transactionTime)
            changes <- pagedBody(VisibilityCursor.Observations(None))(fetch(user, until))
            resp    <- Ok(encode(changes.map(visibilityTsv)))
          yield resp
        }.handleErrorWith:
           case _: AccessControlException => Forbidden()
           case other                     => other.raiseError[F, Response[F]]

    GZip(EntityLimiter(routes, MaxAtomsRequestBytes), level = DeflateParams.Level.NINE)
