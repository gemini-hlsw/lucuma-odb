// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.ApplicativeError
import cats.MonadError
import cats.Parallel
import cats.effect.Async
import cats.effect.MonadCancelThrow
import cats.effect.Resource
import cats.effect.Temporal
import cats.effect.std.UUIDGen
import cats.syntax.all.*
import grackle.Mapping
import grackle.Result
import grackle.ResultT
import io.circe.Json
import io.circe.JsonObject
import lucuma.catalog.clients.GaiaClient
import lucuma.catalog.telluric.TelluricTargetsClient
import lucuma.core.data.Metadata
import lucuma.core.model.Access
import lucuma.core.model.User
import lucuma.core.util.Gid
import lucuma.graphql.routes.GraphQLService
import lucuma.horizons.HorizonsClient
import lucuma.itc.client.ItcClient
import lucuma.odb.Config
import lucuma.odb.data.OdbError
import lucuma.odb.data.OdbErrorExtensions.*
import lucuma.odb.graphql.enums.Enums
import lucuma.odb.logic.Generator
import lucuma.odb.logic.TimeEstimateCalculatorImplementation
import lucuma.odb.logic.TimeEstimateService
import lucuma.odb.sequence.util.CommitHash
import natchez.Trace
import org.http4s.client.Client
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.LoggerFactory
import skunk.Session
import skunk.Transaction
import skunk.codec.all.*
import skunk.syntax.all.*

import scala.util.NotGiven

/** Witnesses that there is no transaction in context. */
type NoTransaction[F[_]] = NotGiven[Transaction[F]]

/**
 * A collection of services, all bound to a single `Session` and `User`, together with a canonical
 * operation for interacting with the database in a transactional way.
 */
trait Services[F[_]]:

  /** The underlying `Session`. */
  def session: Session[F]

  /** Recursively execute a graphql query using the resources of this `Service` instance. */
  def runGraphQLQuery(query: String, op: Option[String] = None, vars: Option[JsonObject] = None): F[Result[Json]]

  /** The associated `User`. */
  def user: User

  /** The dynamic enums loaded from the DB. */
  val enums: Enums

  /** Availability metadata (hardcoded for now). */
  def metadata: Metadata = Metadata.placeholder

  /**
   * Define an interaction with the database that will execute a block `fa` within a transaction,
   * with `user` (see above) set in a transaction-local variable `lucuma.user` in the database,
   * available in PostgreSQL as `current_setting('lucuma.user', true)`
   *
   * Within the passed block `fa` the current `Transaction` is implicit, as well as this `Services`
   * instance. The purpose is as follows:
   *
   *   - When `Services` is `given` and `Services.Syntax.*` is imported, all members here are
   *     available unprefixed; i.e., you can just say `groupService.doWhatever(...)` or `user`
   *     or `session` and the reference will be taken from the implicit `Services` in scope.
   *   - When `Transaction` is `given` and `Services.Syntax.*` is imported, the current transaction
   *     is available as `transaction`. This is rarely necessary, but it allows service code to use
   *     savepoints and rollback to handle errors when necessary.
   *   - Most service methods have a `using Transaction[F]` clause, to prevent them from being
   *     called without an active transaction. This adds a bit of safety.
   *
   * See also `Services.Syntax.useTransactionally`, which is more commonly used; it does the same
   * thing but works on `Resource[F, Service[F]]`. In this form you never actually see the
   * `Services` instance; it's always implicit.
   */
  def transactionally[A](fa: (Transaction[F], Services[F]) ?=> F[A])(using NoTransaction[F]): F[A]

  /** Analog of `transactionally` that works in `ResultT[F,*]`. */
  def transactionallyT[A](fa: (Transaction[F], Services[F]) ?=> ResultT[F, A])(using NoTransaction[F]): ResultT[F, A]

  /** The `AllocationService`. */
  def allocationService: AllocationService[F]

  /** The `AsterismService`. */
  def asterismService: AsterismService[F]

  /** The `AttachmentFileService`.  */
  def attachmentFileService: AttachmentFileService[F]

  /** The `AttachmentMetadataService`. */
  def attachmentMetadataService: AttachmentMetadataService[F]

  /** The `BlindOffsetsService`. */
  def blindOffsetsService: BlindOffsetsService[F]

  /** The `CalibrationsService`. */
  def calibrationsService: CalibrationsService[F]

  /** The `CallForProposalsService`. */
  def callForProposalsService: CallForProposalsService[F]

  /** The `ChronicleService`. */
  def chronicleService: ChronicleService[F]

  /** The `ConfigurationService`. */
  def configurationService: ConfigurationService[F]

  /** The `DatasetService`. */
  def datasetService: DatasetService[F]

  /** The `ExecutionDigestService`. */
  def executionDigestService: ExecutionDigestService[F]

  /** The `ExecutionEventService`. */
  def executionEventService: ExecutionEventService[F]

  /** The `ExposureTimeModeService`. */
  def exposureTimeModeService: ExposureTimeModeService[F]

  /** The `GeneratorParamsService`. */
  def generatorParamsService: GeneratorParamsService[F]

  /** The `GmosLongSlitService`. */
  def gmosLongSlitService: GmosLongSlitService[F]

  /** The `GmosImagingService`. */
  def gmosImagingService: GmosImagingService[F]

  /** The `Flamingos2LongSlitService`. */
  def flamingos2LongSlitService: Flamingos2LongSlitService[F]

  /** The `Flamingos2SequenceService` */
  def flamingos2SequenceService: Flamingos2SequenceService[F]

  /** The `GmosSequenceService` */
  def gmosSequenceService: GmosSequenceService[F]

  /** The `GroupService`. */
  def groupService: GroupService[F]

  /** The `ObsAttachmentAssignmentService`. */
  def obsAttachmentAssignmentService: ObsAttachmentAssignmentService[F]

  /** The `ObservationService`. */
  def observationService: ObservationService[F]

  /** The `ObservationService`. */
  def observationWorkflowService: ObservationWorkflowService[F]

  /** The `ObservingModeServices`. */
  def observingModeServices: ObservingModeServices[F]

  /** The `OffsetGeneratorService`. */
  def offsetGeneratorService: OffsetGeneratorService[F]

  /** The `PartnerSplitsService`. */
  def partnerSplitsService: PartnerSplitsService[F]

  /** The `ProgramNoteService`. */
  def programNoteService: ProgramNoteService[F]

  /** The `ProgramService`. */
  def programService: ProgramService[F]

  /** The `ProgramUserService`. */
  def programUserService: ProgramUserService[F]

  /** The `ProposalService`. */
  def proposalService: ProposalService[F]

  /** The `SmartGcalService`. */
  def smartGcalService: SmartGcalService[F]

  /** The `Sequence`. */
  def sequenceService: SequenceService[F]

  /** The `TargetService`. */
  def targetService: TargetService[F]

  /** The `TimeService`. */
  def timeService: TimeService[F]

  /** The `TimingWindowService`. */
  def timingWindowService: TimingWindowService[F]

  /** The `VisitService` */
  def visitService: VisitService[F]

  /** Construct an `ItcService`.*/
  def itcService: ItcService[F]

  /** Construct a `Generator`, given a `CommitHash`.*/
  def generator: Generator[F]

  def obscalcService: ObscalcService[F]

  /** Construct a `TelluricTargetsService`, given a `TelluricTargetsClient`. */
  def telluricTargetsService: TelluricTargetsService[F]

  /** The `TimeAccounting` service. */
  def timeAccountingService: TimeAccountingService[F]

  /** Construct a `TimeEstimateService`, given a `CommitHash`.*/
  def timeEstimateService: TimeEstimateService[F]

  def trackingService: TrackingService[F]

  /** Construct a `guideService`, given a `CommitHash` and a `TimeEstimateCalculator`. */
  def guideService: GuideService[F]

  /** The `UserInvitationService` */
  def userInvitationService: UserInvitationService[F]

  def emailService: EmailService[F]

object Services:

  // Type-level representation of user access. Read <: here as "implies".
  opaque type GuestAccess                    = Unit
  opaque type PiAccess        <: GuestAccess = Unit
  opaque type NgoAccess       <: PiAccess    = Unit
  opaque type StaffAccess     <: NgoAccess   = Unit
  opaque type AdminAccess     <: StaffAccess = Unit
  opaque type ServiceAccess   <: AdminAccess = Unit
  opaque type SuperUserAccess <: ServiceAccess = Unit

  // You're always at least a guest
  given GuestAccess = ()

  def asSuperUser[A](f: SuperUserAccess ?=> A): A =
    f(using ())

  /**
   * Construct a `Services` for the given `User` and `Session`. Service instances are constructed
   * lazily.
   */
  def forUser[F[_]](
    user0: User,
    enums0: Enums,
    mapping0: Option[Session[F] => Mapping[F]],
    emailConfig0: Config.Email,
    commitHash0: CommitHash,
    tc0: TimeEstimateCalculatorImplementation.ForInstrumentMode,
    httpClient0: Client[F],
    itcClient0: ItcClient[F],
    gaiaClient0: GaiaClient[F],
    s3FileService0: S3FileService[F],
    horizonsClient: HorizonsClient[F],
    telluricClient0: TelluricTargetsClient[F],
  )(s: Session[F])(
    using tf: Trace[F], uf: UUIDGen[F], cf: Temporal[F], par: Parallel[F], log: Logger[F], lf: LoggerFactory[F], as: Async[F]
  ): Services[F[_]] =
    new Services[F]:

      val user = user0
      val session = s
      val enums = enums0
      val emailConfig = emailConfig0
      val httpClient = httpClient0
      val itcClient = itcClient0
      val gaiaClient = gaiaClient0
      val s3FileService = s3FileService0
      val commitHash = commitHash0
      val tc = tc0

      given Services[F] = this // need is for `instantiate` calls below

      private val graphQlService: Result[GraphQLService[F]] =
        mapping0 match
          case None => Result.internalError("No GraphQL Mapping available for this Services instance.")
          case Some(f) => Result(GraphQLService(f(session)))

      def runGraphQLQueryImpl(query: String, op: Option[String], vars: Option[JsonObject]): ResultT[F, Json] =
        for
          svc  <- ResultT(graphQlService.pure[F])
          op   <- ResultT(svc.parse(query, op, vars).pure[F])
          json <- ResultT(svc.query(op))
        yield json

      def runGraphQLQuery(query: String, op: Option[String], vars: Option[JsonObject]): F[Result[Json]] =
        runGraphQLQueryImpl(query, op, vars).value

      def transactionally[A](fa: (Transaction[F], Services[F]) ?=> F[A])(
        using NoTransaction[F]
      ): F[A] =
        session.transaction.use { xa =>
          session.prepareR(sql"select set_config('lucuma.user', $text, true)".query(text)).use { ps =>
            ps.unique(Gid[User.Id].fromString.reverseGet(user0.id))
          } >>
          fa(using xa)
        }

      def transactionallyT[A](fa: (Transaction[F], Services[F]) ?=> ResultT[F, A])(
        using NoTransaction[F]
      ): ResultT[F, A] =
        ResultT(transactionally { val x = fa; x.value })


      // Services as passed their "owning" `Services` (i.e., `this`) on instantiation, which is
      // circular and requires everything to be done lazily, which luckily is what we want. No point
      // instantiating anything we're not using.
      lazy val allocationService = AllocationService.instantiate
      lazy val asterismService = AsterismService.instantiate
      lazy val attachmentMetadataService = AttachmentMetadataService.instantiate
      lazy val blindOffsetsService = BlindOffsetsService.instantiate
      lazy val callForProposalsService = CallForProposalsService.instantiate
      lazy val chronicleService = ChronicleService.instantiate
      lazy val configurationService = ConfigurationService.instantiate
      lazy val datasetService = DatasetService.instantiate
      lazy val executionDigestService = ExecutionDigestService.instantiate
      lazy val executionEventService = ExecutionEventService.instantiate
      lazy val exposureTimeModeService = ExposureTimeModeService.instantiate
      lazy val generatorParamsService = GeneratorParamsService.instantiate
      lazy val flamingos2LongSlitService = Flamingos2LongSlitService.instantiate
      lazy val flamingos2SequenceService = Flamingos2SequenceService.instantiate
      lazy val gmosLongSlitService = GmosLongSlitService.instantiate
      lazy val gmosImagingService = GmosImagingService.instantiate
      lazy val gmosSequenceService = GmosSequenceService.instantiate
      lazy val obsAttachmentAssignmentService = ObsAttachmentAssignmentService.instantiate
      lazy val observationService = ObservationService.instantiate
      lazy val observingModeServices = ObservingModeServices.instantiate
      lazy val offsetGeneratorService = OffsetGeneratorService.instantiate
      lazy val partnerSplitsService = PartnerSplitsService.instantiate
      lazy val programNoteService = ProgramNoteService.instantiate
      lazy val programUserService = ProgramUserService.instantiate
      lazy val smartGcalService = SmartGcalService.instantiate
      lazy val sequenceService = SequenceService.instantiate
      lazy val targetService = TargetService.instantiate
      lazy val timeAccountingService = TimeAccountingService.instantiate
      lazy val timeService = TimeService.instantiate
      lazy val timingWindowService = TimingWindowService.instantiate
      lazy val visitService = VisitService.instantiate
      lazy val obscalcService = ObscalcService.instantiate
      lazy val timeEstimateService = TimeEstimateService.instantiate
      lazy val calibrationsService = CalibrationsService.instantiate
      lazy val groupService = GroupService.instantiate
      lazy val programService = ProgramService.instantiate
      lazy val observationWorkflowService = ObservationWorkflowService.instantiate

      // A few services require additional arguments for instantiation that may not always be
      // available, so we require them here instead of demanding them before constructing a
      // `Services` instance.
      lazy val attachmentFileService = AttachmentFileService.instantiate(s3FileService)
      lazy val itcService = ItcService.instantiate(itcClient)
      lazy val generator = Generator.instantiate(commitHash, tc)
      lazy val guideService = GuideService.instantiate(gaiaClient)
      lazy val emailService = EmailService.fromConfigAndClient(emailConfig, httpClient)
      lazy val proposalService = ProposalService.instantiate(emailConfig)
      lazy val userInvitationService = UserInvitationService.instantiate(emailConfig)
      lazy val trackingService = TrackingService.instantiate(horizonsClient)
      lazy val telluricTargetsService: TelluricTargetsService[F] = TelluricTargetsService.instantiate(telluricClient0)

  /**
   * This adds syntax to access the members of `Services` and the current `Transaction` when they
   * are present implicitly.
   */
  object Syntax:
    def services[F[_]](using Services[F]): Services[F] = summon
    def transaction[F[_]](using Transaction[F]): Transaction[F] = summon
    def session[F[_]](using Services[F]): Session[F] = summon[Services[F]].session
    def user[F[_]](using Services[F]): User = summon[Services[F]].user
    def allocationService[F[_]](using Services[F]): AllocationService[F] = summon[Services[F]].allocationService
    def asterismService[F[_]](using Services[F]): AsterismService[F] = summon[Services[F]].asterismService
    def attachmentFileService[F[_]](using Services[F]): AttachmentFileService[F] = summon[Services[F]].attachmentFileService
    def attachmentMetadataService[F[_]](using Services[F]): AttachmentMetadataService[F] = summon[Services[F]].attachmentMetadataService
    def blindOffsetsService[F[_]](using Services[F]): BlindOffsetsService[F] = summon[Services[F]].blindOffsetsService
    def calibrationsService[F[_]](using Services[F]): CalibrationsService[F] = summon[Services[F]].calibrationsService
    def callForProposalsService[F[_]](using Services[F]): CallForProposalsService[F] = summon[Services[F]].callForProposalsService
    def chronicleService[F[_]](using Services[F]): ChronicleService[F] = summon[Services[F]].chronicleService
    def configurationService[F[_]](using Services[F]): ConfigurationService[F] = summon[Services[F]].configurationService
    def datasetService[F[_]](using Services[F]): DatasetService[F] = summon[Services[F]].datasetService
    def executionDigestService[F[_]](using Services[F]): ExecutionDigestService[F] = summon[Services[F]].executionDigestService
    def executionEventService[F[_]](using Services[F]): ExecutionEventService[F] = summon[Services[F]].executionEventService
    def exposureTimeModeService[F[_]](using Services[F]): ExposureTimeModeService[F] = summon[Services[F]].exposureTimeModeService
    def generatorParamsService[F[_]](using Services[F]): GeneratorParamsService[F] = summon[Services[F]].generatorParamsService
    def gmosLongSlitService[F[_]](using Services[F]): GmosLongSlitService[F] = summon[Services[F]].gmosLongSlitService
    def gmosImagingService[F[_]](using Services[F]): GmosImagingService[F] = summon[Services[F]].gmosImagingService
    def flamingos2LongSlitService[F[_]](using Services[F]): Flamingos2LongSlitService[F] = summon[Services[F]].flamingos2LongSlitService
    def flamingos2SequenceService[F[_]](using Services[F]): Flamingos2SequenceService[F] = summon[Services[F]].flamingos2SequenceService
    def gmosSequenceService[F[_]](using Services[F]): GmosSequenceService[F] = summon[Services[F]].gmosSequenceService
    def groupService[F[_]](using Services[F]): GroupService[F] = summon[Services[F]].groupService
    def obsAttachmentAssignmentService[F[_]](using Services[F]): ObsAttachmentAssignmentService[F] = summon[Services[F]].obsAttachmentAssignmentService
    def observationService[F[_]](using Services[F]): ObservationService[F] = summon[Services[F]].observationService
    def observationWorkflowService[F[_]](using Services[F]): ObservationWorkflowService[F] = summon[Services[F]].observationWorkflowService
    def observingModeServices[F[_]](using Services[F]): ObservingModeServices[F] = summon[Services[F]].observingModeServices
    def offsetGeneratorService[F[_]](using Services[F]): OffsetGeneratorService[F] = summon[Services[F]].offsetGeneratorService
    def partnerSplitsService[F[_]](using Services[F]): PartnerSplitsService[F] = summon[Services[F]].partnerSplitsService
    def programNoteService[F[_]](using Services[F]): ProgramNoteService[F] = summon[Services[F]].programNoteService
    def programService[F[_]](using Services[F]): ProgramService[F] = summon[Services[F]].programService
    def programUserService[F[_]](using Services[F]): ProgramUserService[F] = summon[Services[F]].programUserService
    def proposalService[F[_]](using Services[F]): ProposalService[F] = summon[Services[F]].proposalService
    def smartGcalService[F[_]](using Services[F]): SmartGcalService[F] = summon[Services[F]].smartGcalService
    def sequenceService[F[_]](using Services[F]): SequenceService[F] = summon[Services[F]].sequenceService
    def targetService[F[_]](using Services[F]): TargetService[F] = summon[Services[F]].targetService
    def trackingService[F[_]](using Services[F]): TrackingService[F] = summon[Services[F]].trackingService
    def timeAccountingService[F[_]](using Services[F]): TimeAccountingService[F] = summon[Services[F]].timeAccountingService
    def timeService[F[_]](using Services[F]): TimeService[F] = summon[Services[F]].timeService
    def timingWindowService[F[_]](using Services[F]): TimingWindowService[F] = summon[Services[F]].timingWindowService
    def visitService[F[_]](using Services[F]): VisitService[F] = summon[Services[F]].visitService
    def itcService[F[_]](using Services[F]): ItcService[F] = summon[Services[F]].itcService
    def generator[F[_]](using Services[F]): Generator[F] = summon[Services[F]].generator
    def obscalcService[F[_]](using Services[F]): ObscalcService[F] = summon[Services[F]].obscalcService
    def timeEstimateService[F[_]](using Services[F]): TimeEstimateService[F] = summon[Services[F]].timeEstimateService
    def guideService[F[_]](using Services[F]): GuideService[F] = summon[Services[F]].guideService
    def userInvitationService[F[_]](using Services[F]): UserInvitationService[F] = summon[Services[F]].userInvitationService
    def emailService[F[_]](using Services[F]) = summon[Services[F]].emailService
    def telluricTargetsService[F[_]](using Services[F]): TelluricTargetsService[F] = summon[Services[F]].telluricTargetsService
    def metadata[F[_]](using Services[F]) = summon[Services[F]].metadata

    def requirePiAccess[F[_], A](fa: Services.PiAccess ?=> F[Result[A]])(using Services[F], Applicative[F]): F[Result[A]] =
      if user.role.access >= Access.Pi then fa(using ())
      else OdbError.NotAuthorized(user.id).asFailureF

    def requireNgoAccess[F[_], A](fa: Services.NgoAccess ?=> F[Result[A]])(using Services[F], Applicative[F]): F[Result[A]] =
      if user.role.access >= Access.Ngo then fa(using ())
      else OdbError.NotAuthorized(user.id).asFailureF

    def requireStaffAccess[F[_], A](fa: Services.StaffAccess ?=> F[Result[A]])(using Services[F], Applicative[F]): F[Result[A]] =
      if user.role.access >= Access.Staff then fa(using ())
      else OdbError.NotAuthorized(user.id).asFailureF

    def requireServiceAccess[F[_], A](fa: Services.ServiceAccess ?=> F[Result[A]])(using Services[F], Applicative[F]): F[Result[A]] =
      if user.role.access >= Access.Service then fa(using ())
      else OdbError.NotAuthorized(user.id).asFailureF

    def requireServiceAccessOrThrow[F[_], A](fa: Services.ServiceAccess ?=> F[A])(using Services[F], MonadError[F, Throwable]): F[A] =
      requireServiceAccess(fa.map(Result.success)).flatMap: r =>
        ApplicativeError.liftFromOption(r.toOption, new RuntimeException(s"ServiceAccess required, ${user.id} not authorized."))

    // In order to actually use this as an Enumerated, you'll probably have to assign it to a val in
    // the service in which you want to use it. Like:
    //   'val enumVal = services.enums' or `val enumVal = enums`.
    // This is because you need a stable identifier in order to do anything like
    //   `Enumerated[enumVal.ProposalStatus]`
    // Alternatively, you could assign a variable to the implicit Services in the service instantiation
    // method, like
    //   `(using theSvcs: Services[F])`
    // and then use
    //   `Enumerated[theSvcs.enums.ProposalStatus]`
    // You just need to be consistent within a service.
    def enums[F[_]](using Services[F]): Enums = summon[Services[F]].enums

    extension [F[_]: MonadCancelThrow, A](s: Resource[F, Services[F]])

      def useTransactionally(fa: (Transaction[F], Services[F]) ?=> F[A])(
        using NoTransaction[F], NotGiven[Services[F]] // discourage nested calls
      ): F[A] =
          s.use(_.transactionally(fa))

      def useNonTransactionally(fa: (NoTransaction[F], Services[F]) ?=> F[A])(
        using NoTransaction[F], NotGiven[Services[F]] // discourage nested calls
      ): F[A] =
          s.use(s => fa(using summon, s))
