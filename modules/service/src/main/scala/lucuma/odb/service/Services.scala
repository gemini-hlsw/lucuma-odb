// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.Applicative
import cats.ApplicativeError
import cats.MonadError
import cats.Parallel
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
import lucuma.core.model.Access
import lucuma.core.model.User
import lucuma.core.util.Gid
import lucuma.graphql.routes.GraphQLService
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

  /** Construct an `AttachmentFileService`, given an `S3FileService`.  */
  def attachmentFileService(s3: S3FileService[F]): AttachmentFileService[F]

  /** The `AttachmentMetadataService`. */
  def attachmentMetadataService: AttachmentMetadataService[F]

  /** The `BlindOffsetsService`. */
  def blindOffsetsService: BlindOffsetsService[F]

  /** The `CalibrationsService`. */
  def calibrationsService(emailConfig: Config.Email, httpClient: Client[F]): CalibrationsService[F]

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
  def groupService(emailConfig: Config.Email, httpClient: Client[F]): GroupService[F]

  /** The `ObsAttachmentAssignmentService`. */
  def obsAttachmentAssignmentService: ObsAttachmentAssignmentService[F]

  /** The `ObservationService`. */
  def observationService: ObservationService[F]

  /** The `ObservationService`. */
  def observationWorkflowService(httpClient: Client[F]): ObservationWorkflowService[F]

  /** The `ObservingModeServices`. */
  def observingModeServices: ObservingModeServices[F]

  /** The `PartnerSplitsService`. */
  def partnerSplitsService: PartnerSplitsService[F]

  /** The `ProgramNoteService`. */
  def programNoteService: ProgramNoteService[F]

  /** The `ProgramService`. */
  def programService(emailConfig: Config.Email, httpClient: Client[F]): ProgramService[F]

  /** The `ProgramUserService`. */
  def programUserService: ProgramUserService[F]

  /** The `ProposalService`. */
  def proposalService(emailConfig: Config.Email, httpClient: Client[F]): ProposalService[F]

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

  /** Construct an `ItcService`, given an `ItcClient`.*/
  def itcService(itcClient: ItcClient[F]): ItcService[F]

  /** Construct a `Generator`, given a `CommitHash` and an `ItcClient`.*/
  def generator(commitHash: CommitHash, itcClient: ItcClient[F], ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode): Generator[F]

  def obscalcService(commitHash: CommitHash, itcClient: ItcClient[F], ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode, httpClient: Client[F])(using Logger[F]): ObscalcService[F]

  /** The `TimeAccounting` service. */
  def timeAccountingService: TimeAccountingService[F]

  /** Construct a `TimeEstimateService`, given a `CommitHash` and an `ItcClient`.*/
  def timeEstimateService(commitHash: CommitHash, itcClient: ItcClient[F], ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode, emailConfig: Config.Email, httpClient: Client[F]): TimeEstimateService[F]

  def trackingService(httpClient: Client[F]): TrackingService[F]

  /** Construct a `guideService`, given an http4s `Client`, an `ItcClient`, a `CommitHash` and a `TimeEstimateCalculator`. */
  def guideService(gaiaClient: GaiaClient[F], itcClient: ItcClient[F], commitHash: CommitHash, ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode, httpClient: Client[F]): GuideService[F]

  /** The `UserInvitationService` */
  def userInvitationService(emailConfig: Config.Email, httpClient: Client[F]): UserInvitationService[F]

  def emailService(emailConfig: Config.Email, httpClient: Client[F]): EmailService[F]

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
  def forUser[F[_]](u: User, e: Enums, m: Option[Session[F] => Mapping[F]])(s: Session[F])(
    using tf: Trace[F], uf: UUIDGen[F], cf: Temporal[F], par: Parallel[F], log: Logger[F]
  ): Services[F[_]] =
    new Services[F]:

      val user = u
      val session = s
      val enums = e

      given Services[F] = this // need is for `instantiate` calls below

      private val graphQlService: Result[GraphQLService[F]] =
        m match
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
            ps.unique(Gid[User.Id].fromString.reverseGet(u.id))
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

      // A few services require additional arguments for instantiation that may not always be
      // available, so we require them here instead of demanding them before constructing a
      // `Services` instance.
      def attachmentFileService(s3: S3FileService[F]) = AttachmentFileService.instantiate(s3)
      def itcService(itcClient: ItcClient[F]) = ItcService.instantiate(itcClient)
      def generator(commitHash: CommitHash, itcClient: ItcClient[F], ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode) = Generator.instantiate(commitHash, itcClient, ptc)
      def obscalcService(commitHash: CommitHash, itcClient: ItcClient[F], ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode, httpClient: Client[F])(using Logger[F]) = ObscalcService.instantiate(commitHash, itcClient, ptc, httpClient)
      def timeEstimateService(commitHash: CommitHash, itcClient: ItcClient[F], ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode, emailConfig: Config.Email, httpClient: Client[F]) = TimeEstimateService.instantiate(commitHash, itcClient, ptc, emailConfig, httpClient)
      def guideService(gaiaClient: GaiaClient[F], itcClient: ItcClient[F], commitHash: CommitHash, ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode, httpClient: Client[F]) = GuideService.instantiate(gaiaClient, itcClient, commitHash, ptc, httpClient)
      def calibrationsService(emailConfig: Config.Email, httpClient: Client[F]) = CalibrationsService.instantiate(emailConfig, httpClient)
      def emailService(emailConfig: Config.Email, httpClient: Client[F]) = EmailService.fromConfigAndClient(emailConfig, httpClient)
      def groupService(emailConfig: Config.Email, httpClient: Client[F]) = GroupService.instantiate(emailConfig, httpClient)
      def programService(emailConfig: Config.Email, httpClient: Client[F]) = ProgramService.instantiate(emailConfig, httpClient)
      def proposalService(emailConfig: Config.Email, httpClient: Client[F]) = ProposalService.instantiate(emailConfig, httpClient)
      def userInvitationService(emailConfig: Config.Email, httpClient: Client[F]) = UserInvitationService.instantiate(emailConfig, httpClient)
      def trackingService(httpClient: Client[F]) = TrackingService.instantiate(httpClient)
      def observationWorkflowService(httpClient: Client[F]) = ObservationWorkflowService.instantiate(httpClient)


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
    def attachmentFileService[F[_]](s3: S3FileService[F])(using Services[F]): AttachmentFileService[F] = summon[Services[F]].attachmentFileService(s3)
    def attachmentMetadataService[F[_]](using Services[F]): AttachmentMetadataService[F] = summon[Services[F]].attachmentMetadataService
    def blindOffsetsService[F[_]](using Services[F]): BlindOffsetsService[F] = summon[Services[F]].blindOffsetsService
    def calibrationsService[F[_]](emailConfig: Config.Email, httpClient: Client[F])(using Services[F]): CalibrationsService[F] = summon[Services[F]].calibrationsService(emailConfig, httpClient)
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
    def groupService[F[_]](emailConfig: Config.Email, httpClient: Client[F])(using Services[F]): GroupService[F] = summon[Services[F]].groupService(emailConfig, httpClient)
    def obsAttachmentAssignmentService[F[_]](using Services[F]): ObsAttachmentAssignmentService[F] = summon[Services[F]].obsAttachmentAssignmentService
    def observationService[F[_]](using Services[F]): ObservationService[F] = summon[Services[F]].observationService
    def observationWorkflowService[F[_]](httpClient: Client[F])(using Services[F]): ObservationWorkflowService[F] = summon[Services[F]].observationWorkflowService(httpClient)
    def observingModeServices[F[_]](using Services[F]): ObservingModeServices[F] = summon[Services[F]].observingModeServices
    def partnerSplitsService[F[_]](using Services[F]): PartnerSplitsService[F] = summon[Services[F]].partnerSplitsService
    def programNoteService[F[_]](using Services[F]): ProgramNoteService[F] = summon[Services[F]].programNoteService
    def programService[F[_]](emailConfig: Config.Email, httpClient: Client[F])(using Services[F]): ProgramService[F] = summon[Services[F]].programService(emailConfig, httpClient)
    def programUserService[F[_]](using Services[F]): ProgramUserService[F] = summon[Services[F]].programUserService
    def proposalService[F[_]](emailConfig: Config.Email, httpClient: Client[F])(using Services[F]): ProposalService[F] = summon[Services[F]].proposalService(emailConfig, httpClient)
    def smartGcalService[F[_]](using Services[F]): SmartGcalService[F] = summon[Services[F]].smartGcalService
    def sequenceService[F[_]](using Services[F]): SequenceService[F] = summon[Services[F]].sequenceService
    def targetService[F[_]](using Services[F]): TargetService[F] = summon[Services[F]].targetService
    def trackingService[F[_]](httpClient: Client[F])(using Services[F]): TrackingService[F] = summon[Services[F]].trackingService(httpClient)
    def timeAccountingService[F[_]](using Services[F]): TimeAccountingService[F] = summon[Services[F]].timeAccountingService
    def timeService[F[_]](using Services[F]): TimeService[F] = summon[Services[F]].timeService
    def timingWindowService[F[_]](using Services[F]): TimingWindowService[F] = summon[Services[F]].timingWindowService
    def visitService[F[_]](using Services[F]): VisitService[F] = summon[Services[F]].visitService
    def itcService[F[_]](client: ItcClient[F])(using Services[F]): ItcService[F] = summon[Services[F]].itcService(client)
    def generator[F[_]](commitHash: CommitHash, itcClient: ItcClient[F], ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode)(using Services[F]): Generator[F] = summon[Services[F]].generator(commitHash, itcClient, ptc)
    def obscalcService[F[_]](commitHash: CommitHash, itcClient: ItcClient[F], ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode, httpClient: Client[F])(using Services[F], Logger[F]): ObscalcService[F] = summon[Services[F]].obscalcService(commitHash, itcClient, ptc, httpClient)
    def timeEstimateService[F[_]](commitHash: CommitHash, itcClient: ItcClient[F], ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode, emailConfig: Config.Email, httpClient: Client[F])(using Services[F]): TimeEstimateService[F] = summon[Services[F]].timeEstimateService(commitHash, itcClient, ptc, emailConfig, httpClient)
    def guideService[F[_]](gaiaClient: GaiaClient[F], itcClient: ItcClient[F], commitHash: CommitHash, ptc: TimeEstimateCalculatorImplementation.ForInstrumentMode, httpClient: Client[F])(using Services[F]): GuideService[F] = summon[Services[F]].guideService(gaiaClient, itcClient, commitHash, ptc, httpClient)
    def userInvitationService[F[_]](emailConfig: Config.Email, httpClient: Client[F])(using Services[F]): UserInvitationService[F] = summon[Services[F]].userInvitationService(emailConfig, httpClient)
    def emailService[F[_]](emailConfig: Config.Email, httpClient: Client[F])(using Services[F]) = summon[Services[F]].emailService(emailConfig, httpClient)

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
