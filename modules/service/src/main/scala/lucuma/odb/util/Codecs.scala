// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.util

import cats.data.NonEmptyList
import cats.syntax.apply.*
import cats.syntax.either.*
import cats.syntax.option.*
import eu.timepit.refined.types.numeric.NonNegBigDecimal
import eu.timepit.refined.types.numeric.NonNegInt
import eu.timepit.refined.types.numeric.NonNegLong
import eu.timepit.refined.types.numeric.NonNegShort
import eu.timepit.refined.types.numeric.PosBigDecimal
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.types.numeric.PosLong
import eu.timepit.refined.types.numeric.PosShort
import eu.timepit.refined.types.string.NonEmptyString
import lucuma.ags.GuideStarName
import lucuma.core.data.EmailAddress
import lucuma.core.enums.*
import lucuma.core.enums.CalibrationRole
import lucuma.core.enums.CallForProposalsType
import lucuma.core.enums.EmailStatus
import lucuma.core.enums.ObservingModeType
import lucuma.core.enums.PartnerLinkType
import lucuma.core.enums.ProgramUserRole
import lucuma.core.enums.ScienceBand
import lucuma.core.enums.TimeAccountingCategory
import lucuma.core.math.Angle
import lucuma.core.math.BoundedInterval
import lucuma.core.math.Declination
import lucuma.core.math.Epoch
import lucuma.core.math.Offset
import lucuma.core.math.Parallax
import lucuma.core.math.RadialVelocity
import lucuma.core.math.RightAscension
import lucuma.core.math.SignalToNoise
import lucuma.core.math.Wavelength
import lucuma.core.model.*
import lucuma.core.model.ConfigurationRequest
import lucuma.core.model.ElevationRange.AirMass
import lucuma.core.model.ElevationRange.HourAngle
import lucuma.core.model.PartnerLink
import lucuma.core.model.ProgramUser
import lucuma.core.model.UserInvitation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.CategorizedTime
import lucuma.core.model.sequence.Dataset
import lucuma.core.model.sequence.DatasetReference
import lucuma.core.model.sequence.Step
import lucuma.core.model.sequence.StepConfig
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.TimeChargeCorrection
import lucuma.core.util.DateInterval
import lucuma.core.util.Enumerated
import lucuma.core.util.Gid
import lucuma.core.util.TimeSpan
import lucuma.core.util.Timestamp
import lucuma.core.util.TimestampInterval
import lucuma.core.util.Uid
import lucuma.odb.data.AtomExecutionState
import lucuma.odb.data.EditType
import lucuma.odb.data.EmailId
import lucuma.odb.data.ExecutionEventType
import lucuma.odb.data.Existence
import lucuma.odb.data.Extinction
import lucuma.odb.data.Md5Hash
import lucuma.odb.data.PosAngleConstraintMode
import lucuma.odb.data.StepExecutionState
import lucuma.odb.data.Tag
import lucuma.odb.data.TimeCharge.DiscountDiscriminator
import lucuma.odb.data.TimingWindowEndTypeEnum
import lucuma.odb.data.UserType
import lucuma.odb.service.ObservationWorkflowService
import monocle.Prism
import skunk.*
import skunk.codec.all.*
import skunk.data.Arr
import skunk.data.Type
import spire.math.interval.Closed
import spire.math.interval.Open
import spire.math.interval.ValueBound

import scala.util.control.Exception
import scala.util.matching.Regex


// Codecs for some atomic types.
trait Codecs {

  def enumerated[A](tpe: Type)(implicit ev: Enumerated[A]): Codec[A] =
    `enum`(ev.tag, ev.fromTag, tpe)

  /** Codec for an array of an enumerated type. */
  def _enumerated[A](tpe: Type)(implicit ev: Enumerated[A]): Codec[List[A]] =
    Codec
      .array[A](ev.tag, s => ev.fromTag(s).toRight(s"${tpe.name}: no such element '$s'"), tpe)
      .imap[List[A]](_.flattenTo(List))(Arr.fromFoldable)

  private def codecFromPrism[A](prism: Prism[String, A], tpe: Type): Codec[A] =
    Codec.simple(
      prism.reverseGet,
      s => prism.getOption(s).toRight(s"Invalid: $s"),
      tpe
    )

  def gid[A](implicit ev: Gid[A]): Codec[A] =
    codecFromPrism(ev.fromString, Type.varchar)

  def uid[A](implicit ev: Uid[A]): Codec[A] =
    codecFromPrism(ev.fromString, Type.varchar)

  final case class DomainCodec[A](domainName: String, codec: Codec[A]) extends Codec[A]:
    export codec.* // delegate everything!

  extension [A](c: Codec[A]) def withDomain(name: String): Codec[A] =
    DomainCodec(name, c)

  val int4range: Codec[BoundedInterval[Int]] = {
    val intPair             = raw"([+-]?\d+),([+-]?\d+)"
    val OpenOpen: Regex     = raw"\($intPair\)".r
    val OpenClosed: Regex   = raw"\($intPair\]".r
    val ClosedOpen: Regex   = raw"\[$intPair\)".r
    val ClosedClosed: Regex = raw"\[$intPair\]".r

    Codec.simple(
      { bi =>
        val lower = bi.lowerBound match {
          case Open(low)   => s"($low"
          case Closed(low) => s"[$low"
        }
        val upper = bi.upperBound match {
          case Open(hi)   => s"$hi)"
          case Closed(hi) => s"$hi]"
        }
        s"$lower,$upper"
      },
      { s =>
        def parseInt(s: String): Option[Int] =
          Exception.nonFatalCatch.opt(s.toInt)

        def mk(l: String, h: String, f: (Int, Int) => Option[BoundedInterval[Int]]): Option[BoundedInterval[Int]] =
          (parseInt(l), parseInt(h)).flatMapN(f)

        val bi = s match {
          case OpenOpen(l, h)     => mk(l, h, BoundedInterval.open)
          case OpenClosed(l, h)   => mk(l, h, BoundedInterval.openLower)
          case ClosedOpen(l, h)   => mk(l, h, BoundedInterval.openUpper)
          case ClosedClosed(l, h) => mk(l, h, BoundedInterval.closed)
          case _                  => none
        }
        bi.toRight(s"Invalid BoundedInterval: $s")
      },
      Type.int4range
    )
  }

  val air_mass_range_value: Codec[PosBigDecimal] =
    numeric(3, 2).eimap(PosBigDecimal.from)(_.value)

  val air_mass_range: Codec[AirMass] =
    (air_mass_range_value ~ air_mass_range_value).eimap { case (min, max) =>
      for {
        n <- AirMass.DecimalValue.from(min.value)
        x <- AirMass.DecimalValue.from(max.value)
        a <- AirMass.fromOrderedDecimalValues.getOption((n, x)).toRight(s"air mass min and max out of order: ($min, $max)")
      } yield a
    } { a =>
      (PosBigDecimal.unsafeFrom(a.min.value), PosBigDecimal.unsafeFrom(a.max.value))
    }

  val angle_µas: Codec[Angle] =
    int8.imap(Angle.microarcseconds.reverseGet)(Angle.microarcseconds.get)

  val _angle_µas: Codec[Arr[Angle]] =
    Codec.array(
      a => Angle.microarcseconds.get(a).toString,
      safe(s => Angle.microarcseconds.reverseGet(s.toLong)),
      Type("_d_angle_µas", List(Type("d_angle_µas")))
    )

  val atom_execution_state: Codec[AtomExecutionState] =
    enumerated[AtomExecutionState](Type.varchar)

  val atom_id: Codec[Atom.Id] =
    uid[Atom.Id]

  val atom_stage: Codec[AtomStage] =
    enumerated(Type("e_atom_stage"))

  val attachment_id: Codec[Attachment.Id] =
    gid[Attachment.Id]

  val attachment_type: Codec[AttachmentType] =
    enumerated(Type("e_attachment_type"))

  val catalog_name: Codec[CatalogName] =
    enumerated(Type("e_catalog_name"))

  val cfp_id: Codec[CallForProposals.Id] =
    gid[CallForProposals.Id]

  val cfp_type: Codec[CallForProposalsType] =
    enumerated(Type("e_cfp_type"))

  val charge_class: Codec[ChargeClass] =
    enumerated(Type("e_charge_class"))

  val cloud_extinction: Codec[CloudExtinction] =
    enumerated[CloudExtinction](Type.varchar)

  val core_timestamp: Codec[Timestamp] =
    timestamp.eimap(
      ldt => Timestamp.FromLocalDateTime.getOption(ldt).toRight(s"Invalid Timestamp: $ldt"))(
      _.toLocalDateTime
    )

  val dataset_id: Codec[Dataset.Id] =
    gid[Dataset.Id]

  val dataset_qa_state: Codec[DatasetQaState] =
    enumerated(Type("e_dataset_qa_state"))

  val dataset_reference: Codec[DatasetReference] =
    text.eimap(
      s => DatasetReference.fromString.getOption(s).toRight(s"Invalid dataset reference: $s"))(
      DatasetReference.fromString.reverseGet
    )

  val dataset_stage: Codec[DatasetStage] =
    enumerated(Type("e_dataset_stage"))

  val date_interval: Codec[DateInterval] =
    (date *: date).imap { case (min, max) =>
      DateInterval.between(min, max)
    } { interval => (interval.start, interval.end) }

  val declination: Codec[Declination] =
    angle_µas.eimap(
      a => Declination.fromAngle.getOption(a).toRight(s"Invalid declination: $a"))(
      Declination.fromAngle.reverseGet
    )

  val edit_type: Codec[EditType] =
    enumerated(Type("e_edit_type"))

  val educational_status: Codec[EducationalStatus] =
    enumerated(Type("e_educational_status"))

  val email_address: Codec[EmailAddress] =
    codecFromPrism(EmailAddress.from, Type("citext"))

  val guide_target_name: Codec[GuideStarName] =
    codecFromPrism(GuideStarName.from, Type("text"))

  val email_id: Codec[EmailId] =
    text.eimap(EmailId.fromString)(_.value.value)

  val email_status: Codec[EmailStatus] =
    enumerated(Type("e_email_status"))

  val ephemeris_key_type: Codec[EphemerisKeyType] =
    enumerated(Type("e_ephemeris_key_type"))

  val epoch: Codec[Epoch] =
    varchar.eimap(
      s => Epoch.fromString.getOption(s).toRight(s"Invalid epoch: $s"))(
      Epoch.fromString.reverseGet
    )

  val execution_event_id: Codec[ExecutionEvent.Id] =
    gid[ExecutionEvent.Id]

  val execution_event_type: Codec[ExecutionEventType] =
    enumerated(Type("e_execution_event_type"))

  val execution_state: Codec[ExecutionState] =
    enumerated(Type("e_execution_state"))

  val existence: Codec[Existence] =
    enumerated(Type("e_existence"))

  val focal_plane: Codec[FocalPlane] =
    enumerated[FocalPlane](Type.varchar)

  val gcal_arc: Codec[GcalArc] =
    enumerated[GcalArc](Type.varchar)

  val gcal_baseline: Codec[GcalBaselineType] =
    enumerated(Type("e_gcal_baseline_type"))

  val gcal_continuum: Codec[GcalContinuum] =
    enumerated[GcalContinuum](Type.varchar)

  val gcal_diffuser: Codec[GcalDiffuser] =
    enumerated[GcalDiffuser](Type.varchar)

  val gcal_filter: Codec[GcalFilter] =
    enumerated[GcalFilter](Type.varchar)

  val gcal_shutter: Codec[GcalShutter] =
    enumerated[GcalShutter](Type.varchar)

  val gender: Codec[Gender] =
    enumerated(Type("e_gender"))

  val guide_state: Codec[StepGuideState] =
    enumerated(Type("e_guide_state"))

  val hour_angle_range_value: Codec[BigDecimal] =
    numeric(3, 2)

  val hour_angle_range: Codec[HourAngle] =
    (hour_angle_range_value ~ hour_angle_range_value).eimap { case (min, max) =>
      for {
        n <- HourAngle.DecimalHour.from(min)
        x <- HourAngle.DecimalHour.from(max)
        h <- HourAngle.fromOrderedDecimalHours.getOption((n, x)).toRight(s"hour angle min and max out of order: ($min, $max)")
      } yield h
    } { h =>
      (h.minHours.value, h.maxHours.value)
    }

  val image_quality: Codec[ImageQuality] =
    enumerated[ImageQuality](Type.varchar)

  val _instrument: Codec[Arr[Instrument]] =
    Codec.array(_.tag, s => Instrument.fromTag(s).toRight(s"Invalid Instrument tag: $s"), Type("_d_tag", List(Type("d_tag"))))

  val instrument: Codec[Instrument] =
    enumerated[Instrument](Type.varchar)

  val int_percent: Codec[IntPercent] =
    int2.eimap(n => IntPercent.from(n))(_.value.toShort)

  val md5_hash: Codec[Md5Hash] =
    bytea.eimap(b => Md5Hash.fromByteArray(b).toRight(s"Expected an MD5 hash value but found ${b.size} bytes"))(_.toByteArray)

  val mos_pre_imaging: Codec[MosPreImaging] =
    bool.imap(
      b => if (b) MosPreImaging.IsMosPreImaging else MosPreImaging.IsNotMosPreImaging
    )(_.toBoolean)

  val obs_class: Codec[ObserveClass] =
    enumerated(Type("e_obs_class"))

  val observation_id: Codec[Observation.Id] =
    gid[Observation.Id]

  val configuration_request_id: Codec[ConfigurationRequest.Id] =
    gid[ConfigurationRequest.Id]

  val configuration_request_status: Codec[ConfigurationRequestStatus] =
    enumerated(Type("e_configuration_request_status"))

  val observing_mode_type: Codec[ObservingModeType] =
    enumerated(Type("e_observing_mode_type"))

  val orcid_id: Codec[OrcidId] =
    Codec.simple[OrcidId](
      _.value.toString(),
      OrcidId.fromValue(_),
      Type.varchar
    )

  val pac_mode: Codec[PosAngleConstraintMode] =
    enumerated(Type("e_pac_mode"))

  val parallax: Codec[Parallax] =
    angle_µas.imap(
      a => Parallax.fromMicroarcseconds(a.toMicroarcseconds))(
      p => Angle.fromMicroarcseconds(p.μas.value.value)
    )

  val partner: Codec[Partner] =
    enumerated(Type.varchar)

  val numeric_nonneg: Codec[NonNegBigDecimal] =
    numeric.eimap(NonNegBigDecimal.from)(_.value)

  val numeric_pos: Codec[PosBigDecimal] =
    numeric.eimap(PosBigDecimal.from)(_.value)

  val int2_nonneg: Codec[NonNegShort] =
    int2.eimap(NonNegShort.from)(_.value)

  val int2_pos: Codec[PosShort] =
    int2.eimap(PosShort.from)(_.value)

  val int4_nonneg: Codec[NonNegInt] =
    int4.eimap(NonNegInt.from)(_.value)

  val int4_pos: Codec[PosInt] =
    int4.eimap(PosInt.from)(_.value)

  val int8_nonneg: Codec[NonNegLong] =
    int8.eimap(NonNegLong.from)(_.value)

  val int8_pos: Codec[PosLong] =
    int8.eimap(PosLong.from)(_.value)

  val program_id: Codec[Program.Id] =
    gid[Program.Id]

  val group_id: Codec[Group.Id] =
    gid[Group.Id]

  val observation_reference: Codec[ObservationReference] =
    text.eimap(
      s => ObservationReference.fromString.getOption(s).toRight(s"Invalid observation reference: $s"))(
      ObservationReference.fromString.reverseGet
    )

  val partner_link_type: Codec[PartnerLinkType] =
    enumerated(Type("e_partner_link"))

  val proposal_reference: Codec[ProposalReference] =
    text.eimap(
      s => ProposalReference.fromString.getOption(s).toRight(s"Invalid proposal reference: $s"))(
      ProposalReference.fromString.reverseGet
    )

  val program_reference: Codec[ProgramReference] =
    text.eimap(
      s => ProgramReference.fromString.getOption(s).toRight(s"Invalid program reference: $s"))(
      ProgramReference.fromString.reverseGet
    )

  val program_type: Codec[ProgramType] =
    enumerated(Type("e_program_type"))

  val program_user_id: Codec[ProgramUser.Id] =
    gid[ProgramUser.Id]

  val program_user_role: Codec[ProgramUserRole] =
    enumerated(Type("e_program_user_role"))

  val radial_velocity: Codec[RadialVelocity] =
    numeric.eimap(
      bd => RadialVelocity.kilometerspersecond.getOption(bd).toRight(s"Invalid radial velocity: $bd"))(
      RadialVelocity.kilometerspersecond.reverseGet
    )

  val right_ascension: Codec[RightAscension] =
    angle_µas.eimap(
      a => RightAscension.fromAngleExact.getOption(a).toRight(s"Invalid right ascension: $a"))(
      RightAscension.fromAngleExact.reverseGet
    )

  val science_band: Codec[ScienceBand] =
    enumerated(Type("e_science_band"))

  val science_mode: Codec[ScienceMode] =
    enumerated[ScienceMode](Type.varchar)

  val science_subtype: Codec[ScienceSubtype] =
    enumerated(Type("e_science_subtype"))

  val semester: Codec[Semester] =
    varchar.eimap(
      s => Semester.fromString.getOption(s).toRight(s"Invalid semester: $s"))(
      _.format
    )

  val sequence_command: Codec[SequenceCommand] =
    enumerated[SequenceCommand](Type("e_sequence_command"))

  val sequence_type: Codec[SequenceType] =
    enumerated[SequenceType](Type("e_sequence_type"))

  val _site: Codec[Arr[Site]] =
    Codec.array(_.tag.toLowerCase, s => Site.fromTag(s.toUpperCase).toRight(s"Invalid tag: $s"), Type("_e_site"))

  val site: Codec[Site] =
    `enum`(_.tag.toLowerCase, s => Site.fromTag(s.toUpperCase), Type("e_site"))

  val sky_background: Codec[SkyBackground] =
    enumerated[SkyBackground](Type.varchar)

  val slew_stage: Codec[SlewStage] =
    enumerated(Type("e_slew_stage"))

  val smart_gcal_type: Codec[SmartGcalType] =
    enumerated(Type("e_smart_gcal_type"))

  val spectroscopy_capabilities: Codec[SpectroscopyCapabilities] =
    enumerated[SpectroscopyCapabilities](Type.varchar)

  val signal_to_noise: Codec[SignalToNoise] =
    numeric(10,3).eimap(
     bd => SignalToNoise.FromBigDecimalExact.getOption(bd).toRight(s"Invalid signal-to-noise value: $bd")
    )(_.toBigDecimal)

  val step_execution_state: Codec[StepExecutionState] =
    enumerated[StepExecutionState](Type.varchar)

  val step_id: Codec[Step.Id] =
    uid[Step.Id]

  val step_stage: Codec[StepStage] =
    enumerated(Type("e_step_stage"))

  val step_type: Codec[StepType] =
    enumerated(Type("e_step_type"))

  val tag: Codec[Tag] =
    varchar.imap(Tag(_))(_.value)

  val target_id: Codec[Target.Id] =
    gid[Target.Id]

  val calibration_role: Codec[CalibrationRole] =
    enumerated(Type("e_calibration_role"))

  val text_nonempty: Codec[NonEmptyString] =
    text.eimap(NonEmptyString.from)(_.value)

  val varchar_nonempty: Codec[NonEmptyString] =
    varchar.eimap(NonEmptyString.from)(_.value)

  val time_accounting_category: Codec[TimeAccountingCategory] =
    enumerated(Type.varchar)

  val time_charge_correction_op: Codec[TimeChargeCorrection.Op] =
    enumerated(Type("e_time_charge_correction_op"))

  val time_charge_discount_type: Codec[DiscountDiscriminator] =
    enumerated(Type("e_time_charge_discount_type"))

  val time_span: Codec[TimeSpan] =
    interval.eimap(
      µs => TimeSpan.FromDuration.getOption(µs).toRight(s"Invalid TimeSpan, must be non-negative µs <  9,223,372,036,854,775,807 µs: $µs"))(
      TimeSpan.FromDuration.reverseGet
    )

  val timing_window_end_type: Codec[TimingWindowEndTypeEnum] =
    enumerated(Type("e_timing_window_end_type"))

  val timing_window_inclusion: Codec[TimingWindowInclusion] =
    enumerated(Type("e_timing_window_inclusion"))

  val too_activation: Codec[ToOActivation] =
    enumerated(Type("e_too_activation"))

  val user_id: Codec[User.Id] =
    gid[User.Id]

  val user_type: Codec[UserType] =
    enumerated(Type("e_user_type"))

  val visit_id: Codec[Visit.Id] =
    gid[Visit.Id]

  val water_vapor: Codec[WaterVapor] =
    enumerated[WaterVapor](Type.varchar)

  val wavelength_pm: Codec[Wavelength] =
    int4.eimap(
      pm => Wavelength.intPicometers.getOption(pm).toRight(s"Invalid wavelength, must be positive pm: $pm"))(
      Wavelength.intPicometers.reverseGet
    )

  val wavelength_pm_range: Codec[BoundedInterval[Wavelength]] = {

    def to: ValueBound[Int] => Option[ValueBound[Wavelength]] = {
      case Open(a)   => Wavelength.intPicometers.getOption(a).map(Open(_))
      case Closed(a) => Wavelength.intPicometers.getOption(a).map(Closed(_))
    }

    def from: ValueBound[Wavelength] => ValueBound[Int] = {
      case Open(a)   => Open(Wavelength.intPicometers.reverseGet(a))
      case Closed(a) => Closed(Wavelength.intPicometers.reverseGet(a))
    }

    int4range.eimap[BoundedInterval[Wavelength]](bi =>
      (to(bi.lowerBound), to(bi.upperBound)).flatMapN { case (l, h) =>
        BoundedInterval.fromBounds(l, h)
      }.toRight(s"Invalid wavelength bounds: $bi")
    )(bw =>
      BoundedInterval.unsafeFromBounds(from(bw.lowerBound), from(bw.upperBound))
    )
  }

  // Not so atomic ...

  val elevation_range: Codec[ElevationRange] =
    (air_mass_range.opt ~ hour_angle_range.opt).eimap { case (am, ha) =>
      am.orElse(ha).toRight("Undefined elevation range")
    } { e =>
      (ElevationRange.airMass.getOption(e), ElevationRange.hourAngle.getOption(e))
    }

  val categorized_time: Codec[CategorizedTime] =
    (time_span *: time_span *: time_span).imap {
      case (non_charged, partner_time, program_time) =>
        CategorizedTime(
          ChargeClass.NonCharged -> non_charged,
          ChargeClass.Partner    -> partner_time,
          ChargeClass.Program    -> program_time
        )
    } { ct =>
      (ct(ChargeClass.NonCharged), ct(ChargeClass.Partner), ct(ChargeClass.Program))
    }

  val constraint_set: Codec[ConstraintSet] =
    (image_quality    *:
     cloud_extinction *:
     sky_background   *:
     water_vapor      *:
     elevation_range
    ).to[ConstraintSet]

  val dataset_filename: Codec[Dataset.Filename] =
    (site *: date *: int4_pos).eimap { case (s, d, p) =>
      Dataset.Filename.from(s, d, p).toRight(s"Unsupported date: $d")
    } { f =>
      (f.site, f.localDate, f.index)
    }

  val offset: Codec[Offset] =
    (angle_µas *: angle_µas).imap { (p, q) =>
      Offset(Offset.P(p), Offset.Q(q))
    }(o => (o.p.toAngle, o.q.toAngle))

  val partner_link: Codec[PartnerLink] =
    (partner_link_type *: partner.opt).eimap { (l, p) =>
      l match {
        case PartnerLinkType.HasPartner            => p.toRight("Invalid data: has_partner link type without partner").map(PartnerLink.HasPartner.apply)
        case PartnerLinkType.HasNonPartner         => PartnerLink.HasNonPartner.asRight
        case PartnerLinkType.HasUnspecifiedPartner => PartnerLink.HasUnspecifiedPartner.asRight
      }
    } { pl => (pl.linkType, pl.partnerOption) }

  val step_config_gcal: Codec[StepConfig.Gcal] =
    (gcal_continuum.opt *:
     bool               *:
     bool               *:
     bool               *:
     bool               *:
     gcal_filter        *:
     gcal_diffuser      *:
     gcal_shutter
    ).eimap { case (c, ar, cuar, thar, xe, f, d, s) =>

      val arcs = List(
        GcalArc.ArArc   -> ar,
        GcalArc.CuArArc -> cuar,
        GcalArc.ThArArc -> thar,
        GcalArc.XeArc   -> xe
      ).collect { case (arc, true) => arc}

      StepConfig.Gcal.Lamp
        .fromContinuumOrArcs(c, arcs)
        .map(StepConfig.Gcal(_, f, d, s))

    } { (gcal: StepConfig.Gcal) =>
      val arcs = gcal.lamp.toArcsSortedSet
      (
        gcal.lamp.continuum   ,
        arcs(GcalArc.ArArc)   ,
        arcs(GcalArc.CuArArc) ,
        arcs(GcalArc.ThArArc) ,
        arcs(GcalArc.XeArc)   ,
        gcal.filter           ,
        gcal.diffuser         ,
        gcal.shutter
      )
    }

  val step_config_smart_gcal: Codec[StepConfig.SmartGcal] =
    smart_gcal_type.to[StepConfig.SmartGcal]

  val telescope_config: Codec[TelescopeConfig] =
    (offset *: guide_state).to[TelescopeConfig]

  val timestamp_interval: Codec[TimestampInterval] =
    (core_timestamp *: core_timestamp).imap { case (min, max) =>
      TimestampInterval.between(min, max)
    } { interval => (interval.start, interval.end) }

  val timestamp_interval_tsrange: Codec[TimestampInterval] = {
    Codec.simple(
      { ti =>
        val start = core_timestamp.encode(ti.start).head.get
        val end   = core_timestamp.encode(ti.end).head.get
        s"[$start,$end)"
      },
      { s =>
        def parseTimestamp(s: String): Option[Timestamp] =
          core_timestamp.decode(0, List(s.some)).toOption

        val StartEnd = raw"\[\"([^,]+)\",\"([^,]+)\"\)".r
        val ti = s match {
          case StartEnd(s, e) =>
            for {
              start <- parseTimestamp(s)
              end   <- parseTimestamp(e)
            } yield TimestampInterval.between(start, end)
          case _              =>
            none
        }
        ti.toRight(s"Invalid TimestampInterval: $s")
      },
      Type.tsrange
    )
  }

  val void: Codec[Unit] =
    val rightUnit = Right(())
    Codec.simple(_ => "", _ => rightUnit, Type.void)

  val extinction: Codec[Extinction] =
    int2_nonneg.imap(Extinction.apply)(_.underlying)

  val user_invitation: Codec[UserInvitation] =
    import UserInvitation.fromString
    text.eimap(
      fromString.getOption(_).toRight("Invalid invitatation"))(
      fromString.reverseGet
    )

  val user_invitation_status: Codec[InvitationStatus] =
    enumerated(Type("e_invitation_status"))

  val observation_workflow_state: Codec[ObservationWorkflowState] = 
    enumerated[ObservationWorkflowState](Type("e_workflow_user_state"))

  val user_state: Codec[ObservationWorkflowService.UserState] =
    import ObservationWorkflowService.UserState
    import ObservationWorkflowState.*
    observation_workflow_state.eimap[UserState] {
      case Inactive   => Right(Inactive)
      case Ready      => Right(Ready)
      case s          => Left(s"Invalid user state: $s")
    } (a => a: ObservationWorkflowState)

  extension [A](enc: Encoder[A]) def nel(as: NonEmptyList[A]): Encoder[as.type] =
    val lst = as.toList
    enc.list(lst).contramap(_ => lst)

  val _science_band: Codec[List[ScienceBand]] =
    _enumerated(Type("_e_science_band"))

}

object Codecs extends Codecs
