// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package input

import cats.Eq
import cats.syntax.order.*
import cats.syntax.parallel.*
import cats.syntax.traverse.*
import grackle.Result
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.ExposureTimeMode
import lucuma.core.model.SlitTelescopeConfigs
import lucuma.core.model.TelluricType
import lucuma.core.model.sequence.TelescopeConfig
import lucuma.core.model.sequence.igrins2.MaxExposureTime
import lucuma.core.model.sequence.igrins2.SvcMinExposureTime
import lucuma.core.util.TimeSpan
import lucuma.odb.data.Nullable
import lucuma.odb.format.telescopeConfigs.*
import lucuma.odb.graphql.binding.*

object Igrins2LongSlitInput:

  /**
   * SVC (Slit-Viewing Camera) acquisition sub-config input. Each field controls one explicit
   * override of an SVC parameter (save toggle, exposure time, telescope dither positions).
   */
  object Svc:

    private def secondsLabel(ts: TimeSpan): String =
      ts.toSeconds.bigDecimal.stripTrailingZeros.toPlainString

    /** An explicit SVC exposure must respect the slit-viewing camera limit.
      */
    private def validateExposure(ts: TimeSpan): Result[TimeSpan] =
      if ts >= SvcMinExposureTime && ts <= MaxExposureTime then Result(ts)
      else Result.failure(
        s"SVC exposure time must be between ${secondsLabel(SvcMinExposureTime)} s and ${secondsLabel(MaxExposureTime)} s."
      )

    /** Create semantics: each field is set-or-skip (`Option`). */
    case class Create(
      explicitExposure:         Option[TimeSpan],
      explicitTelescopeConfigs: Option[List[TelescopeConfig]]
    )

    object Create:
      val Binding: Matcher[Create] =
        ObjectFieldsBinding.rmap:
          case List(
            TimeSpanInput.Binding.Option("explicitExposure", rExposure),
            TelescopeConfigInput.Binding.List.Option("explicitTelescopeConfigs", rTcs)
          ) =>
            (rExposure.flatMap(_.traverse(validateExposure)), rTcs).parMapN(Create.apply)

    /** Edit semantics: each field is set-clear-skip (`Nullable`). */
    case class Edit(
      explicitExposure:         Nullable[TimeSpan],
      explicitTelescopeConfigs: Nullable[List[TelescopeConfig]]
    )

    object Edit:
      val Binding: Matcher[Edit] =
        ObjectFieldsBinding.rmap:
          case List(
            TimeSpanInput.Binding.Nullable("explicitExposure", rExposure),
            TelescopeConfigInput.Binding.List.Nullable("explicitTelescopeConfigs", rTcs)
          ) =>
            (rExposure.flatMap(_.traverse(validateExposure)), rTcs).parMapN(Edit.apply)

  case class Create(
    exposureTimeMode: Option[ExposureTimeMode],
    svc:              Option[Svc.Create] = None,
    explicitTelescopeConfigs: Option[SlitTelescopeConfigs] = None,
    telluricType: TelluricType = TelluricType.Hot
  ):
    def observingModeType: ObservingModeType =
      ObservingModeType.Igrins2LongSlit

    private val stored = explicitTelescopeConfigs.map(storedSlitTelescopeConfigs)

    val explicitSlitOffsetMode = stored.map(_.slitOffsetMode)

    val formattedTelescopeConfigs = stored.map(_.telescopeConfigs)

  object Create:

    val Binding: Matcher[Create] =
      ObjectFieldsBinding.rmap {
        case List(
          ExposureTimeModeInput.Binding.Nullable("exposureTimeMode", rETM),
          Svc.Create.Binding.Option("svc", rSvc),
          SlitTelescopeConfigsInput.Binding.Option("explicitTelescopeConfigs", rTelescopeConfigs),
          TelluricTypeBinding.Option("telluricType", rTelluricType)
        ) =>
          (rETM.flatMap(TelluricExposureTimeModeEdit.forCreate), rSvc, rTelescopeConfigs, rTelluricType).parMapN { (etm, svc, telescopeConfigs, telluricType) =>
            Create(etm, svc, telescopeConfigs, telluricType.getOrElse(TelluricType.Hot))
          }
      }

  case class Edit(
    exposureTimeMode: Nullable[ExposureTimeMode],
    svc:              Nullable[Svc.Edit],
    explicitTelescopeConfigs: Nullable[SlitTelescopeConfigs],
    telluricType: Option[TelluricType]
  ):

    val observingModeType: ObservingModeType =
      ObservingModeType.Igrins2LongSlit

    def isScienceExposureTimeModeOnly: Boolean =
      exposureTimeMode.isDefined &&
        copy(exposureTimeMode = Nullable.Absent) === Edit.AllUndefined

    private val stored = explicitTelescopeConfigs.map(storedSlitTelescopeConfigs)

    val explicitSlitOffsetMode = stored.map(_.slitOffsetMode)

    val formattedTelescopeConfigs = stored.map(_.telescopeConfigs)

    val toCreate: Result[Create] =
      TelluricExposureTimeModeEdit.forCreate(exposureTimeMode).map: etm =>
        Create(
          etm,
          svc.toOption.map: s =>
            Svc.Create(
              s.explicitExposure.toOption,
              s.explicitTelescopeConfigs.toOption
            ),
          explicitTelescopeConfigs.toOption,
          telluricType.getOrElse(TelluricType.Hot)
        )

  object Edit:
    given Eq[Edit] = Eq.fromUniversalEquals

    private val AllUndefined: Edit =
      Edit(Nullable.Absent, Nullable.Absent, Nullable.Absent, None)

    val Binding: Matcher[Edit] =
      ObjectFieldsBinding.rmap {
        case List(
          ExposureTimeModeInput.Binding.Nullable("exposureTimeMode", rETM),
          Svc.Edit.Binding.Nullable("svc", rSvc),
          SlitTelescopeConfigsInput.Binding.Nullable("explicitTelescopeConfigs", rTelescopeConfigs),
          TelluricTypeBinding.Option("telluricType", rTelluricType)
        ) =>
          (rETM, rSvc, rTelescopeConfigs, rTelluricType).parMapN(Edit.apply)
      }
