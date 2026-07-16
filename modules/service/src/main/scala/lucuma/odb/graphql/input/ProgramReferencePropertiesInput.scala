// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.option.*
import cats.syntax.parallel.*
import grackle.Result
import grackle.syntax.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ProgramType
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.SubaruCallForProposalsType
import lucuma.core.model.ProgramReference
import lucuma.core.model.Semester
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.ObjectFieldsBinding

case class ProgramReferencePropertiesInput(
  input: ProgramReferencePropertiesCalibrationInput   |
         ProgramReferencePropertiesCommissioningInput |
         ProgramReferencePropertiesEngineeringInput   |
         ProgramReferencePropertiesExampleInput       |
         ProgramReferencePropertiesKeckInput          |
         ProgramReferencePropertiesLibraryInput       |
         ProgramReferencePropertiesMonitoringInput    |
         ProgramReferencePropertiesScienceInput       |
         ProgramReferencePropertiesSubaruInput        |
         ProgramReferencePropertiesSystemInput
) {

  def programType: ProgramType =
    input match {
      case ProgramReferencePropertiesCalibrationInput(_, _)   => ProgramType.Calibration
      case ProgramReferencePropertiesCommissioningInput(_, _) => ProgramType.Commissioning
      case ProgramReferencePropertiesEngineeringInput(_, _)   => ProgramType.Engineering
      case ProgramReferencePropertiesExampleInput(_)          => ProgramType.Example
      case ProgramReferencePropertiesKeckInput(_)             => ProgramType.Keck
      case ProgramReferencePropertiesLibraryInput(_, _)       => ProgramType.Library
      case ProgramReferencePropertiesMonitoringInput(_, _)    => ProgramType.Monitoring
      case ProgramReferencePropertiesScienceInput(_, _)       => ProgramType.Science
      case ProgramReferencePropertiesSubaruInput(_, _)        => ProgramType.Subaru
      case ProgramReferencePropertiesSystemInput(_)           => ProgramType.System
    }

  def description: Option[ProgramReference.Description] =
    input match {
      case ProgramReferencePropertiesLibraryInput(_, d) => d.some
      case ProgramReferencePropertiesSystemInput(d)     => d.some
      case _                                            => none
    }

  def instrument: Option[Instrument] =
    input match {
      case ProgramReferencePropertiesCalibrationInput(_, i)   => i.some
      case ProgramReferencePropertiesCommissioningInput(_, i) => i.some
      case ProgramReferencePropertiesEngineeringInput(_, i)   => i.some
      case ProgramReferencePropertiesExampleInput(i)          => i.some
      case ProgramReferencePropertiesLibraryInput(i, _)       => i.some
      case ProgramReferencePropertiesMonitoringInput(_, i)    => i.some
      case _                                                  => none
    }

  def semester: Option[Semester] =
    input match {
      case ProgramReferencePropertiesCalibrationInput(s, _)   => s.some
      case ProgramReferencePropertiesCommissioningInput(s, _) => s.some
      case ProgramReferencePropertiesEngineeringInput(s, _)   => s.some
      case ProgramReferencePropertiesKeckInput(s)             => s.some
      case ProgramReferencePropertiesMonitoringInput(s, _)    => s.some
      case ProgramReferencePropertiesScienceInput(s, _)       => s.some
      case ProgramReferencePropertiesSubaruInput(s, _)        => s.some
      case _                                                  => none
    }

  def scienceSubtype: Option[ScienceSubtype] =
    input match {
      case ProgramReferencePropertiesScienceInput(_, s) => s.some
      case _                                            => none
    }

  def subaruProposalType: Option[SubaruCallForProposalsType] =
    input match {
      case ProgramReferencePropertiesSubaruInput(_, t) => t.some
      case _                                           => none
    }

}

object ProgramReferencePropertiesInput {

  val Binding: Matcher[ProgramReferencePropertiesInput] =
    ObjectFieldsBinding.rmap {
      case List(
        ProgramReferencePropertiesCalibrationInput.Binding.Option("calibration", rCal),
        ProgramReferencePropertiesCommissioningInput.Binding.Option("commissioning", rCom),
        ProgramReferencePropertiesEngineeringInput.Binding.Option("engineering", rEng),
        ProgramReferencePropertiesExampleInput.Binding.Option("example", rXpl),
        ProgramReferencePropertiesKeckInput.Binding.Option("keck", rKeck),
        ProgramReferencePropertiesLibraryInput.Binding.Option("library", rLib),
        ProgramReferencePropertiesMonitoringInput.Binding.Option("monitoring", rMon),
        ProgramReferencePropertiesScienceInput.Binding.Option("science", rSci),
        ProgramReferencePropertiesSubaruInput.Binding.Option("subaru", rSubaru),
        ProgramReferencePropertiesSystemInput.Binding.Option("system", rSys)
      ) => (rCal, rCom, rEng, rXpl, rKeck, rLib, rMon, rSci, rSubaru, rSys).parTupled.flatMap {
         case (Some(cal), None, None, None, None, None, None, None, None, None) => ProgramReferencePropertiesInput(cal).success
         case (None, Some(com), None, None, None, None, None, None, None, None) => ProgramReferencePropertiesInput(com).success
         case (None, None, Some(eng), None, None, None, None, None, None, None) => ProgramReferencePropertiesInput(eng).success
         case (None, None, None, Some(xpl), None, None, None, None, None, None) => ProgramReferencePropertiesInput(xpl).success
         case (None, None, None, None, Some(keck), None, None, None, None, None) => ProgramReferencePropertiesInput(keck).success
         case (None, None, None, None, None, Some(lib), None, None, None, None) => ProgramReferencePropertiesInput(lib).success
         case (None, None, None, None, None, None, Some(mon), None, None, None) => ProgramReferencePropertiesInput(mon).success
         case (None, None, None, None, None, None, None, Some(sci), None, None) => ProgramReferencePropertiesInput(sci).success
         case (None, None, None, None, None, None, None, None, Some(subaru), None) => ProgramReferencePropertiesInput(subaru).success
         case (None, None, None, None, None, None, None, None, None, Some(sys)) => ProgramReferencePropertiesInput(sys).success
         case _                                               =>
           Result.failure("Exactly one of 'calibration', 'commissioning', 'engineering', 'example', 'keck', 'library', 'monitoring', 'science', 'subaru' or 'system' expected.")
      }
    }

}
