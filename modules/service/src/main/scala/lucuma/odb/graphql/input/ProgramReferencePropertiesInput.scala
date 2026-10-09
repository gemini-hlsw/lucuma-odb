// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql.input

import cats.syntax.option.*
import lucuma.core.enums.Instrument
import lucuma.core.enums.ProgramType
import lucuma.core.enums.ScienceSubtype
import lucuma.core.enums.SubaruCallForProposalsType
import lucuma.core.model.ProgramReference
import lucuma.core.model.Semester
import lucuma.odb.graphql.binding.Matcher
import lucuma.odb.graphql.binding.OneOfBinding

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
    OneOfBinding(
      "calibration"   -> ProgramReferencePropertiesCalibrationInput.Binding.map(ProgramReferencePropertiesInput(_)),
      "commissioning" -> ProgramReferencePropertiesCommissioningInput.Binding.map(ProgramReferencePropertiesInput(_)),
      "engineering"   -> ProgramReferencePropertiesEngineeringInput.Binding.map(ProgramReferencePropertiesInput(_)),
      "example"       -> ProgramReferencePropertiesExampleInput.Binding.map(ProgramReferencePropertiesInput(_)),
      "keck"          -> ProgramReferencePropertiesKeckInput.Binding.map(ProgramReferencePropertiesInput(_)),
      "library"       -> ProgramReferencePropertiesLibraryInput.Binding.map(ProgramReferencePropertiesInput(_)),
      "monitoring"    -> ProgramReferencePropertiesMonitoringInput.Binding.map(ProgramReferencePropertiesInput(_)),
      "science"       -> ProgramReferencePropertiesScienceInput.Binding.map(ProgramReferencePropertiesInput(_)),
      "subaru"        -> ProgramReferencePropertiesSubaruInput.Binding.map(ProgramReferencePropertiesInput(_)),
      "system"        -> ProgramReferencePropertiesSystemInput.Binding.map(ProgramReferencePropertiesInput(_))
    )

}
