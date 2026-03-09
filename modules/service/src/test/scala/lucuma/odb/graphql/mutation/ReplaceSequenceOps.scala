// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import io.circe.Json
import lucuma.core.enums.Instrument
import lucuma.core.enums.SequenceType
import lucuma.core.model.Observation
import lucuma.core.model.sequence.Atom
import lucuma.core.model.sequence.Step
import lucuma.core.syntax.string.*

trait ReplaceSequenceOps:

  def atomInput(description: String, steps: String*): String =
    s"""
        {
          description: "$description"
          steps: ${steps.mkString("[\n", ",\n", "]\n")}
        }
    """

  def input(
    oid:          Observation.Id,
    sequenceType: SequenceType,
    atoms:        String*
  ): String =
    s"""
      {
        observationId: "$oid"
        sequenceType:  ${sequenceType.tag.toScreamingSnakeCase}
        sequence: ${atoms.mkString("[\n", ",\n", "]\n")}
      }
    """

  def mutation(instrument: Instrument, input: String): String =
    s"""
      mutation {
        replace${instrument.tag}Sequence(input: $input) {
          sequence {
            id
            steps { id }
          }
        }
      }
    """

  def mutationOutput(instrument: Instrument, json: Json): List[(Atom.Id, List[Step.Id])] =
    json
      .hcursor
      .downFields(s"replace${instrument}Sequence", "sequence")
      .values
      .toList
      .flatMap(_.toList)
      .map: atomJson =>
        val c       = atomJson.hcursor
        val atomId  = c.downField("id").require[Atom.Id]
        val stepIds =
          c.downField("steps")
            .values
            .toList
            .flatMap(_.toList)
            .map(_.hcursor.downField("id").require[Step.Id])
        (atomId, stepIds)