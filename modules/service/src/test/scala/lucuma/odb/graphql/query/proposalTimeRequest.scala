// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package query

import cats.effect.IO
import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.Program

// The proposal's time request: derived from the program's observations until
// somebody states one explicitly.
class proposalTimeRequest extends ExecutionTestSupportForGmos:

  // A program with a proposal and one fully defined observation, so that the
  // derived request is a real, non-zero figure.
  private val setup: IO[Program.Id] =
    for
      pid <- createProgramAs(pi)
      tid <- createTargetWithProfileAs(pi, pid)
      oid <- createGmosNorthLongSlitObservationAs(pi, pid, List(tid))
      _   <- runObscalcUpdate(pid, oid)
      _   <- addProposal(pi, pid)
    yield pid

  private def requestQuery(pid: Program.Id): String =
    s"""
      query {
        program(programId: "$pid") {
          timeEstimateRange {
            value { minimum { total { seconds } } maximum { total { seconds } } }
          }
          proposal {
            timeRequest {
              calculationState
              value { minimum { total { seconds } } maximum { total { seconds } } }
            }
            defaultTimeRequest {
              value { minimum { total { seconds } } maximum { total { seconds } } }
            }
            explicitTimeRequest { hours }
          }
        }
      }
    """

  private def setTimeRequest(pid: Program.Id, value: String): IO[Unit] =
    query(
      pi,
      s"""
        mutation {
          updateProposal(
            input: {
              programId: "$pid"
              SET: { explicitTimeRequest: $value }
            }
          ) {
            proposal { explicitTimeRequest { hours } }
          }
        }
      """
    ).void

  // The estimate, the effective request and the default request, in that order.
  private def requestFields(js: Json): (Option[Json], Option[Json], Option[Json]) =
    val program  = js.hcursor.downField("program")
    val proposal = program.downField("proposal")
    (
      program.downFields("timeEstimateRange", "value").focus,
      proposal.downFields("timeRequest", "value").focus,
      proposal.downFields("defaultTimeRequest", "value").focus
    )

  test("unset: the request is the sum over the program's observations"):
    setup.flatMap: pid =>
      query(pi, requestQuery(pid)).map: js =>
        val (estimate, effective, default) = requestFields(js)
        // Compared against the program's own estimate rather than a hard-coded
        // duration, which would only restate the sequence calculation here.
        assert(estimate.exists(_.asObject.nonEmpty), s"No time estimate in: $js")
        assertEquals(effective, estimate)
        assertEquals(default, estimate)
        assertEquals(js.hcursor.downFields("program", "proposal", "explicitTimeRequest").focus, Json.Null.some)

  test("explicit: replaces the derived sum, which keeps tracking the observations"):
    setup.flatMap: pid =>
      setTimeRequest(pid, "{ hours: 42 }") >>
      query(pi, requestQuery(pid)).map: js =>
        val (estimate, effective, default) = requestFields(js)

        // The explicit request stands alone: a settled, degenerate range charged
        // entirely to program time.
        assertEquals(
          effective,
          json"""
            {
              "minimum": { "total": { "seconds": 151200.000000 } },
              "maximum": { "total": { "seconds": 151200.000000 } }
            }
          """.some
        )
        assertEquals(
          js.hcursor.downFields("program", "proposal", "timeRequest", "calculationState").focus,
          json""""READY"""".some
        )
        assertEquals(
          js.hcursor.downFields("program", "proposal", "explicitTimeRequest").focus,
          json"""{ "hours": 42.000000 }""".some
        )

        // The derivation is untouched by the override, and the program's
        // estimate is unaware of it altogether.
        assertEquals(default, estimate)
        assertNotEquals(effective, estimate)

  test("cleared: null returns the request to the derived sum"):
    setup.flatMap: pid =>
      setTimeRequest(pid, "{ hours: 42 }") >>
      setTimeRequest(pid, "null") >>
      query(pi, requestQuery(pid)).map: js =>
        val (estimate, effective, _) = requestFields(js)
        assertEquals(effective, estimate)
        assertEquals(js.hcursor.downFields("program", "proposal", "explicitTimeRequest").focus, Json.Null.some)
