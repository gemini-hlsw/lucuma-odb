// Copyright (c) 2016-2026 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.json

import io.circe.Json
import io.circe.literal.*
import io.circe.testing.ArbitraryInstances
import io.circe.testing.CodecTests
import lucuma.core.model.group.GroupConstraintWaiver
import lucuma.core.model.group.GroupProgress
import lucuma.core.model.group.arb.ArbGroupProgress
import lucuma.core.util.Timestamp
import munit.DisciplineSuite

class GroupProgressSuite extends DisciplineSuite with ArbitraryInstances {

  import ArbGroupProgress.given
  import groupprogress.given

  checkAll("GroupProgress", CodecTests[GroupProgress].codec)

  test("a group that isn't empty but requires nothing doesn't decode"):
    val js =
      json"""
        {
          "status": "PENDING",
          "completed": 0,
          "required": 0,
          "nextWindow": null,
          "violations": []
        }
      """
    assert(js.as[GroupProgress].isLeft)

  test("a window ending before it starts doesn't decode"):
    val valid =
      json"""
        {
          "status": "ONGOING",
          "completed": 0,
          "required": 1,
          "nextWindow": { "start": ${Timestamp.Min}, "end": ${Timestamp.Max} },
          "violations": []
        }
      """
    assert(valid.as[GroupProgress].isRight)
    val js =
      json"""
        {
          "status": "ONGOING",
          "completed": 0,
          "required": 1,
          "nextWindow": { "start": ${Timestamp.Max}, "end": ${Timestamp.Min} },
          "violations": []
        }
      """
    assert(js.as[GroupProgress].isLeft)

  test("a waiver of a violation that can't be waived doesn't decode"):
    val waiver = (code: String, element: Json) =>
      json"""
        {
          "id": "w-1",
          "groupId": "g-1",
          "violation": { "code": $code, "element": $element, "night": null, "subgroupId": null },
          "reason": "The PI accepts it.",
          "grantedBy": "u-1",
          "grantedAt": ${Timestamp.Min},
          "revokedBy": null,
          "revokedAt": null
        }
      """
    val element = json"""{ "groupId": null, "observationId": "o-1" }"""
    assert(waiver("ORDER", element).as[GroupConstraintWaiver].isRight)
    assert(waiver("INSUFFICIENT_ELEMENTS", Json.Null).as[GroupConstraintWaiver].isLeft)

}
