// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql
package mutation

import cats.syntax.all.*
import io.circe.Json
import io.circe.literal.*
import lucuma.core.model.User

class addConditionsEntry extends OdbSuite {

  val staff: User = TestUsers.Standard.staff(nextId, nextId)
  val pi: User = TestUsers.Standard.pi(nextId, nextId)

  override lazy val validUsers: List[User] = List(staff, pi)

  test("input validation: empty input not allowed") {
    expect(
      pi,
      """
        mutation {
          addConditionsEntry(
            input: {}
          ) {
            conditionsEntry {
              user {
                id
              }
            }
          }
        }
      """,
      expected = Left(List(
        "Argument 'input' is invalid: At least one of measurement and intuition must be specified."
      ))
    )
  }

  test("input validation: measurement source is required") {
    expect(
      pi,
      """
        mutation {
          addConditionsEntry(
            input: {
              measurement: {
              }
            }
          ) {
            conditionsEntry {
              user {
                id
              }
            }
          }
        }
      """,
      expected = Left(List(
        "Value of type ConditionsMeasurementSource required for 'source' in field 'addConditionsEntry' of type 'Mutation'"
      ))
    )
  }

  test("input validation: measurement can't be empty") {
    expect(
      pi,
      """
        mutation {
          addConditionsEntry(
            input: {
              measurement: {
                source: OBSERVER
              }
            }
          ) {
            conditionsEntry {
              user {
                id
              }
            }
          }
        }
      """,
      expected = Left(List(
        "Argument 'input.measurement' is invalid: At least one of seeing, wavelength, extinction, azimuth, and elevation must be defined."
      ))
    )
  }

  test("input validation: if azimuth is present, so is elevation") {
    expect(
      pi,
      """
        mutation {
          addConditionsEntry(
            input: {
              measurement: {
                source: OBSERVER
                azimuth: { degrees: 20 }
              }
            }
          ) {
            conditionsEntry {
              user {
                id
              }
            }
          }
        }
      """,
      expected = Left(List(
        "Argument 'input.measurement' is invalid: Azimuth and elevation must both be defined, or must both be empty."
      ))
    )
  }

  test("input validation: if elevation is present, so is azimuth") {
    expect(
      pi,
      """
        mutation {
          addConditionsEntry(
            input: {
              measurement: {
                source: OBSERVER
                elevation: { degrees: 30 }
              }
            }
          ) {
            conditionsEntry {
              user {
                id
              }
            }
          }
        }
      """,
      expected = Left(List(
        "Argument 'input.measurement' is invalid: Azimuth and elevation must both be defined, or must both be empty."
      ))
    )
  }

  test("input validation: intuition can't be empty") {
    expect(
      pi,
      """
        mutation {
          addConditionsEntry(
            input: {
              intuition: {
              }
            }
          ) {
            conditionsEntry {
              user {
                id
              }
            }
          }
        }
      """,
      expected = Left(List(
        "Argument 'input.intuition' is invalid: At least one of expectation and seeingTrend must be specified."
      ))
    )
  }

  test("pi can't add an entry") {
    expect(
      pi,
      """
        mutation {
          addConditionsEntry(
            input: {
              measurement: {
                source: OBSERVER
                seeing: { arcseconds: 0.4 }
                extinction: 2.34
                wavelength: { micrometers: 1 }
                azimuth: { degrees: 20 }
                elevation: { degrees: 30 }
              }
              intuition: {
                expectation: {
                  type: THIN_CLOUDS
                  timeframe: { hours: 1.5 }
                }
                seeingTrend: VARIABLE
              }
            }
          ) {
            conditionsEntry {
              user {
                id
              }
            }
          }
        }
      """,
      expected = Left(List(
        s"User ${pi.id} is not authorized to perform this operation."
      ))
    )
  }

    test("staff user can add an entry (minimal)") {
    expect(
      staff,
      """
        mutation {
          addConditionsEntry(
            input: {
              measurement: {
                source: OBSERVER
                extinction: 2.34
              }
              intuition: {
                seeingTrend: VARIABLE
              }
            }
          ) {
            conditionsEntry {
              user {
                id
              }
              measurement {
                source
                seeing { arcseconds }
                extinction
                wavelength { micrometers }
                azimuth { degrees }
                elevation { degrees }                
              }
              intuition {
                seeingTrend
                expectation {
                  type
                  timeframe { hours }
                }
              }
            }
          }
        }
      """,
      expected = Right(
        json"""
          {
            "addConditionsEntry" : {
              "conditionsEntry" : {
                "user" : {
                  "id" : ${staff.id}
                },
                "measurement" : {
                  "source" : "OBSERVER",
                  "seeing" : null,
                  "extinction" : 2.34,
                  "wavelength" : null,
                  "azimuth" : null,
                  "elevation" : null
                },
                "intuition" : {
                  "seeingTrend" : "VARIABLE",
                  "expectation" : null
                }
              }
            }
          }
        """
      )
    )
  }

  test("staff user can add an entry (all fields defined)") {
    expect(
      staff,
      """
        mutation {
          addConditionsEntry(
            input: {
              measurement: {
                source: OBSERVER
                seeing: { arcseconds: 0.4 }
                extinction: 2.34
                wavelength: { micrometers: 1 }
                azimuth: { degrees: 20 }
                elevation: { degrees: 30 }
              }
              intuition: {
                expectation: {
                  type: THIN_CLOUDS
                  timeframe: { hours: 1.5 }
                }
                seeingTrend: VARIABLE
              }
            }
          ) {
            conditionsEntry {
              user {
                id
              }
              measurement {
                source
                seeing { arcseconds }
                extinction
                wavelength { micrometers }
                azimuth { degrees }
                elevation { degrees }                
              }
              intuition {
                seeingTrend
                expectation {
                  type
                  timeframe { hours }
                }
              }
            }
          }
        }
      """,
      expected = Right(
        json"""
          {
            "addConditionsEntry" : {
              "conditionsEntry" : {
                "user" : {
                  "id" : ${staff.id}
                 },
                 "measurement" : {
                    "source" : "OBSERVER",
                    "seeing" : {
                      "arcseconds" : 0.4
                    },
                    "extinction" : 2.34,
                    "wavelength" : {
                      "micrometers" : 1.000000
                    },
                    "azimuth" : {
                      "degrees" : 20
                    },
                    "elevation" : {
                      "degrees" : 30
                    }
                 },
                 "intuition" : {
                   "seeingTrend" : "VARIABLE",
                   "expectation" : {
                     "type" : "THIN_CLOUDS",
                     "timeframe" : {
                       "hours" : 1.500000
                     }
                  }  
                }
              }
            }
          }
        """
      )
    )
  }

  test("staff user can add an entry (just measurement)") {
    expect(
      staff,
      """
        mutation {
          addConditionsEntry(
            input: {
              measurement: {
                source: OBSERVER
                seeing: { arcseconds: 0.4 }
                extinction: 2.34
                wavelength: { micrometers: 1 }
                azimuth: { degrees: 20 }
                elevation: { degrees: 30 }
              }
            }
          ) {
            conditionsEntry {
              user {
                id
              }
              measurement {
                source
                seeing { arcseconds }
                extinction
                wavelength { micrometers }
                azimuth { degrees }
                elevation { degrees }                
              }
              intuition {
                seeingTrend
                expectation {
                  type
                  timeframe { hours }
                }
              }
            }
          }
        }
      """,
      expected = Right(
        json"""
          {
            "addConditionsEntry" : {
              "conditionsEntry" : {
                "user" : {
                  "id" : ${staff.id}
                },
                "measurement" : {
                  "source" : "OBSERVER",
                  "seeing" : {
                    "arcseconds" : 0.4
                  },
                  "extinction" : 2.34,
                  "wavelength" : {
                    "micrometers" : 1.000000
                  },
                  "azimuth" : {
                    "degrees" : 20
                  },
                  "elevation" : {
                    "degrees" : 30
                  }
                },
                "intuition" : null
              }
            }
          }
        """
      )
    )
  }

  test("staff user can add an entry (just intuition)") {
    expect(
      staff,
      """
        mutation {
          addConditionsEntry(
            input: {
              intuition: {
                expectation: {
                  type: THIN_CLOUDS
                  timeframe: { hours: 1.5 }
                }
                seeingTrend: VARIABLE
              }
            }
          ) {
            conditionsEntry {
              user {
                id
              }
              measurement {
                source
                seeing { arcseconds }
                extinction
                wavelength { micrometers }
                azimuth { degrees }
                elevation { degrees }                
              }
              intuition {
                seeingTrend
                expectation {
                  type
                  timeframe { hours }
                }
              }
            }
          }
        }
      """,
      expected = Right(
        json"""
          {
            "addConditionsEntry" : {
              "conditionsEntry" : {
                "user" : {
                  "id" : ${staff.id}
                },
                "measurement" : null,
                "intuition" : {
                  "seeingTrend" : "VARIABLE",
                  "expectation" : {
                    "type" : "THIN_CLOUDS",
                    "timeframe" : {
                      "hours" : 1.500000
                    }
                  }
                }
              }
            }
          }
        """
      )
    )
  }

}
