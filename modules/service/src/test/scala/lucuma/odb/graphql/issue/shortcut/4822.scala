// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

// package lucuma.odb.graphql.issue.shortcut

// import lucuma.odb.graphql.OdbSuite
// import cats.effect.IO
// import lucuma.odb.Config
// import lucuma.odb.graphql.TestUsers

// N.B. this needs to be run with a local copy of the dev database
// class ShortCut_4822 extends OdbSuite:

//   val admin = TestUsers.Standard.admin(1, 101)
//   val validUsers = List(admin)

//   override def databaseConfig: Config.Database =
//     Config.Database(
//       maxConnections = 10,
//       maxCalibrationConnections = 10,
//       host     = "localhost",
//       port     = 5432,
//       user     = "jimmy",
//       password = "banana",
//       database = "lucuma-odb",
//     )

//   test("foo") {
//     query(
//       user = admin,
//       query = """
//         query {
//           observationsByWorkflowState(
//             states: [ UNDEFINED ]
//           ) {
//             id
//             workflow {
//               state
//             }
//           }
//         }
//       """
//     ).flatTap(IO.println)
//   }

