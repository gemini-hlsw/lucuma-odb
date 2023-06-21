// Copyright (c) 2016-2023 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.service

import cats.effect.MonadCancelThrow
import cats.syntax.all.*
import lucuma.core.model.Access
import lucuma.core.model.StandardUser
import lucuma.odb.util.Codecs.*
import skunk.*
import skunk.codec.all.*
import skunk.syntax.all.*

import Services.Syntax.*

trait ChronicleService[F[_]]//:
  // def insertObservedConditions(input: ObservedConditionsInput)(using Transaction[F]): F[Long]

object ChronicleService:

  def instantiate[F[_]: MonadCancelThrow](using Services[F]): ChronicleService[F] =
    new ChronicleService[F] {}//:

  //     def insertObservedConditions(input: ObservedConditionsInput)(using Transaction[F]): F[Long] =
  //       user.verifyAccess(Access.Staff) >> // only staff or better can log conditions
  //       session.prepareR(InsertObservedConditions).use { pq =>
  //         pq.unique(input)
  //       }

  object Statements//:

    // val InsertObservedConditions: Query[ObservedConditionsInput, Long] =
    //   sql"""
    //     INSERT INTO t_chron_observed_conditions (
    //       c_cloud_extinction,
    //       c_image_quality,   
    //       c_sky_background,  
    //       c_water_vapor
    //     ) VALUES (
    //       ${cloud_extinction.opt},
    //       ${image_quality.opt},
    //       ${sky_background.opt},
    //       ${water_vapor.opt}
    //     )
    //     RETURNING c_chron_id
    //   """.query(int8)
    //     .contramap[ObservedConditionsInput] { oci =>
    //       (oci.cloudExtinction, oci.imageQuality, oci.skyBackground, oci.waterVapor)  
    //     }