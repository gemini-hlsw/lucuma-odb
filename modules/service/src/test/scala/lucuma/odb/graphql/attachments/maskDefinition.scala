// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

package attachments

import cats.effect.IO
import cats.effect.Resource
import cats.syntax.all.*
import io.circe.ACursor
import io.circe.Json
import io.circe.literal.*
import lucuma.core.enums.ObservingModeType
import lucuma.core.model.Attachment
import lucuma.core.model.Observation
import lucuma.core.model.Program
import lucuma.odb.service.AttachmentFileService
import org.http4s.Response
import org.http4s.Status

import java.nio.charset.StandardCharsets

class maskDefinition extends AttachmentsSuite:

  val gmosMask = TestAttachment("GS2015AQ023-01_ODF.fits", "mos_mask", none, "unused", maskName = "GS2015AQ023-01".some, binary = gmosOdfFits.some)
  val f2Mask   = TestAttachment("GS2015AQ023-01_ODF.fits", "mos_mask", none, "unused", maskName = "GS2015AQ023-01".some, binary = flamingos2OdfFits.some)
  val garbage  = TestAttachment("GS2015AQ023-01_ODF.fits", "mos_mask", none, "not a fits file")

  // The F2 mas with its MASK_PA card overwritten in place by a COMMENT of
  // the same 80-byte length.
  // still a structurally valid design, but with no recorded position angle.
  lazy val noPaFits: Array[Byte] = {
    val bytes = flamingos2OdfFits.clone
    val start = new String(bytes, StandardCharsets.ISO_8859_1).indexOf("MASK_PA ")
    assert(start >= 0, "fixture has no MASK_PA card")
    val card  = "COMMENT position angle removed for this test".padTo(80, ' ')
    System.arraycopy(card.getBytes(StandardCharsets.US_ASCII), 0, bytes, start, 80)
    bytes
  }

  val noPaMask = TestAttachment("GS2015AQ023-01_ODF.fits", "mos_mask", none, "unused", maskName = "GS2015AQ023-01".some, binary = noPaFits.some)

  def expectInvalidMaskFile(response: Resource[IO, Response[IO]]): IO[Unit] =
    response.use { resp =>
      resp.getBody.map { body =>
        assertEquals(resp.status, Status.BadRequest)
        assert(body.startsWith("Invalid MOS mask file."), body)
      }
    }

  def queryMasks(pid: Program.Id): IO[List[Json]] =
    query(
      user  = pi,
      query = s"""
        query {
          program(programId: "$pid") {
            attachments {
              mask {
                name
                instrument
                pixelScale
                pointing { ra { hms } dec { dms } }
                positionAngle { degrees }
                dispersionDirection
                scienceSlitCount
                acquisitionSlitCount
                averageSlitWidth { arcseconds }
                slits {
                  id
                  coordinates { ra { hms } dec { dms } }
                  x
                  y
                  width { arcseconds }
                  length { arcseconds }
                  offsetAlongSlit { arcseconds }
                  offsetAcrossSlit { arcseconds }
                  tilt { degrees }
                  priority
                }
              }
            }
          }
        }
      """
    ).map(_.hcursor.downField("program").downField("attachments").as[List[Json]].fold(e => fail(e.getMessage), identity))

  extension (json: Json)
    def path(names: String*): Json =
      names.foldLeft(json.hcursor: ACursor)(_.downField(_)).focus.getOrElse(Json.Null)

    def decodePath[A: io.circe.Decoder](names: String*): A =
      names.foldLeft(json.hcursor: ACursor)(_.downField(_)).as[A].fold(e => fail(e.getMessage), identity)

  test("uploading a GMOS-S design records its mask definition"):
    for
      pid   <- createProgramAs(pi)
      _     <- insertAttachment(pi, pid, gmosMask).toAttachmentId
      masks <- queryMasks(pid)
      mask   = masks.head
      slits  = mask.decodePath[List[Json]]("mask", "slits")
      first  = slits.head
    yield
      assertEquals(masks.length, 1)
      assertEquals(mask.decodePath[String]("mask", "name"), "GS2015AQ023-01")
      assertEquals(mask.decodePath[String]("mask", "instrument"), "GMOS_SOUTH")
      assertEquals(mask.decodePath[BigDecimal]("mask", "positionAngle", "degrees"), BigDecimal("160.1"))
      assertEquals(mask.decodePath[BigDecimal]("mask", "pixelScale"), BigDecimal("0.16"))
      assertEquals(mask.decodePath[String]("mask", "pointing", "ra", "hms"), "23:59:07.055999")
      assertEquals(mask.decodePath[String]("mask", "pointing", "dec", "dms"), "-55:28:16.608000")
      assertEquals(mask.decodePath[String]("mask", "dispersionDirection"), "HORIZONTAL")
      assertEquals(slits.length, 40)
      assertEquals(first.decodePath[Int]("id"), 10)
      assertEquals(first.decodePath[String]("priority"), "MEDIUM")
      assertEquals(first.decodePath[String]("coordinates", "ra", "hms"), "23:58:59.135742")
      assertEqualsDouble(first.decodePath[BigDecimal]("x").toDouble, 765.2130127, 1e-6)
      assertEqualsDouble(first.decodePath[BigDecimal]("y").toDouble, 70.8921967, 1e-6)
      assertEquals(first.decodePath[BigDecimal]("width", "arcseconds"), BigDecimal(1))
      assertEquals(first.decodePath[BigDecimal]("length", "arcseconds"), BigDecimal(4))
      assertEquals(first.decodePath[BigDecimal]("tilt", "degrees"), BigDecimal(0))
      assertEquals(first.decodePath[BigDecimal]("offsetAlongSlit", "arcseconds"), BigDecimal(0))
      assertEquals(first.decodePath[BigDecimal]("offsetAcrossSlit", "arcseconds"), BigDecimal(0))
      assertEquals(slits.count(_.decodePath[String]("priority") === "ACQUISITION"), 3)
      assertEquals(mask.decodePath[Int]("mask", "scienceSlitCount"), 37)
      assertEquals(mask.decodePath[Int]("mask", "acquisitionSlitCount"), 3)
      // The stored average must agree with the slits it was derived from.
      val sciWidths = slits
        .filter(_.decodePath[String]("priority") =!= "ACQUISITION")
        .map(_.decodePath[BigDecimal]("width", "arcseconds"))
      assertEqualsDouble(
        mask.decodePath[BigDecimal]("mask", "averageSlitWidth", "arcseconds").toDouble,
        (sciWidths.sum / sciWidths.length).toDouble,
        1e-5
      )

  test("a file that is not a readable mask design is rejected"):
    for
      pid <- createProgramAs(pi)
      _   <- expectInvalidMaskFile(insertAttachment(pi, pid, garbage))
      masks <- queryMasks(pid)
    yield assertEquals(masks, List.empty)

  test("re-uploading a mask replaces its definition"):
    for
      pid   <- createProgramAs(pi)
      aid   <- insertAttachment(pi, pid, gmosMask).toAttachmentId
      _     <- updateAttachment(pi, aid, f2Mask).expectOk
      masks <- queryMasks(pid)
      mask   = masks.head
      slits  = mask.decodePath[List[Json]]("mask", "slits")
    yield
      assertEquals(mask.decodePath[String]("mask", "instrument"), "FLAMINGOS2")
      assertEquals(mask.decodePath[String]("mask", "dispersionDirection"), "VERTICAL")
      assertEquals(mask.decodePath[BigDecimal]("mask", "positionAngle", "degrees"), BigDecimal("104.5"))
      assertEquals(slits.length, 53)
      assertEquals(slits.count(_.decodePath[String]("priority") === "ACQUISITION"), 3)

  test("a failed re-upload keeps the existing definition and file"):
    for {
      pid   <- createProgramAs(pi)
      aid   <- insertAttachment(pi, pid, gmosMask).toAttachmentId
      _     <- expectInvalidMaskFile(updateAttachment(pi, aid, garbage))
      _     <- getAttachment(pi, aid).expectBodyBytes(gmosMask.bytes)
      masks <- queryMasks(pid)
      mask   = masks.head
    } yield {
      assertEquals(mask.decodePath[String]("mask", "instrument"), "GMOS_SOUTH")
      assertEquals(mask.decodePath[List[Json]]("mask", "slits").length, 40)
    }

  test("a design without a recorded position angle is rejected"):
    for
      pid   <- createProgramAs(pi)
      _     <- expectInvalidMaskFile(insertAttachment(pi, pid, noPaMask))
      masks <- queryMasks(pid)
    yield assertEquals(masks, List.empty)

  test("non-mask attachments have a null mask"):
    val finder = TestAttachment("finder.jpg", "finder", none, "A finder JPG file")
    for
      pid   <- createProgramAs(pi)
      _     <- insertAttachment(pi, pid, finder).toAttachmentId
      masks <- queryMasks(pid)
    yield assertEquals(masks.head.path("mask"), Json.Null)

  private def gmosSouthMosObservation(pid: Program.Id): IO[Observation.Id] =
    createTargetAs(pi, pid).flatMap: tid =>
      createObservationAs(pi, pid, ObservingModeType.GmosSouthMos.some, tid)

  private def assignMask(oid: Observation.Id, aid: Attachment.Id): String =
    s"""
      mutation {
        updateObservations(input: {
          WHERE: { id: { EQ: "$oid" } }
          SET: { observingMode: { gmosSouthMos: { customMask: {
            slitWidth: CUSTOM_WIDTH_0_50
            attachmentId: "$aid"
          } } } }
        }) {
          observations { observingMode { gmosSouthMos { customMask { attachmentId } } } }
        }
      }
    """

  test("an uploaded design can be assigned to an observation for its instrument"):
    for
      pid <- createProgramAs(pi)
      oid <- gmosSouthMosObservation(pid)
      aid <- insertAttachment(pi, pid, gmosMask).toAttachmentId
      _   <- expect(pi, assignMask(oid, aid), json"""
               {
                 "updateObservations": {
                   "observations": [
                     {
                       "observingMode": {
                         "gmosSouthMos": { "customMask": { "attachmentId": $aid } }
                       }
                     }
                   ]
                 }
               }
             """.asRight)
    yield ()

  test("an assigned mask's file cannot be replaced by one for another instrument"):
    for
      pid   <- createProgramAs(pi)
      oid   <- gmosSouthMosObservation(pid)
      aid   <- insertAttachment(pi, pid, gmosMask).toAttachmentId
      _     <- query(pi, assignMask(oid, aid))
      _     <- updateAttachment(pi, aid, f2Mask)
                 .withExpectation(Status.Conflict, AttachmentFileService.MaskInstrumentInUseMsg)
      // The refused replace leaves the original file and definition in place.
      _     <- getAttachment(pi, aid).expectBodyBytes(gmosMask.bytes)
      masks <- queryMasks(pid)
    yield assertEquals(masks.head.decodePath[String]("mask", "instrument"), "GMOS_SOUTH")

  test("an assigned mask's file can be replaced by one for the same instrument"):
    for
      pid   <- createProgramAs(pi)
      oid   <- gmosSouthMosObservation(pid)
      aid   <- insertAttachment(pi, pid, gmosMask).toAttachmentId
      _     <- query(pi, assignMask(oid, aid))
      _     <- updateAttachment(pi, aid, gmosMask).expectOk
      masks <- queryMasks(pid)
    yield assertEquals(masks.head.decodePath[String]("mask", "instrument"), "GMOS_SOUTH")
