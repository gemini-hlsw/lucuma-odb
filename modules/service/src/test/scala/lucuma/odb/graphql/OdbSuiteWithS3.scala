// Copyright (c) 2016-2025 Association of Universities for Research in Astronomy, Inc. (AURA)
// For license information see LICENSE or https://opensource.org/licenses/BSD-3-Clause

package lucuma.odb.graphql

import cats.effect.*
import com.dimafeng.testcontainers.LocalStackV2Container
import eu.timepit.refined.types.string.NonEmptyString
import fs2.aws.s3.S3
import fs2.aws.s3.models.Models.BucketName
import fs2.aws.s3.models.Models.FileKey
import fs2.text
import io.laserdisc.pure.s3.tagless.Interpreter
import io.laserdisc.pure.s3.tagless.S3AsyncClientOp
import lucuma.odb.Config
import lucuma.refined.*
import org.testcontainers.containers.localstack.LocalStackContainer.Service
import software.amazon.awssdk.regions.Region
import software.amazon.awssdk.services.s3.S3AsyncClient
import software.amazon.awssdk.services.s3.S3Configuration
import software.amazon.awssdk.services.s3.model.CreateBucketRequest
import software.amazon.awssdk.services.s3.model.NoSuchKeyException
import software.amazon.awssdk.services.s3.presigner.S3Presigner

abstract class OdbSuiteWithS3 extends OdbSuite {

  private val s3Container = LocalStackV2Container(tag = "1.4.0", services = List(Service.S3))

  val bucketName = BucketName("test".refined)

  override protected def startS3: Unit = {
    s3Container.start()
    s3ClientOpsResource
      .use(s3Ops =>
        s3Ops.createBucket(CreateBucketRequest.builder().bucket(bucketName.value.value).build()).void
      )
      .unsafeRunSync()
  }
  override protected def stopS3: Unit  = s3Container.stop()

  override protected def awsConfig: Config.Aws =
    Config.Aws(
      accessKey = NonEmptyString.unsafeFrom(s3Container.container.getAccessKey()),
      secretKey = NonEmptyString.unsafeFrom(s3Container.container.getSecretKey()),
      basePath = "lucuma".refined,
      bucketName = bucketName,
      fileUploadMaxMb = 1
    )

  override protected def s3ClientOpsResource: Resource[IO, S3AsyncClientOp[IO]] =
    Interpreter[IO].S3AsyncClientOpResource(
      S3AsyncClient
        .builder()
        .endpointOverride(s3Container.endpointOverride(Service.S3))
        .credentialsProvider(
          s3Container.staticCredentialsProvider
        )
        .serviceConfiguration(
          S3Configuration
            .builder()
            .pathStyleAccessEnabled(true)
            .build()
        )
        .region(Region.of(s3Container.container.getRegion()))
    )

  override protected def s3PresignerResource: Resource[IO, S3Presigner] = {
    val builder     =
      S3Presigner
        .builder()
        .endpointOverride(s3Container.endpointOverride(Service.S3))
        .credentialsProvider(
          s3Container.staticCredentialsProvider
        )
        .serviceConfiguration(
          S3Configuration
            .builder()
            .pathStyleAccessEnabled(true)
            .build()
        )
        .region(Region.of(s3Container.container.getRegion()))

    Resource.fromAutoCloseable(IO.delay(builder.build()))
  }

  def assertS3(fileKey: FileKey, expected: String): IO[Unit] =
    s3ClientOpsResource.use(s3Ops =>
      val s3 = S3.create[IO](s3Ops)
      s3.readFile(bucketName, fileKey)
        .through(text.utf8.decode)
        .compile
        .string
        .assertEquals(expected)
    )

  def assertS3NotThere(fileKey: FileKey): IO[Unit] =
    s3ClientOpsResource.use(s3Ops =>
      val s3 = S3.create[IO](s3Ops)
      s3.readFile(bucketName, fileKey)
        .through(text.utf8.decode)
        .compile
        .string
        .intercept[NoSuchKeyException]
        .void
    )

}
