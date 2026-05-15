# Resource

The GPP resource service is a combination telescope calendar and configuration/resource manager (e.g. ICTD replacement).

Declare a `DATABASE_URL` environment variable pointing to a Postgres database and run the service:

```
   sbt '~resourceService/reStart'
```

Then open the graphql playground at http://localhost:8484/resource/playground.html to interact with the API.

## GraphQL Schema

The schema is defined in `resource/service/src/main/resources/graphql/resource.graphql`. It uses imports from the [ODB schema](../modules/schema/src/main/resources/lucuma/odb/graphql/OdbSchema.graphql) and combines the two schemas at runtime using the `SchemaStitcher` in the `bindings` module.

## OpenTelemetry (OTLP) Setup

Services support exporting traces and metrics via OTLP.
Telemetry is enabled when both `RESOURCE_OTEL_ENDPOINT` and `RESOURCE_OTEL_KEY` are set; otherwise it is a no-op.
