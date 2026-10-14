# Grafana resources

Definitions of what runs on the `gemini` Grafana Cloud stack for the GPP
services. Applied with [`gcx`](https://github.com/grafana/gcx); they are not
provisioned automatically, so a change here must be pushed by hand.

| Directory | What | Apply |
|---|---|---|
| `synthetic-monitoring/` | One readiness check per service and environment, `GET /health/ready` every 15 min from two probes | `gcx synthetic-monitoring checks create -f <file>` (first time) or `checks update <id> -f <file>` |
| `alerting/` | "Service readiness failing" (35 min of every probe failing), "Worker loop stalled" (no iteration for 5 min) and "Database connections near limit" (over 80% of `max_connections` for 10 min) | `gcx api /api/v1/provisioning/alert-rules/<uid> -X PUT -d @<file>` |
| `folders/` | The "Lucuma Health" folder the rules and dashboards live in | `gcx resources push folders -p grafana/folders` |
| `dashboard-templates/` | "GPP Service Status": readiness tiles, uptime, history, worker liveness, response time, database connections. "GPP Status Overview": component counts and 14 days of daily availability per service, worker and the database | `grafana/push-dashboards.sh` (add `--dry-run` to preview) |

Each template is rendered into one dashboard per environment (dev, staging,
production), so edit only the template. Every query filters on `$env`;
`push-dashboards.sh` replaces it with the literal environment, drops the
variable, and suffixes the uid and title. The rendered dashboards carry no
variables because making one public fails on a variable with no current value.
Readiness
panels need a check whose `environment` label matches, so staging and production
show no readiness data until their checks exist.

The stack keeps 14 days of metrics; a query reaching further back is clamped
and its points lose their midnight alignment, so the overview stays at 14 days.
Its daily cells use `offset -1d` so the cell stamped at a UTC midnight covers
the day that follows it, including today so far.

Probe frequency is sized to the stack's monthly execution quota, which refuses
a check that would exceed it. Nine checks (three services in three
environments) at 15 min from two probes fit within it.

Check ids on the stack: dev odb 8158, sso 8159, itc 8160; staging itc 8462, odb
8463, sso 8464; production itc 8465, odb 8466, sso 8467. The alert rules route to
the stack's default contact point.
