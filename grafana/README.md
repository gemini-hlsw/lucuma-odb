# Grafana resources

Definitions of what runs on the `gemini` Grafana Cloud stack for the GPP
services. Applied with [`gcx`](https://github.com/grafana/gcx); they are not
provisioned automatically, so a change here must be pushed by hand.

| Directory | What | Apply |
|---|---|---|
| `synthetic-monitoring/` | One readiness check per service and environment, `GET /health/ready` every 15 min from two probes | `gcx synthetic-monitoring checks create -f <file>` (first time) or `checks update <id> -f <file>` |
| `alerting/` | "Service readiness failing" (35 min of every probe failing), "Worker loop stalled" (no iteration for 5 min) and "Database connections near limit" (over 80% of `max_connections` for 10 min) | `gcx api /api/v1/provisioning/alert-rules/<uid> -X PUT -d @<file>` |
| `folders/` | The "Lucuma Health" folder the rules and dashboards live in | `gcx resources push folders -p grafana/folders` |
| `dashboard-templates/` | "GPP Service Status": readiness tiles, uptime, history, worker liveness, response time, database connections | `grafana/push-dashboards.sh` (add `--dry-run` to preview) |

Each template is rendered into one dashboard per environment (dev, staging,
production), so edit only the template. The hidden `env` variable filters every
query; `push-dashboards.sh` sets it, and suffixes the uid and title. Readiness
panels need a check whose `environment` label matches, so staging and production
show no readiness data until their checks exist.

Probe frequency is sized to the stack's monthly execution quota, which refuses
a check that would exceed it. Three checks at 15 min from two probes leaves
room for staging and production at the same cadence.

Check ids on the stack: odb 8158, sso 8159, itc 8160. The alert rules route to
the stack's default contact point.
