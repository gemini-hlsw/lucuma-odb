# Grafana resources

Definitions of what runs on the `gemini` Grafana Cloud stack for the GPP
services. Applied with [`gcx`](https://github.com/grafana/gcx); they are not
provisioned automatically, so a change here must be pushed by hand.

| Directory | What | Apply |
|---|---|---|
| `synthetic-monitoring/` | One readiness check per service and environment, `GET /health/ready` every 15 min from two probes | `gcx synthetic-monitoring checks create -f <file>` (first time) or `checks update <id> -f <file>` |
| `alerting/` | "Service readiness failing" (35 min of every probe failing) and "Worker loop stalled" (no iteration for 5 min) | `gcx api /api/v1/provisioning/alert-rules/<uid> -X PUT -d @<file>` |
| `folders/` | The "Lucuma Health" folder the rules and dashboard live in | `gcx resources push folders -p grafana` |
| `dashboards/` | "GPP Service Status": readiness tiles, uptime, history, worker liveness, response time | `gcx resources push dashboards -p grafana` |

Probe frequency is sized to the stack's monthly execution quota, which refuses
a check that would exceed it. Three checks at 15 min from two probes leaves
room for staging and production at the same cadence.

Check ids on the stack: odb 8158, sso 8159, itc 8160. The alert rules route to
the stack's default contact point.
