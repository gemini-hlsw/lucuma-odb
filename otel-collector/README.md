# OTel Collector

Proxy that receives OTLP traces from gpp services and forwards them to Grafana Tempo and Honeycomb.

## Architecture

```
lucuma-odb services ──┐
                      │  Basic auth
                      ▼
browser (explore) ──► nginx (:$PORT) ──► otelcol-contrib (127.0.0.1:4318)
                      CORS + auth         │
                                          ├──► Grafana Cloud (OTLP HTTP → Tempo)
                                          └──► Honeycomb (OTLP HTTP)
```

The collector runs as a Heroku Docker app (`lucuma-otel-collector`). A
single dyno runs nginx alongside the collector; nginx terminates HTTP on
`$PORT`, handles CORS, and enforces Basic auth for every request whose
`Origin` header is **not** in the browser allowlist. Allowlisted
browser origins bypass auth. The collector itself listens only on
loopback. Honeycomb routing is determined by the `service.name`.

## Configuration

- `otel-collector-config.yaml` — receivers, exporters, and pipelines for
  the collector. Secrets are injected via environment variables.
- `nginx.conf.template` — nginx ingress; edit the two `map` blocks to
  change the browser `Origin` allowlist.
- `entrypoint.sh` — materializes `OTLP_HTPASSWD` into
  `/etc/nginx/.htpasswd`, renders the nginx template, and runs both
  processes.

### Heroku config vars

| Variable | Description |
|---|---|
| `HONEYCOMB_API_KEY` | Honeycomb ingest key |
| `GRAFANA_OTLP_ENDPOINT` | Full URL, e.g. `https://otlp-gateway-prod-us-east-3.grafana.net/otlp` |
| `GRAFANA_OTLP_TOKEN` | Base64-encoded `instanceID:token` from Grafana Cloud |
| `OTLP_HTPASSWD` | htpasswd entry enforced by nginx for non-browser origins, e.g. `otlp:$2y$05$...`. Generate with: `htpasswd -nbB otlp <password>` |

### Browser origin allowlist

Allow listed origins bypass Basic auth and receive CORS headers. Thus we avoid including the pwd
on the web applications though is a weak form of protection

```nginx
map $http_origin $auth_realm {
  default                            "Restricted";
  "https://explore.gemini.edu"       off;
  "https://explore-test.gemini.edu"  off;
  "https://explore-dev.lucuma.xyz"   off;
  "https://local.lucuma.xyz:8080"    off;
  "https://local.gemini.edu:8080"    off;
}
map $http_origin $cors_allow_origin {
  default                            "";
  "https://explore.gemini.edu"       $http_origin;
  "https://explore-test.gemini.edu"  $http_origin;
  "https://explore-dev.lucuma.xyz"   $http_origin;
  "https://local.lucuma.xyz:8080"    $http_origin;
  "https://local.gemini.edu:8080"    $http_origin;
}
```

## Deploying changes

The collector lives in the `otel-collector/` subdirectory of the main repo.
Heroku only sees that subtree. After editing `otel-collector-config.yaml` or `Dockerfile`, push with:

```bash
git push heroku-collector $(git subtree split --prefix otel-collector <branch>):main --force
```

Replace `<branch>` with the current branch name

The `heroku-collector` remote must point to `https://git.heroku.com/lucuma-otel-collector.git`. To set it up on a fresh clone:

```bash
heroku git:remote -a lucuma-otel-collector --remote heroku-collector
```
