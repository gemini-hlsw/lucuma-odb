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

The collector runs as a Docker app in heroku (`lucuma-otel-collector`).

A single dyno runs nginx alongside the collector.
nginx terminates HTTP, handles CORS, and enforces auth for every request whose
`Origin` header is **not** in the browser allowlist. This lets the webapps
through but requires auth for other clients.

The Allow list of browser origins bypass auth, this is a weak for of protection and
we could improve it in the future, but I don't want to have secrets on the client code

## Configuration

* `otel-collector-config.yaml` — receivers, exporters, and pipelines for the collector.
Secrets are injected via environment variables.
* `nginx.conf.template` — nginx template, defines the allow list.
* `entrypoint.sh` — materializes `OTLP_HTPASSWD` into
  `/etc/nginx/.htpasswd`, and runs both processes.

### Heroku config vars

| Variable | Description |
|---|---|
| `HONEYCOMB_API_KEY` | Honeycomb ingest key |
| `GRAFANA_OTLP_ENDPOINT` | Full URL, e.g. `https://otlp-gateway-prod-us-east-3.grafana.net/otlp` |
| `GRAFANA_OTLP_TOKEN` | Base64-encoded `instanceID:token` from Grafana Cloud |
| `OTLP_HTPASSWD` | htpasswd entry for receiver auth, e.g. `otlp:$2y$05$...`. Generate with: `htpasswd -nbB otlp <password>` |

### Browser origin allowlist

Allow listed origins bypass Basic auth and receive CORS headers.
Thus we avoid including the pwd or a token on the web applications though it
is a weak form of protection. see `nginx.conf.template`

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

## Running locally

Build and run the collector with dummy exporter secrets:

```bash
docker build -t otel-collector-local .

# Generate an htpasswd entry for user oltp and password banana
HTPASSWD=$(docker run --rm httpd:2.4-alpine htpasswd -nbB otlp banana)

docker run --rm --name otel-local \
  -p 8888:8888 \
  -e PORT=8888 \
  -e OTLP_HTPASSWD="$HTPASSWD" \
  -e HONEYCOMB_API_KEY=dummy \
  -e GRAFANA_OTLP_ENDPOINT=https://otlp-gateway.example.invalid \
  -e GRAFANA_OTLP_TOKEN=dummy \
  otel-collector-local
```

To point ODB at the local collector:

```bash
export ODB_OTEL_ENDPOINT=http://localhost:8888
export ODB_OTEL_KEY=$(echo -n 'otlp:banana' | base64)
```
