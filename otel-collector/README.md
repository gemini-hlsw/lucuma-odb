# OTel Collector

Proxy that receives OTLP traces from gpp services and forwards them to Grafana Tempo and Honeycomb.

## Architecture

```
lucuma-odb services
       │
       │  OTLP HTTP
       ▼
OTel Collector
       ├──► Grafana Cloud (OTLP HTTP → Tempo)
       └──► Honeycomb (OTLP HTTP)
```

The collector runs as a separate Heroku app (`lucuma-otel-collector`). Services send traces to its URL via OTLP HTTP.
Honeycomb routing is determined by the `service.name`.

## Configuration

`otel-collector-config.yaml` defines receivers, exporters, and pipelines. Secrets are injected via environment variables.

### Heroku config vars

| Variable | Description |
|---|---|
| `HONEYCOMB_API_KEY` | Honeycomb ingest key |
| `GRAFANA_OTLP_ENDPOINT` | Full URL, e.g. `https://otlp-gateway-prod-us-east-3.grafana.net/otlp` |
| `GRAFANA_OTLP_TOKEN` | Base64-encoded `instanceID:token` from Grafana Cloud |
| `OTLP_HTPASSWD` | htpasswd entry for receiver auth, e.g. `otlp:$2y$05$...`. Generate with: `htpasswd -nbB otlp <password>` |

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
