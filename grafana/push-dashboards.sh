#!/usr/bin/env bash
# Renders one dashboard per environment from dashboard-templates/ and pushes them.
# Extra arguments go to gcx, e.g. --dry-run or --context remote.
set -euo pipefail

here="$(cd "$(dirname "$0")" && pwd)"
out="$(mktemp -d)"
trap 'rm -rf "$out"' EXIT

for template in "$here"/dashboard-templates/*.json; do
  base="$(basename "$template" .json)"
  for env in dev staging production; do
    jq --arg env "$env" '
      .metadata.name += "-" + $env
      | .spec.uid += "-" + $env
      | .spec.title += " (" + $env + ")"
      | .spec.tags += [$env]
      | .spec.templating.list |= map(select(.name != "env"))
      | walk(if type == "string" then gsub("\\$env\\b"; $env) else . end)
    ' "$template" > "$out/$base-$env.json"
  done
done

gcx resources push dashboards -p "$out" "$@"
