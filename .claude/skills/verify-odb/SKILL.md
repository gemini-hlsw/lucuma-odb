---
name: verify-odb
description: Launch an isolated local lucuma ODB (GraphQL service + fresh Postgres) built from the current checkout, mint PI / staff / admin / NGO / service / guest users, drive GraphQL as those users, and capture request/response/DB evidence. Use to prove an ODB change works end to end the way Explore or a script would hit it, beyond unit tests.
---

# Verify the ODB

The ODB's user surface is its GraphQL API at `POST /odb`, called with a bearer JWT. Explore and other clients are just callers of it. This skill runs a private ODB from the current checkout and drives that API as real users of any role.

How it works:

- Postgres runs in its own Docker container (`verify-odb-<port>-db`, label `verify-odb=<port>`) on port `15432`. Flyway migrates the empty database when the ODB starts.
- The ODB runs as plain `java` on the jars from `sbt service/stage`, on port `18082`. It does not use the staged `bin/` script, which is the Heroku launcher and fails locally.
- **No SSO.** The ODB only checks a JWT's signature against `ODB_SSO_PUBLIC_KEY`. So `up` generates a throwaway RSA key, hands the ODB its public half, and `scripts/mint_token.py` signs tokens for any user and role.
- External boundaries: ITC is the real **dev ITC** over the network. S3 and Mailgun get dummy credentials. SSO root points at a dead port.

Everything lives in `scripts/odb.sh`. Run it from anywhere inside the repo:

```bash
S=.claude/skills/verify-odb/scripts/odb.sh
```

## Launch

```bash
$S build    # sbt service/stage via the nix dev shell (~1-2 min warm). Re-run after any code change.
$S up       # starts Postgres + ODB, waits until usable, then runs doctor
```

`up` waits for the Postgres image's init to finish (`pg_isready` alone passes too early). Then it waits for the ODB's `GET /health`, and runs `doctor`, which checks that a minted PI token is accepted. Ready means doctor printed `auth ok`. That takes about 30 s from cold, including all Flyway migrations.

Requirements: Docker Desktop running (`open -a Docker`), and `uv` and `jq` on PATH. `build` needs `direnv` because `sbt` only exists in the repo's nix dev shell.

**Isolation:** set `VERIFY_ODB_PORT` and `VERIFY_ODB_DB_PORT` to run a second instance side by side. Every command reads the same variables, so export them for the whole session. `up` refuses to start if either port is taken. It never touches the user's own `docker-compose` database on 5432 or `service/reStart` on 8082.

Overridable: `VERIFY_ODB_ITC_ROOT` (default dev ITC) and `VERIFY_ODB_SSO_ROOT`.

## Doctor

```bash
$S doctor
```

Read-only. It checks, line by line:

- `process`: our ODB pid is alive.
- `port`: our process tree owns the port.
- `database`: our labelled container is running.
- `build`: the commit the ODB reports vs `HEAD`. If they differ, run `build` and restart, because you would be testing stale code.
- `auth`: a minted PI token gets a GraphQL answer.
- `ready`: printed for information only. It always shows `sso: fail` because there is no SSO, and that is expected.

Run it first whenever a request behaves oddly. Logs are in `.claude/verify-odb/run-<port>/odb.log`.

## Drive

```bash
$S gql <who> '<graphql>' ['<variables json>']
$S gql <who> @.claude/skills/verify-odb/queries/<file>.graphql ['<variables json>']
$S token <who>           # raw JWT, e.g. for curl or a websocket client
$S psql -c '<select>'    # read-only DB peek for side-effect proof
```

`<who>` picks the user. A fixed default id per kind means the same user every call:

| who | user |
| --- | --- |
| `pi` | standard user `u-1001`, PI role |
| `staff`, `admin` | standard users `u-1002`, `u-1003` |
| `ngo:US` (any partner tag) | standard user `u-1004`, NGO role |
| `service`, `guest` | `u-1005`, `u-1006` |
| `<kind>@<id>` | a distinct user of that kind, e.g. `pi@0x2001` as a second PI |

Ready-made queries live in `queries/`. Their variables are listed in the feature files under `features/`. Read `features/README.md` before driving, then follow the matching feature file.

## Evidence

```bash
export EVIDENCE=$($S evidence <feature-id>)   # creates .claude/verify-odb/evidence/<timestamp>-<feature-id>/
```

While `EVIDENCE` is set, each `gql` call writes `NNN-request.json` (with the user it ran as) and `NNN-response.json` there. Save `psql` output and notes into the same folder.

Proof standards:

- **Go through GraphQL as the real role** that would do it in production. Never write to the DB with `psql` to set up or prove a feature. `psql` is only for reading side effects.
- Capture the action **and** a second, read-only view of the result: re-query as the same user, and for access rules also as a user who must *not* see it.
- For permission changes, prove both sides: the allowed role succeeds **and** a disallowed role gets `not_authorized` or `null`.
- Verify stored side effects with `psql` (rows in `t_program`, `t_observation`, and so on) next to the API answer.
- An ITC answer is real dev ITC physics, not a mock. A failure there can be a genuine configuration problem. See `features/itc-and-sequence.md`.

## Cleanup

```bash
$S down
```

This stops only the pid this run started, removes only the container carrying our label, and deletes `.claude/verify-odb/run-<port>/` (key, logs, pid). **Evidence in `.claude/verify-odb/evidence/` is kept.** Run `down` after every failed attempt too. It is safe to run when nothing is up. Never `pkill java` or remove containers by name pattern.

## Helpers

- `scripts/odb.sh`: `build | up | doctor | token <who> | gql <who> <query|@file> [vars] | psql [args] | evidence [label] | down`.
- `scripts/mint_token.py`: `keygen <dir>` and `token <dir> <who> [--id N]`. A `uv` script, so its dependencies install themselves. You rarely call it directly, because `odb.sh token` wraps it.

## Not covered

- **Workers.** `obscalc`, `calibrations` and `pdfSummary` are not started. Observation workflow state stays `UNDEFINED` and background calculations stay pending. A change in those workers needs its own `runMain`.
- **Real email and S3.** Invitations and attachments fail at the Mailgun or S3 call.
- **Explore UI.** There is no browser path here.
