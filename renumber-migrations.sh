#!/usr/bin/env bash
#
# Renumber this branch's Flyway migrations so they follow the base branch's
# highest migration contiguously.
#
# Run it after rebasing onto the base branch: the base will often have picked up
# migrations that now collide with (or sit above) the ones this branch adds, and
# branch protection requires the V<N>__*.sql numbers to stay sequential and
# gap-free.
#
# The branch's migrations keep their existing relative order; only their numbers
# move.  Renames go through `git mv` so history follows the file.
#
set -euo pipefail

MIGRATION_DIR="modules/service/src/main/resources/db/migration"

usage() {
  cat <<'EOF'
Usage: renumber-migrations.sh [--dry-run] [base-branch]

Renumbers the Flyway migrations added by the current branch so they follow the
base branch's highest migration number, contiguously and in their existing
relative order.

  base-branch   Branch to renumber against (default: main).
  --dry-run     Report what would change without touching anything.
EOF
}

DRY_RUN=false
BASE=""

while [ $# -gt 0 ]; do
  case "$1" in
    --dry-run)  DRY_RUN=true ;;
    -h|--help)  usage; exit 0 ;;
    -*)         echo "Unknown option: $1" >&2; usage >&2; exit 2 ;;
    *)          BASE="$1" ;;
  esac
  shift
done

BASE="${BASE:-main}"

cd "$(git rev-parse --show-toplevel)"

if ! git rev-parse --verify --quiet "$BASE" >/dev/null; then
  echo "error: base branch '$BASE' not found" >&2
  exit 1
fi

if [ ! -d "$MIGRATION_DIR" ]; then
  echo "error: migration directory '$MIGRATION_DIR' not found" >&2
  exit 1
fi

# Migration number of a path, stripped of leading zeros: V0350__foo.sql -> 350
number_of() {
  basename "$1" | sed -E 's/^V0*([0-9]+)__.*/\1/'
}

# The base branch's highest migration number, and the digit width to format with.
base_max=0
base_width=4
while read -r f; do
  [ -n "$f" ] || continue
  n="$(number_of "$f")"
  case "$n" in ''|*[!0-9]*) continue ;; esac
  if [ "$n" -gt "$base_max" ]; then
    base_max="$n"
    base_width="$(basename "$f" | sed -E 's/^V([0-9]+)__.*/\1/' | wc -c | tr -d ' ')"
    base_width=$((base_width - 1))
  fi
done < <(git ls-tree -r --name-only "$BASE" -- "$MIGRATION_DIR")

if [ "$base_max" -eq 0 ]; then
  echo "error: found no migrations on '$BASE'" >&2
  exit 1
fi

# Migrations this branch adds, relative to the base branch.  Compared against the
# working tree rather than HEAD so uncommitted and half-renamed files count too.
{
  git diff --no-renames --name-only --diff-filter=A "$BASE" -- "$MIGRATION_DIR"
  git ls-files --others --exclude-standard -- "$MIGRATION_DIR"
} | sort -u > /tmp/.renumber-added.$$
trap 'rm -f /tmp/.renumber-added.$$' EXIT

# Keep only files that still exist and look like migrations, sorted by number.
mine=()
while read -r f; do
  [ -n "$f" ] || continue
  [ -f "$f" ] || continue
  case "$(basename "$f")" in
    V[0-9]*__*.sql) ;;
    *) continue ;;
  esac
  mine+=("$(number_of "$f") $f")
done < /tmp/.renumber-added.$$

if [ "${#mine[@]}" -eq 0 ]; then
  echo "No migrations added by this branch relative to '$BASE'; nothing to do."
  exit 0
fi

IFS=$'\n' sorted=($(printf '%s\n' "${mine[@]}" | sort -n -k1,1)); unset IFS

# Assign each of ours the next number after the base's highest.
next=$((base_max + 1))
srcs=()
dsts=()
for entry in "${sorted[@]}"; do
  src="${entry#* }"
  name="$(basename "$src" | sed -E 's/^V[0-9]+__//')"
  dst="$MIGRATION_DIR/$(printf "V%0${base_width}d__%s" "$next" "$name")"
  srcs+=("$src")
  dsts+=("$dst")
  next=$((next + 1))
done

# Report, and collect the ones that actually move.
moves=0
echo "Base '$BASE' tops out at V$(printf "%0${base_width}d" "$base_max")."
for i in "${!srcs[@]}"; do
  if [ "${srcs[$i]}" = "${dsts[$i]}" ]; then
    echo "  ok    $(basename "${srcs[$i]}")"
  else
    echo "  move  $(basename "${srcs[$i]}")  ->  $(basename "${dsts[$i]}")"
    moves=$((moves + 1))
  fi
done

if [ "$moves" -eq 0 ]; then
  echo "Already numbered correctly; nothing to do."
  exit 0
fi

if [ "$DRY_RUN" = true ]; then
  echo
  echo "Dry run: $moves file(s) would be renamed."
  exit 0
fi

# Rename via temporary names so a target that is currently occupied by another
# migration we are also moving cannot be clobbered mid-flight.
move_one() {
  if git ls-files --error-unmatch "$1" >/dev/null 2>&1; then
    git mv "$1" "$2"
  else
    mv "$1" "$2"   # untracked: git mv would refuse
  fi
}

for i in "${!srcs[@]}"; do
  [ "${srcs[$i]}" = "${dsts[$i]}" ] && continue
  move_one "${srcs[$i]}" "${dsts[$i]}.renumber-tmp"
done

for i in "${!srcs[@]}"; do
  [ "${srcs[$i]}" = "${dsts[$i]}" ] && continue
  move_one "${dsts[$i]}.renumber-tmp" "${dsts[$i]}"
done

echo
echo "Renamed $moves migration(s)."
echo "Review with 'git status', then commit (or amend the commit that added them)."
