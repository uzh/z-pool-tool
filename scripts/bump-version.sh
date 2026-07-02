#!/usr/bin/env bash

set -euo pipefail

usage() {
  cat <<EOF
Usage:
  $0 --patch
  $0 --minor
  $0 --major
EOF
  exit 1
}

[[ $# -eq 1 ]] || usage

if ! git diff --quiet || ! git diff --cached --quiet; then
  echo "GIT: Working tree is not clean."
  echo "GIT: Commit or stash your changes before bumping the version."
  exit 1
fi

CURRENT_VERSION=$(
  grep '^version: "' ./pool.opam \
    | sed -E 's/version: "([^"]+)"/\1/'
)

if [[ -z "$CURRENT_VERSION" ]]; then
  echo "Failed to read version from pool.opam"
  exit 1
fi

IFS='.' read -r MAJOR MINOR PATCH <<< "$CURRENT_VERSION"

case "$1" in
  --patch)
    PATCH=$((PATCH + 1))
    ;;
  --minor)
    MINOR=$((MINOR + 1))
    PATCH=0
    ;;
  --major)
    MAJOR=$((MAJOR + 1))
    MINOR=0
    PATCH=0
    ;;
  *)
    usage
    ;;
esac

NEW_VERSION="${MAJOR}.${MINOR}.${PATCH}"

echo "Bumping version: ${CURRENT_VERSION} -> ${NEW_VERSION}"

# package.json
perl -0777 -pi -e \
  "s/\"version\"\\s*:\\s*\"[^\"]+\"/\"version\": \"$NEW_VERSION\"/" \
  ./package.json

# pool.opam
perl -pi -e \
  "s/^version: \".*\"/version: \"$NEW_VERSION\"/" \
  ./pool.opam

# pool.opam.locked
perl -pi -e \
  "s/^version: \".*\"/version: \"$NEW_VERSION\"/" \
  ./pool.opam.locked

# pool/version/version.ml
perl -pi -e \
  "s/^let to_string = \".*\"/let to_string = \"$NEW_VERSION\"/" \
  ./pool/version/version.ml

# dune-project
perl -pi -e \
  "s/^\\(version .*\\)/(version $NEW_VERSION)/" \
  ./dune-project

git add \
  package.json \
  pool.opam \
  pool.opam.locked \
  pool/version/version.ml \
  dune-project

git commit -m "chore: release ${NEW_VERSION}"

git tag -a "${NEW_VERSION}" -m "Release ${NEW_VERSION}"

echo
echo "Created:"
echo "  Commit: chore: release ${NEW_VERSION}"
echo "  Tag:    ${NEW_VERSION}"
echo
echo "To publish:"
echo "  git push"
echo "  git push origin ${NEW_VERSION}"
