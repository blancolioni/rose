#!/bin/sh
set -e
branch=$(git rev-parse --abbrev-ref HEAD)
if [ "$branch" != main ]; then
    echo "update-build.sh: on branch $branch, not main" >&2
    exit 1
fi
git pull --ff-only
./clean-build.sh
