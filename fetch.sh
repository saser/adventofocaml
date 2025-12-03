#!/usr/bin/env zsh

if [ -z "${SESSION}" ]; then
    echo "Usage: SESSION=<session cookie> ./fetch.sh"
    exit 1
fi

set -euo pipefail

for year in {2015..2025}; do
    for day in {1..25}; do
        # Mask errors as a hacky solution to the problem "the input for ${year}
        # and ${day} isn't available yet", since it would return a 404.
        (curl -fsSL "https://adventofcode.com/${year}/day/${day}/input" \
            -b "session=${SESSION}" \
            -o "lib/inputs/$(printf year%04d_day%02d ${year} ${day})" || true) &
    done
done

wait
