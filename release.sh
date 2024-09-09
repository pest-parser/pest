#!/bin/bash

set -e

CRATES="pest pest_meta pest_vm pest_generator pest_derive pest_grammars pest_debugger"

get_local_version() {
  cargo metadata --format-version 1 | jq -r '.packages[]|select(.name == "'"${1}"'" and ((.manifest_path|contains("registry"))|not))|.version'
}

check_version_online() {
  curl -s "https://crates.io/api/v1/crates/${1}" | jq -r '.versions[]|select(.num == "'"${2}"'").updated_at'
}

get_manifest_path() {
  cargo metadata --format-version 1 | jq -r '.packages[]|select(.name == "'"${1}"'" and ((.manifest_path|contains("registry"))|not))|.manifest_path'
}

publish() {
  echo "Publishing crate $1..."
  if [ "${1}" = "pest" ] || [ "${1}" = "pest_grammars" ] || [ "${1}" = "pest_debugger" ]; then
    cargo publish --manifest-path "$(get_manifest_path "${1}")" --allow-dirty --all-features
  else
    # cannot publish with the `not-bootstrap-in-src` feature enabled
    cargo publish --manifest-path "$(get_manifest_path "${1}")" --allow-dirty --features grammar-extras
  fi
  echo ""
}

wait_until_available() {
  echo "Waiting for crate ${1} to become available via crates.io..."
  for retry in {1..5}; do
    sleep 5
    ONLINE_DATE="$(check_version_online "${1}" "${2}")"
    if [ -n "${ONLINE_DATE}" ]; then
      echo "Crate ${crate} is now available online"
      break
    else
      if [ "${retry}" == 5 ]; then
        echo "ERROR: Crate should have become available by now"
        exit 1
      else
        echo "Not available just yet. Waiting a few seconds..."
      fi
    fi
  done
  echo "Waiting an additional 20 seconds for crate to propagate through CDN..."
  sleep 20
}

for crate in ${CRATES}; do
  VERSION="$(get_local_version "${crate}")"
  publish "${crate}"
  wait_until_available "${crate}" "${VERSION}"
done