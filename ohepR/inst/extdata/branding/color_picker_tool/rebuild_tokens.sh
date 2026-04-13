#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
REPO_ROOT="$(cd "${SCRIPT_DIR}/../../../.." && pwd)"

Rscript "${REPO_ROOT}/tools/build_color_picker_tokens.R"
echo "Done: rebuilt ${SCRIPT_DIR}/tokens.js from ${REPO_ROOT}/inst/extdata/branding/colors.csv"
