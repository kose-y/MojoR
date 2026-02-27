#!/usr/bin/env bash
set -euo pipefail

usage() {
  cat <<'USAGE'
Usage: check_release_report_consistency.sh [options]

Checks the generated release report format and status.

Options:
  --report <path>    report path (default: docs/BASELINES/RELEASE_REPORT_LATEST.md)
  --require-pass     require `Overall: PASS`
  --verbose          print report path and matched markers
  --help, -h         show help

Exit codes:
  0 consistent
  1 inconsistent
  2 usage error
USAGE
}

REPO_ROOT="$(cd "$(dirname "$0")/.." && pwd)"
REPORT="$REPO_ROOT/docs/BASELINES/RELEASE_REPORT_LATEST.md"
REQUIRE_PASS=0
VERBOSE=0

while [[ "${1:-}" != "" ]]; do
  case "${1:-}" in
    --report)
      REPORT="${2:-}"
      shift 2
      ;;
    --require-pass)
      REQUIRE_PASS=1
      shift
      ;;
    --verbose)
      VERBOSE=1
      shift
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      echo "Unknown argument: $1" >&2
      usage
      exit 2
      ;;
  esac
done

if [ ! -f "$REPORT" ]; then
  echo "Release report consistency: missing report: $REPORT"
  exit 1
fi

violation=0

if ! grep -Fq -- "# Release Validation Report" "$REPORT"; then
  echo "Release report consistency: missing report title marker."
  violation=1
fi
if ! grep -Fq -- "- Generated:" "$REPORT"; then
  echo "Release report consistency: missing generated timestamp marker."
  violation=1
fi
if ! grep -Fq -- "- Readiness status:" "$REPORT"; then
  echo "Release report consistency: missing readiness status marker."
  violation=1
fi
if ! grep -Fq -- "- Release-candidate status:" "$REPORT"; then
  echo "Release report consistency: missing release-candidate status marker."
  violation=1
fi
if ! grep -Fq -- "- Overall:" "$REPORT"; then
  echo "Release report consistency: missing overall status marker."
  violation=1
fi

readiness_status="$(sed -n 's/^- Readiness status:[[:space:]]*//p' "$REPORT" | head -n 1)"
rc_status="$(sed -n 's/^- Release-candidate status:[[:space:]]*//p' "$REPORT" | head -n 1)"
overall_status="$(sed -n 's/^- Overall:[[:space:]]*//p' "$REPORT" | head -n 1)"

if [[ -z "$readiness_status" || ! "$readiness_status" =~ ^[0-9]+$ ]]; then
  echo "Release report consistency: invalid readiness status value."
  violation=1
fi
if [[ -z "$rc_status" || ! "$rc_status" =~ ^[0-9]+$ ]]; then
  echo "Release report consistency: invalid release-candidate status value."
  violation=1
fi
if [[ "$overall_status" != "PASS" && "$overall_status" != "FAIL" ]]; then
  echo "Release report consistency: invalid overall status value."
  violation=1
fi

if [[ "$readiness_status" =~ ^[0-9]+$ && "$rc_status" =~ ^[0-9]+$ && ( "$overall_status" = "PASS" || "$overall_status" = "FAIL" ) ]]; then
  if [ "$readiness_status" -eq 0 ] && [ "$rc_status" -eq 0 ]; then
    if [ "$overall_status" != "PASS" ]; then
      echo "Release report consistency: overall must be PASS when readiness and release-candidate statuses are 0."
      violation=1
    fi
  else
    if [ "$overall_status" != "FAIL" ]; then
      echo "Release report consistency: overall must be FAIL when readiness or release-candidate status is non-zero."
      violation=1
    fi
  fi
fi

if [ "$REQUIRE_PASS" -eq 1 ] && [ "$overall_status" != "PASS" ]; then
  echo "Release report consistency: report is not PASS."
  violation=1
fi

if [ "$VERBOSE" -eq 1 ]; then
  echo "Checked report: $REPORT"
  grep -E '^- (Generated|Readiness status|Release-candidate status|Overall):' "$REPORT" || true
fi

if [ "$violation" -ne 0 ]; then
  exit 1
fi

echo "Release report consistency: clean"
exit 0
