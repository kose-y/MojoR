# Scripts

Utility scripts for maintaining MöjoR repo hygiene and structural checks.

## `check_mirror_parity.sh`

Migration-complete guard for legacy mirror path tokens in tracked files.

Current repo workflow is package-authoritative (`packages/mojor/`), and this
script now enforces that legacy mirror paths are no longer present.

## `check_ir_docs_consistency.sh`

Checks targeted IR docs consistency:
- package-level IR docs are compatibility entrypoints
- active IR docs are free of legacy mirror path tokens
- contradiction-prone wording remains canonical

## `clean_build_artifacts.sh`

Removes generated binaries/object files and scratch artifacts.

Usage:

```bash
bash scripts/clean_build_artifacts.sh
bash scripts/clean_build_artifacts.sh --dry-run
```

Use `--dry-run` to preview removals before deleting.

## `parse_gibbs_benchmark_log.R`

Parses `gibbs_benchmark.R` output logs into canonical perf CSV (`name,ms`).

Usage:

```bash
Rscript scripts/parse_gibbs_benchmark_log.R \
  --input=/path/to/gibbs.log \
  --output=/path/to/current.csv
```

Emits required metrics:

- `gibbs_r_mean_ms`
- `gibbs_mojor_build_ms`
- `gibbs_mojor_mean_ms`
- `gibbs_mojor_fast_build_ms`
- `gibbs_mojor_fast_mean_ms`

And bulk metrics when present in the log:

- `gibbs_bulk_build_ms`
- `gibbs_bulk_mean_ms`

## `check_gibbs_perf_guardrail.sh`

Compares current Gibbs CSV versus baseline and enforces regression thresholds.

Usage:

```bash
bash scripts/check_gibbs_perf_guardrail.sh \
  --baseline docs/BASELINES/GIBBS_PERF_BASELINE.csv \
  --current /tmp/gibbs_current.csv
```

Defaults:

- build regression ratio limit: `1.35`
- runtime regression ratio limit: `1.20`
- minimum speedup floor (`r_mean/mojor_mean`): `2.00`

Exit codes:

- `0` clean
- `1` regression
- `2` input/usage error

## Release Gate Wiring

`check_release_candidate.sh` supports optional Gibbs perf gating:

```bash
bash scripts/check_release_candidate.sh \
  --with-gibbs-perf \
  --gibbs-perf-baseline docs/BASELINES/GIBBS_PERF_BASELINE.csv \
  --gibbs-perf-current /tmp/gibbs_current.csv
```

`run_release_validation.sh` forwards the same flags.
