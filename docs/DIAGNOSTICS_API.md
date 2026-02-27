# Diagnostics API Reference

This page documents callable diagnostics helpers used in development checkouts.

Status note:

- These helpers are currently non-exported in the installed package API.
- They are available in source checkouts and are used by troubleshooting/debug workflows.

## Diagnostics Helpers

| Function | Purpose | Return shape | Status |
|---|---|---|---|
| `mojor_diagnostics()` | Read current in-memory diagnostics buffer. | List of diagnostic entries. | non-exported helper |
| `mojor_diagnostics_filter(severity = NULL)` | Filter diagnostics by severity. | Filtered list. | non-exported helper |
| `mojor_diagnostics_report(severity = NULL)` | Tabular diagnostics report helper. | Data-frame-like report payload. | non-exported helper |
| `mojor_diagnostics_clear()` | Clear diagnostics buffer. | `NULL` (invisible). | non-exported helper |
| `mojor_diagnostics_sink(fn = NULL, severity = NULL)` | Register callback sink for emitted diagnostics. | Registered sink metadata. | non-exported helper |
| `mojor_diagnostics_snapshot(severity = NULL)` | Snapshot diagnostics buffer for reporting. | Snapshot list. | non-exported helper |
| `mojor_diagnostics_export(path, format = c("json", "csv"), severity = NULL)` | Export diagnostics to file. | Output path/invisible value. | non-exported helper |

## Minimal Example

```r
# Development checkout helper usage
mojor_diagnostics_clear()

# run a transpile/build call here...

mojor_diagnostics_report()
mojor_diagnostics_snapshot()
```

## Related References

- `docs/DIAGNOSTICS_INDEX.md`
- `docs/TROUBLESHOOTING.md`
- `docs/COOKBOOK.md`
- `docs/API_SURFACE.md`
