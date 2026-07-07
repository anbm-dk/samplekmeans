# samplekmeans development

## samplekmeans 1.1.0

* Merged `feature/prune-empty-small-clusters` into `main`.
* Improved handling of small or empty clusters during center selection for
    more robust sampling behavior.
* Expanded and refreshed tests/documentation to cover the updated behavior.

## samplekmeans 1.0.0

* Added optional candidate weighting via `candidate_weights` and
    `candidate_weight_col` with explicit precedence (`candidate_weights`
    overrides column-based weights with a warning).
* Hardened weight validation for raster, point, and `data.frame` inputs
    (finite/non-negative checks and clearer failures for invalid weights).
* Added focused raster candidate-weight tests in
    `setup/test_candidates_raster_spatvector_weights.R`.
* Added a test runner entrypoint `setup/run_tests.R` that discovers and runs
    all `setup/test_*.R` scripts with a pass/fail summary.
* Reworked `setup/test_sampling.R` into deterministic smoke tests so it runs
    cleanly under the shared test entrypoint.

* Added candidate-constrained center selection for non-raster inputs:
    `data.frame` and point `SpatVector`.
* For non-raster inputs, `candidates` now filters candidate indices more
    robustly (removes `NA`, keeps integer indices, and validates bounds).
* When candidates do not cover all clusters for non-raster inputs,
    fewer centers are returned (uncovered clusters are skipped).
* Added focused tests in `setup/test_candidates_nonraster.R` for
    `data.frame` and point workflows, including partial candidate coverage.
* `sample_kmeans()` now reliably restores options on exit and returns
    the output list.

## samplekmeans 0.1.0

* Initial GitHub submission.
