# samplekmeans development

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

# samplekmeans 0.1.0

* Initial GitHub submission.
