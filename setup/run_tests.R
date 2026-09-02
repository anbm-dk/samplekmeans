# Entrypoint to run all focused test scripts in setup/

script_path <- NULL
if (!is.null(sys.frames()) && length(sys.frames()) > 0) {
  script_path <- tryCatch(sys.frame(1)$ofile, error = function(e) NULL)
}
if (is.null(script_path)) {
  script_path <- "setup/run_tests.R"
}

repo_root <- normalizePath(
  file.path(dirname(script_path), ".."),
  winslash = "/",
  mustWork = FALSE
)

# Fallback for contexts where sys.frame(1)$ofile is unavailable
if (!dir.exists(repo_root) ||
      !file.exists(file.path(repo_root, "DESCRIPTION"))) {
  repo_root <- normalizePath(getwd(), winslash = "/", mustWork = TRUE)
}

setwd(repo_root)

cat("Running test entrypoint from:", repo_root, "\n")

scripts <- list.files(
  path = "setup",
  pattern = "^test_.*\\.R$",
  full.names = TRUE
)
scripts <- sort(scripts)

if (length(scripts) == 0) {
  stop("No test scripts found in setup/ matching pattern '^test_.*\\.R$'.")
}

cat("Discovered test scripts:\n")
for (s in scripts) {
  cat(" -", s, "\n")
}

results <- vector("list", length(scripts))

for (i in seq_along(scripts)) {
  script <- scripts[i]
  cat("\n=== Running", script, "===\n")

  res <- tryCatch(
    {
      source(script, local = new.env(parent = globalenv()))
      list(ok = TRUE, message = "PASS")
    },
    error = function(e) {
      list(ok = FALSE, message = conditionMessage(e))
    }
  )

  results[[i]] <- c(list(script = script), res)
}

cat("\n=== Test Summary ===\n")
failed <- 0L
for (r in results) {
  status <- if (isTRUE(r$ok)) "PASS" else "FAIL"
  cat(sprintf("[%s] %s\n", status, r$script))
  if (!isTRUE(r$ok)) {
    failed <- failed + 1L
    cat("      ", r$message, "\n", sep = "")
  }
}

if (failed > 0L) {
  stop(sprintf("%d test script(s) failed.", failed))
}

cat("All test scripts passed.\n")
