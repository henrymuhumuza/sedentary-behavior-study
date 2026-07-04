source(file.path("R", "analysis_helpers.R"))
root <- init_project()

master <- load_master()
master <- derive_analysis_data(master)
analytic <- analysis_sample(master)

writeLines(
  c(
    paste("Project root:", root),
    paste("Master rows:", nrow(master)),
    paste("Master columns:", ncol(master)),
    paste("Analytic rows after core exclusions:", nrow(analytic)),
    "Physical activity derivation note: the current master dataset does not retain PAD790U/PAD810U unit variables; frequency variables are treated as weekly frequencies unless raw PAQ unit fields are reintroduced."
  ),
  con = root_path("outputs", "logs", "00_setup_log.txt")
)
