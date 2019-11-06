# Install packages to a docker image with packrat

# Install packrat
install.packages("packrat", repos = "https://cran.rstudio.com/")

# Initialize packrat, but don't let it try to find packages to install itself.
packrat::init(
  infer.dependencies = FALSE,
  enter = TRUE,
  restart = FALSE)

# Install CRAN packages
list.of.packages <- c(
  "caret",
  "mlbench",
  "fscaret",
  "dplyr",
  "psych",
  "corrplot",
  "xgboost",
  "ROCR",
  "pROC"
)

new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[, "Package"])]
if (length(new.packages)) install.packages(new.packages, repos = "https://cran.rstudio.com/", dependencies = T)

# Take snapshot

packrat::snapshot(
  snapshot.sources = FALSE,
  ignore.stale = TRUE,
  infer.dependencies = FALSE)
