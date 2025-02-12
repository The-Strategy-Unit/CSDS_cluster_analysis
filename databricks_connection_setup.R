# Linking to Databricks

# Follow instructions from: https://github.com/The-Strategy-Unit/sconn

remotes::install_github("The-Strategy-Unit/sconn")


library(reticulate)
reticulate::install_python("3.12") # to match Databricks version

reticulate::virtualenv_create(
  envname = Sys.getenv("DATABRICKS_VENV"),
  python = "3.12", # match this to the version of Python installed above
  packages = "pyspark",
  force = TRUE
)

pysparklyr::install_databricks(
  version = "15.4", # match the version of Databricks used in your instance
  envname = Sys.getenv("DATABRICKS_VENV"),
  new_env = FALSE
)
