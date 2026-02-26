library(targets)
library(dplyr)

tar_load(scored_resp)
tar_load(demo_data)

source("R/06_tables_dif_dim.R")

for (t_id in names(scored_resp)) {
  cat("Testing", t_id, "\n")
  res <- tryCatch({
    make_table_11_subgroup_reliability(scored_resp[[t_id]], demo_data, test_id = t_id)
  }, error = function(e) {
    cat("ERROR:\n")
    print(e)
    # capture traceback
    traceback()
  })
}