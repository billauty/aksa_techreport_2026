# Build tables and figures for one test from model results.
# model_results: list returned by fit_models()
# Returns a list containing:
#   - ft: a flextable object
#   - fig: a ggplot object
#   - alt_text: character string describing the figure
build_test_content <- function(model_results) {
  # TODO: implement table and figure generation
  ft <- flextable::flextable(data.frame())

  fig <- ggplot2::ggplot()

  list(
    ft       = ft,
    fig      = fig,
    alt_text = "Placeholder figure"
  )
}
