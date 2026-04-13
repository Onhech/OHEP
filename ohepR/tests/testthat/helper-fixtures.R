sample_dashboard_data <- function() {
  ex <- example_fundamental_inputs()
  runtime <- ohepRDisplayr()
  runtime$build_dashboard_data_from_inputs(
    fundamental = "Purpose",
    index_data = ex$index_data,
    user_data = ex$user_data
  )
}
