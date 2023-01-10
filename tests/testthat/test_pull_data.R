
data <- pull_data(year_selection = 2015)

# Test that all reports are included
# Test that all expected columns are included

test_that("pull_data is in expected format", {
  expect_type(data, "data.frame")
  expect_equal(colnames(data), c("report_name", "pwsid", "supplier_name", "year", "month", "category",
                 "volume_af", "use_group"))
  expect_equal(unique(data$report_name), c("UWMP", "WLA", "CR", "EAR"))
  expect_equal(unique(data$year), 2015)
  expect_equal(sapply(data, class), c(report_name = "character", pwsid = "character", supplier_name = "character",
                                year = "numeric", month = "numeric", category = "character",
                                volume_af = "numeric", use_group = "character"))
})
