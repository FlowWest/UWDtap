
summary_data <- pull_data_summary()

# Test that all reports are included
# Test that all expected columns are included

test_that("pull_data_summary is in expected format", {
  expect_type(summary_data, "data.frame")
  expect_equal(colnames(summary_data), c("report_name", "year", "month", "category", "use_type", "n",
                                         "use_group", "statistic", "volume_af"))
  expect_equal(unique(summary_data$report_name), c("CR", "UWMP", "WLA", "EAR"))
  expect_equal(unique(summary_data$year), c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021, 2022, 2023,
  2013))
  expect_equal(sapply(summary_data, class), c(report_name = "character", year = "numeric", month = "numeric",
                                              category = "character", n = "integer",
                                              use_group = "character", statistic = "character", volume_af = "numeric"
  ))
})
