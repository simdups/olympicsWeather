library(tidyverse)
test_that("multiplication works", {
  resp <- perform_request(48.8566, 2.3522)
  expect_equal(nrow(unnest_response(resp)), 168)

  expect_identical(colnames(unnest_response(resp)),
                   c("date_heure", "temperature_celsius",
                      "temperature_ressentie_celsius",
                      "precipation_proba", "precipitation"))

  expect_equal(ncol(unnest_response(resp)), 5)
})
