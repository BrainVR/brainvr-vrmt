test_that("Testing geom location", {
  #single locaiton plotting
  # two location plotting
  expect_silent(res <- geom_vremt_location("hotel"))
  expect_s3_class(res, "list")
})
