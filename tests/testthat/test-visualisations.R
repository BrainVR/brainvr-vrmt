
test_that("Testing geom location", {
  #single locaiton plotting
  # two location plotting
  expect_silent(res <- geom_vremt_location("hotel", version = 2024))
  expect_is(res, "list")
  expect_silent(res <- geom_vremt_location("hotel", version = 2020))
  expect_is(res, "list")
  expect_silent(res <- geom_vremt_location(c("mill", "well"), version = 2020))
  expect_is(res, "list")
})
