exps <- load_vremt_experiments("../../inst/extdata/raw/")
exp <- exps[[1]]

test_that("Getting phases", {
  expect_warning(obj <- get_phase_data(exp, "recall"))
  expect_null(obj)
  expect_silent(obj <- get_phase_data(exp, "recall", 1))
  expect_s3_class(obj, class(exp))

  expect_silent(obj <- get_phase_data(exp, "recallItems", 1))
  expect_silent(obj2 <- get_recallItems_data(exp, 1))
  expect_equal(obj, obj2)

  expect_silent(obj <- get_phase_data(exp, "recallPlacement", 1))
  expect_silent(obj2 <- get_recallPlacement_data(exp, 1))
  expect_equal(obj, obj2)
})
