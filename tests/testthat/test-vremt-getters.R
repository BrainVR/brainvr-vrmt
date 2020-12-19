exps <- load_vremt_experiments("../../inst/extdata/raw/")
exp <- exps[[3]]

test_that("Simple getters", {
  out <- get_actions_log(exp)
  expect_s3_class(out, "data.frame")
})

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

  expect_warning(obj <- get_phase_data(exp, "non existent phase name"))
  expect_null(obj)

  expect_warning(obj <- get_phase_data(exp, "recallItems", 999))
  expect_null(obj)
})

test_that("Getting tasks", {
  expect_silent(task <- get_task_settings(exp))
  expect_s3_class(task, "data.frame")
  expect_silent(task <- get_task_settings(exp, 1))
  expect_equal(nrow(task), 1)

  expect_warning(task <- get_task_settings(exp, 4))
  expect_null(task)
})

test_that("Geting item information", {
  obj <- get_recallItems_data(exp, 1)
  items <- get_collected_items(obj)
  expect_equal(items, c("dinosaur", "globe", "ball", "cream"))
  items <- get_collected_items(get_recallItems_data(exp, 2))
  expect_length(items, 5)
})

test_that("Getting item and location positions", {
  location <- "cemetery"
  item <- "globe"
  expect_silent(res <- get_location_position(location))
  expect_silent(res2 <- get_item_position(item))
  expect_vector(res, size = 3)
  expect_equal(res, res2)

  expect_silent(res <- get_location_position(location, simplify = FALSE))
  expect_s3_class(res, "data.frame")

  expect_silent(res <- get_item_position(item, simplify = FALSE))
  expect_s3_class(res, "data.frame")

  expect_warning(res <- get_location_position("non existent location"))
  expect_null(res)

  expect_warning(res <- get_location_position(c(location, item)))
  expect_null(res)

  expect_silent(res <- get_location_position(c(location, "hotel")))
  expect_s3_class(res, "data.frame")

  expect_silent(res <- get_item_position(c(item, "beer")))
  expect_s3_class(res, "data.frame")
})
