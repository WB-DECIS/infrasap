test_that("empty_plot() works", {
  expect_s3_class(empty_plot(), "plotly")
})

test_that("has_port() retruns correct result", {
  string <- c('Port of Rotterdam', 'Amsterdam', 'Port')
  expect_true(has_port(string)) #Returns TRUE if any one string has true
})

test_that("open_bracket() retruns correct result", {
  string <- c('This is a new (not new) thing', 'Strange way', '(Different text.')
  expect_equal(open_bracket(string), c(TRUE, FALSE, TRUE))
})


test_that("select_and_round() retruns correct result", {
 df <- data.frame(a = rnorm(5), b = runif(5), value1 = 10, new = 5)
 res <- select_and_round(df, c('a', 'b'))
 expect_equal(dim(res), c(5, 3))
 expect_equal(names(res), c('a', 'b', 'value1'))
})

test_that("year_max_column() retruns correct result", {
  df <- data.frame("2022" = rnorm(5), "2021" = runif(5), "2020" = 10)
  res <- year_max_column(df, 2020:2022)
  expect_type(res, 'character')
  expect_equal(res, "2020")
})

test_that("get_colors() returns correct results", {
  expect_length(get_colors(1:5), 5)
  expect_length(get_colors(1), 3)
  expect_length(get_colors(1:2), 3)
  expect_length(get_colors(1:3), 3)
})
