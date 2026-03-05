
testthat::test_that(desc = 'default panel', code = {
  panel() |>
    suppressMessages() |>
    testthat::expect_no_error()
})


testthat::test_that(desc = "default panel, as_flextable", code = {
  panel() |>
    suppressMessages() |>
    as_flextable.panel() |>
    testthat::expect_no_error()
})

