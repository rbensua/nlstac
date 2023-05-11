test_that("returns nlstac class object", {
  x <- seq(from = 0, to = 40, length.out = 100)
  y <- 5 * exp(-0.1 * x) + 5
  df <- data.frame(time = x, Temp = y)
  nlparam <- list(b = c(0,2))
  fittac <- nls_tac('Temp ~ a1 * exp(-b * time) + a2',
                    data = df,
                    nlparam = nlparam,
                    quiet = TRUE,
                    silent = TRUE,
                    N = 20)
  expect_s3_class(fittac, "nlstac")
})


test_that("length of nlstac object is 12",{
  x <- seq(from = 0, to = 40, length.out = 100)
  y <- 5 * exp(-0.1 * x) + 5
  df <- data.frame(time = x, Temp = y)
  nlparam <- list(b = c(0,2))
  fittac <- nls_tac('Temp ~ a1 * exp(-b * time) + a2',
                    data = df,
                    nlparam = nlparam,
                    quiet = TRUE,
                    silent = TRUE,
                    N = 20)
  expect_length(fittac, 12)
  
})


test_that("field names of the nlstac object are correct",{
  x <- seq(from = 0, to = 40, length.out = 100)
  y <- 5 * exp(-0.1 * x) + 5
  df <- data.frame(time = x, Temp = y)
  nlparam <- list(b = c(0,2))
  fittac <- nls_tac('Temp ~ a1 * exp(-b * time) + a2',
                    data = df,
                    nlparam = nlparam,
                    quiet = TRUE,
                    silent = TRUE,
                    N = 20)
  fieldnames <- c("coefficients", "stdError", "convInfo", 
                  "SSR", "fitted", "resid", "dataset", "data", "formula",
                  "df", "sigma", "Rmat")
  expect_setequal(names(fittac), fieldnames) 
})

test_that("field classes of the nlstac object are correct", {
  x <- seq(from = 0, to = 40, length.out = 100)
  y <- 5 * exp(-0.1 * x) + 5
  df <- data.frame(time = x, Temp = y)
  nlparam <- list(b = c(0,2))
  fittac <- nls_tac('Temp ~ a1 * exp(-b * time) + a2',
                    data = df,
                    nlparam = nlparam,
                    quiet = TRUE,
                    silent = TRUE,
                    N = 20)
  expect_type(fittac$coefficients, "double")
  expect_type(fittac$stdError, "double")
  expect_type(fittac$convInfo, "list")
  expect_type(fittac$SSR, "double")
  expect_type(fittac$fitted, "double")
  expect_s3_class(fittac$resid, "data.frame")
  expect_type(fittac$dataset, "symbol")
  expect_s3_class(fittac$data, "data.frame")
  expect_s3_class(fittac$formula, "formula")
  expect_type(fittac$df, "integer")
  expect_type(fittac$sigma, "double")
  expect_type(fittac$Rmat, "double")
})


test_that("solution of a sample problem",{
  x <- seq(from = 0, to = 40, length.out = 100)
  y <- 5 * exp(-0.1 * x) + 5
  df <- data.frame(time = x, Temp = y)
  nlparam <- list(b = c(0,2))
  fittac <- nls_tac('Temp ~ a1 * exp(-b * time) + a2',
                    data = df,
                    nlparam = nlparam,
                    quiet = TRUE,
                    silent = TRUE,
                    N = 20)
  expected_sol <- c(b = 0.1, a1 = 5, a2 = 5)
  expect_equal(coefficients(fittac), expected_sol, tolerance = 1e-4)
})

test_that("is silent when asked to",{
  x <- seq(from = 0, to = 40, length.out = 100)
  y <- 5 * exp(-0.1 * x) + 5
  df <- data.frame(time = x, Temp = y)
  nlparam <- list(b = c(0,2))
  expect_message(nls_tac('Temp ~ a1 * exp(-b * time) + a2',
                    data = df,
                    nlparam = nlparam,
                    quiet = FALSE,
                    silent = TRUE,
                    N = 20), 
                 regexp = "iteration = "
  )
})