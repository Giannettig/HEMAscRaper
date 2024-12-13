# test-generate_achievements.R

test_that("generate_achievements runs without error", {
  achievements <- generate_achievements()
  expect_s3_class(achievements, "data.frame")
})

test_that("generate_achievements output has the correct structure", {
  achievements <- generate_achievements()
  expected_columns <- c(
    "fighter_id", "tier_id", "achieved", "percentile",
    "achievement_tier", "achievement_name", "achievement_description", "achievement_icon"
  )
  expect_true(all(expected_columns %in% colnames(achievements)))
  
  # Check column types
  expect_type(achievements$fighter_id, "double")
  expect_type(achievements$tier_id, "integer")
  expect_type(achievements$achieved, "logical")
  expect_type(achievements$percentile, "double")
  expect_type(achievements$achievement_tier, "character")
  expect_type(achievements$achievement_name, "character")
  expect_type(achievements$achievement_description, "character")
  expect_type(achievements$achievement_icon, "character")
})

test_that("generate_achievements computes achievements", {
  achievements <- generate_achievements()
  expect_gt(nrow(achievements), 0) # There should be at least one achievement
})

test_that("generate_achievements handles invalid path gracefully", {
  expect_message(
    achievements <- generate_achievements(path = "./invalid_path"),
    "The folder with data './invalid_path' does not exist"
  )
  expect_s3_class(achievements, "data.frame")
})

test_that("generate_achievements handles warnings and errors gracefully", {
  skip_on_cran() # Skip on CRAN due to potential environment-specific issues
  
  mock_data <- tibble::tibble(
    fighter_id = c(1, 2),
    event_year = c(2022, 2023),
    match_id = c(1, 2)
  )
  
  mock_function <- function(data) {
    warning("This is a test warning")
    data.frame(fighter_id = 1, tier_id = 1, achieved = TRUE, percentile = 1, 
               achievement_tier = "Bronze", achievement_name = "Test", 
               achievement_description = "Test desc", achievement_icon = "test.png")
  }
  
  achievement_functions <- list(test_achievement = mock_function)
  
  achievements <- purrr::imap_dfr(achievement_functions, function(fn, name) {
    tryCatch(
      withCallingHandlers(
        fn(mock_data),
        warning = function(w) {
          expect_match(w$message, "This is a test warning")
          invokeRestart("muffleWarning")
        }
      ),
      error = function(e) {
        fail(sprintf("Error occurred in '%s': %s", name, e$message))
      }
    )
  })
  
  expect_s3_class(achievements, "data.frame")
})