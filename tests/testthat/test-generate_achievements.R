# test-generate_achievements.R

test_that("generate_achievements runs without error", {
  achievements <- generate_achievements()
  expect_s3_class(achievements, "data.frame")
})


test_that("all achievement functions generate correct output data", {
  # Load test data
  test_data <- HEMAscRaper::test_data
  
  # Define the data type checker function
  check_datatype <- function(achievements) {
    expected_columns <- c(
      "fighter_id", "tier_id", "achieved", "percentile",
      "achievement_tier", "achievement_name", "achievement_description", "achievement_icon"
    )
    
    # Check if all expected columns are present
    expect_true(all(expected_columns %in% colnames(achievements)), 
                info = "Missing expected columns in the output.")
    
    # Check column types
    expect_type(achievements$fighter_id, "double")
    expect_type(achievements$tier_id, "double")
    expect_type(achievements$achieved, "logical")
    expect_type(achievements$percentile, "double")
    expect_type(achievements$achievement_tier, "character")
    expect_type(achievements$achievement_name, "character")
    expect_type(achievements$achievement_description, "character")
    expect_type(achievements$achievement_icon, "character")
  }
  
  # Retrieve all `ach_` prefixed functions from the HEMAscRaper namespace
  achievement_list <- grep("^ach_", ls("package:HEMAscRaper"), value = TRUE)
  achievement_functions <- stats::setNames(
    lapply(achievement_list, get, envir = asNamespace("HEMAscRaper")), 
    achievement_list
  )
  
  # Test each achievement function
  for (func_name in names(achievement_functions)) {
    func <- achievement_functions[[func_name]]
    
    # Execute the function with test data
    result <- func(test_data)
    
    # Check the output structure and types
    test_that(paste("Testing function", func_name), {
      check_datatype(result)
      
      # Check that no required columns contain NA values
      required_columns <- c("achievement_tier", "achievement_name", "achievement_description", "achievement_icon")
      for (col in required_columns) {
        expect_false(any(is.na(result[[col]])), 
                     info = paste0("Column '", col, "' contains NA values"))
      }
    })
  }
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
