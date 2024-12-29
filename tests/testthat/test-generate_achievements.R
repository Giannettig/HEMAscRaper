# test-generate_achievements.R
library(testthat)
library(dplyr)
library(purrr)

###############################################################################
# 1) test_data has 'fighter_id' column
###############################################################################
test_that("test_data has 'fighter_id' column", {
  test_data <- HEMAscRaper::test_data
  
  expect_true(
    "fighter_id" %in% colnames(test_data),
    info = "The test_data is missing the 'fighter_id' column."
  )
})

###############################################################################
# 2) Check each ach_* function's output
###############################################################################
test_that("achievement functions produce valid output", {
  required_cols <- c(
    "fighter_id", "tier_id", "achieved", "percentile",
    "achievement_tier", "achievement_name", "achievement_description", "achievement_icon"
  )
  
  test_data <- HEMAscRaper::test_data
  # Retrieve all `ach_` prefixed functions from HEMAscRaper namespace
  achievement_list <- grep(
    "^ach_",
    ls(envir = asNamespace("HEMAscRaper"), all.names = TRUE),
    value = TRUE
  )
  
  achievement_functions <- stats::setNames(
    lapply(achievement_list, get, envir = asNamespace("HEMAscRaper")),
    achievement_list
  )
  
  purrr::walk2(achievement_functions, achievement_list, function(fn, fn_name) {
    # Run the function, gather debug info only if needed
    result <- tryCatch(
      fn(test_data),
      error = function(e) {
        fail(sprintf(
          "Error in function '%s': %s",
          fn_name, e$message
        ))
      }
    )
    
    # Prepare a debug string to show only on failure
    debug_info <- sprintf(
      "Debug info for %s:\n- Rows: %d\n- Cols: %d\n- Col names: %s",
      fn_name,
      if (!is.null(result)) nrow(result) else 0,
      if (!is.null(result)) ncol(result) else 0,
      if (!is.null(result)) paste(names(result), collapse = ", ") else "<none>"
    )
    
    if (is.null(result)) {
      fail(
        paste(debug_info, "\nFunction returned NULL (not a data frame).")
      )
      return(NULL)
    }
    
    # If the result has 0 rows, it must still have the required columns
    if (nrow(result) == 0) {
      missing_cols <- setdiff(required_cols, names(result))
      expect_true(
        length(missing_cols) == 0,
        info = paste(
          debug_info,
          sprintf("\nFunction '%s' returns an empty tibble missing columns: %s",
                  fn_name, paste(missing_cols, collapse = ", "))
        )
      )
    } else {
      # Non-empty: must have all required columns
      missing_cols <- setdiff(required_cols, names(result))
      expect_true(
        length(missing_cols) == 0,
        info = paste(
          debug_info,
          sprintf("\nFunction '%s' is missing columns: %s",
                  fn_name, paste(missing_cols, collapse = ", "))
        )
      )
      
      # No NAs in required columns
      for (col in required_cols) {
        expect_false(
          any(is.na(result[[col]])),
          info = paste(
            debug_info,
            sprintf("\nFunction '%s' has NA in column '%s'.",
                    fn_name, col)
          )
        )
      }
    }
  })
})

###############################################################################
# 3) Combine achievements => no multiple distinct descriptions
###############################################################################
test_that("each fighter has a unique description per achievement name", {
  test_data <- HEMAscRaper::test_data
  # Retrieve all `ach_` prefixed functions from HEMAscRaper namespace
  achievement_list <- grep(
    "^ach_",
    ls(envir = asNamespace("HEMAscRaper"), all.names = TRUE),
    value = TRUE
  )
  
  achievement_functions <- stats::setNames(
    lapply(achievement_list, get, envir = asNamespace("HEMAscRaper")),
    achievement_list
  )
  
  # We'll keep track of function name in .id
  achievement_functions <- stats::setNames(
    purrr::map(achievement_list, ~ get(.x, envir = asNamespace("HEMAscRaper"))),
    achievement_list
  )
  
  # Guarantee columns if needed
  required_cols <- c(
    "fighter_id", "tier_id", "achieved", "percentile",
    "achievement_tier", "achievement_name", "achievement_description", "achievement_icon"
  )
  
  guarantee_columns <- function(df) {
    if (is.null(df)) {
      # Return a zero-row df with required columns
      return(tibble::tibble(
        fighter_id = numeric(0),
        tier_id = numeric(0),
        achieved = logical(0),
        percentile = numeric(0),
        achievement_tier = character(0),
        achievement_name = character(0),
        achievement_description = character(0),
        achievement_icon = character(0)
      ))
    }
    missing_cols <- setdiff(required_cols, names(df))
    for (col in missing_cols) {
      if (col %in% c("fighter_id", "tier_id", "percentile")) {
        df[[col]] <- numeric(nrow(df))
      } else if (col == "achieved") {
        df[[col]] <- logical(nrow(df))
      } else {
        df[[col]] <- character(nrow(df))
      }
    }
    df <- df[, union(required_cols, names(df))]
    df
  }
  
  all_results <- purrr::imap_dfr(achievement_functions, function(fn, fn_name) {
    result <- tryCatch(fn(test_data), error = function(e) NULL)
    guarantee_columns(result)
  }, .id = "achievement_func")
  
  # We'll only show debug info on failure
  debug_shape <- sprintf(
    "All results shape: rows=%d, cols=%d. Columns: %s",
    nrow(all_results), ncol(all_results),
    paste(names(all_results), collapse = ", ")
  )
  
  # Now group by fighter_id, achievement_name, achievement_func
  # Each group can have only 1 distinct description
  library(dplyr)
  inconsistencies <- all_results %>%
    group_by(fighter_id, achievement_name, achievement_func) %>%
    summarize(distinct_desc = n_distinct(achievement_description), .groups = "drop") %>%
    filter(distinct_desc > 1)
  
  expect_true(
    nrow(inconsistencies) == 0,
    info = paste0(
      debug_shape,
      "\nMultiple distinct descriptions for the same fighter+achievement_name:\n",
      paste(
        sprintf(
          "- function: %s, fighter_id: %s, achievement_name: '%s'",
          inconsistencies$achievement_func,
          inconsistencies$fighter_id,
          inconsistencies$achievement_name
        ),
        collapse = "\n"
      )
    )
  )
})

###############################################################################
# 4) Debug achievement outputs (no empty dataframes allowed)
###############################################################################
test_that("debug achievement outputs (no empty dataframes allowed)", {
  test_data <- HEMAscRaper::test_data
  
  achievement_list <- grep("^ach_", ls("package:HEMAscRaper"), value = TRUE)
  achievement_functions <- purrr::set_names(
    purrr::map(achievement_list, ~ get(.x, envir = asNamespace("HEMAscRaper"))),
    achievement_list
  )
  
  purrr::walk2(achievement_functions, names(achievement_functions), function(fn, fn_name) {
    result <- tryCatch(
      fn(test_data),
      error = function(e) {
        fail(sprintf("Error calling %s: %s", fn_name, e$message))
        return(NULL)
      }
    )
    
    # We'll build debug info for potential failure message
    debug_info <- sprintf(
      "Function '%s' => Rows: %d, Cols: %d, Column names: %s",
      fn_name,
      if (!is.null(result)) nrow(result) else 0,
      if (!is.null(result)) ncol(result) else 0,
      if (!is.null(result)) paste(names(result), collapse = ", ") else "<none>"
    )
    
    if (is.null(result)) {
      fail(sprintf("%s\nReturned NULL instead of a dataframe.", debug_info))
    } else {
      # If we require exactly 8 columns (no empty)
      expect_true(
        ncol(result) == 8,
        info = paste0(debug_info, "\nExpected 8 columns, but got ", ncol(result))
      )
    }
  })
})