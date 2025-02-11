#' Generate a New Data Model and Save Processed Data as CSV Files
#'
#' This function processes a list of raw HEMA data frames into a new data model.
#' It transforms and joins tables for countries, events, tournaments, ratings, results,
#' fighters, clubs, and achievements; validates the data model using the \code{dm} package;
#' and then saves the processed tables as CSV files.
#'
#' @param data_list A named list of data frames. Expected elements include:
#'   \itemize{
#'     \item \code{hema_countries}
#'     \item \code{hema_events}
#'     \item \code{hema_tournaments}
#'     \item \code{hema_rankings}
#'     \item \code{hema_match_results}
#'     \item \code{hema_fighters}
#'     \item \code{hema_clubs}
#'     \item \code{hema_achievements}
#'   }
#' @param path - the path wehre files will be imported defaults to ./hema_ratings
#'
#' @return A list of processed data frames.
#'
#' @import dm
#' @import dplyr
#' @import readr
#' @import purrr
#' @import tibble
#' @import lubridate
#' @import tidyr
#' @import stringr
#' @importFrom utils capture.output
#'
#' @export
generate_new_model <- function(data_list, path="./hema_ratings") {
  
  ## --- Process Countries ---
  country_codes <- readr::read_csv(paste0(path,"/country_codes.csv"),show_col_types = FALSE )
  
  countries <- data_list$hema_countries %>%
    dplyr::left_join(
      country_codes %>% dplyr::select(-name, -region),
      by = c("country_code" = "alpha-2")
    ) %>%
    dplyr::rename(
      alpha_2             = country_code,
      alpha_3             = `alpha-3`,
      fencer_population   = population,
      hema_scene_id       = community,
      hema_scene          = community_label,
      intermediate_region = `intermediate-region`,
      country_code        = `country-code`
    ) %>%
    dplyr::mutate(
      country_id         = as.integer(country_id),
      name               = as.character(name),
      region             = as.character(region),
      sub_region         = as.character(sub_region),
      intermediate_region= as.character(intermediate_region),
      hema_scene_id      = as.integer(hema_scene_id),
      hema_scene         = as.character(hema_scene),
      country_code       = as.character(country_code),
      alpha_2            = as.character(alpha_2),
      alpha_3            = as.character(alpha_3),
      fencer_population  = as.integer(fencer_population),
      created_at         = Sys.time(),
      updated_at         = Sys.time(),
      deleted            = FALSE
    ) %>%
    dplyr::select(
      country_id, name, region, sub_region, intermediate_region,
      hema_scene_id, hema_scene, country_code, alpha_2, alpha_3,
      fencer_population, created_at, updated_at, deleted
    )
  
  ## --- Process Events ---
  events <- data_list$hema_events %>%
    dplyr::select(event_id, event_brand, event_year, event_name, event_date, event_country, event_city) %>%
    dplyr::left_join(
      countries %>% dplyr::select(country_id, name),
      by = c("event_country" = "name")
    ) %>%
    dplyr::rename(event_country_id = country_id) %>%
    dplyr::mutate(
      event_id        = as.integer(event_id),
      event_brand     = as.character(event_brand),
      event_year      = as.integer(event_year),
      event_name      = as.character(event_name),
      event_date      = as.Date(event_date),
      event_country_id= as.integer(event_country_id),
      event_city      = as.character(event_city),
      event_source_id = as.integer(event_id),
      event_source    = "Hema Ratings",
      created_at      = Sys.time(),
      updated_at      = Sys.time(),
      deleted         = FALSE
    ) %>%
    dplyr::select(
      event_id, event_brand, event_year, event_name, event_date, event_country_id,
      event_city, event_source_id, event_source, created_at, updated_at, deleted
    )
  
  ## --- Process Weapons ---
  weapons <- data_list$hema_tournaments %>%
    dplyr::select(tournament_weapon) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      weapon_id       = dplyr::row_number(),
      weapon_source_id= dplyr::row_number(),
      weapon_source   = "Hema Ratings",
      created_at      = Sys.time(),
      updated_at      = Sys.time(),
      deleted         = FALSE
    ) %>%
    dplyr::select(
      weapon_id, tournament_weapon, weapon_source_id, weapon_source, created_at, updated_at, deleted
    )
  
  ## --- Process Categories ---
  categories <- data_list$hema_tournaments %>%
    dplyr::select(tournament_category) %>%
    dplyr::distinct() %>%
    dplyr::mutate(
      category_id       = dplyr::row_number(),
      category_source_id= dplyr::row_number(),
      category_source   = "Hema Ratings",
      created_at        = Sys.time(),
      updated_at        = Sys.time(),
      deleted           = FALSE
    ) %>%
    dplyr::select(
      category_id, tournament_category, category_source_id, category_source, created_at, updated_at, deleted
    )
  
  ## --- Process Tournaments ---
  tournaments <- data_list$hema_tournaments %>%
    dplyr::left_join(categories %>% dplyr::select(tournament_category, category_id),
                     by = "tournament_category") %>%
    dplyr::left_join(weapons %>% dplyr::select(tournament_weapon, weapon_id),
                     by = "tournament_weapon") %>%
    dplyr::mutate(
      tournament_id         = as.integer(tournament_id),
      tournament_name       = as.character(tournament_name),
      event_id              = as.integer(event_id),
      category_id           = as.integer(category_id),
      weapon_id             = as.integer(weapon_id),
      tournament_note       = as.character(tournament_note),
      match_count           = as.integer(match_count),
      fighter_count         = as.integer(fighter_count),
      tournament_source_id  = as.integer(tournament_id),
      tournament_source     = "Hema Ratings",
      created_at            = Sys.time(),
      updated_at            = Sys.time(),
      deleted               = FALSE
    ) %>%
    dplyr::select(
      tournament_id, event_id, category_id, weapon_id, tournament_name, tournament_note,
      match_count, fighter_count, tournament_source_id, tournament_source, created_at, updated_at, deleted
    )
  
  ## --- Process Ratings ---
  ratings <- data_list$hema_rankings %>%
    dplyr::select(rank, fighter_id, fighter_name, category, month, weighted_rating, club, month_date) %>%
    dplyr::mutate(
      rating_id         = dplyr::row_number(),
      rank              = as.integer(rank),
      fighter_id        = as.integer(fighter_id),
      fighter_name      = as.character(fighter_name),
      ranking_category  = as.character(category),
      month             = as.character(month),
      weighted_rating   = as.numeric(weighted_rating),
      club              = as.character(club),
      month_date        = as.Date(month_date),
      ranking_source_id = as.integer(fighter_id),
      ranking_source    = "Hema Ratings",
      created_at        = Sys.time(),
      updated_at        = Sys.time(),
      deleted           = FALSE
    ) %>%
    dplyr::select(
      rating_id, fighter_id, ranking_category, rank, weighted_rating, month_date,
      ranking_source_id, ranking_source, created_at, updated_at, deleted
    )
  
  ## --- Process Results ---
  results <- data_list$hema_match_results %>%
    dplyr::mutate(
      result_id         = as.integer(match_result_id),
      match_id          = as.integer(match_id),
      match_date        = as.Date(event_date),
      event_id          = as.integer(event_id),
      tournament_id     = as.integer(tournament_id),
      stage             = as.character(stage),
      stage_type        = dplyr::case_when(
        is_final & result == "WIN"                   ~ "Gold",
        is_final & result == "LOSS"                  ~ "Silver",
        stringr::str_detect(stringr::str_to_lower(stage), "bronze") & result == "WIN" ~ "Bronze",
        TRUE                                        ~ NA_character_
      ),
      fighter_id        = as.integer(fighter_id),
      fighter_debut_fight = dplyr::if_else(is.na(debut_fight), FALSE, debut_fight),
      fighter_win_chance  = round(as.numeric(win_chance), 3),
      fighter_elo         = round(as.numeric(fighter_elo), 2),
      fighter_elo_gain    = round(as.numeric(fighter_elo_gain), 2),
      opponent_id         = as.integer(opponent_id),
      opponent_elo        = round(as.numeric(opponent_elo), 2),
      result              = as.character(result),
      result_source       = "Hema Ratings",
      created_at          = Sys.time(),
      updated_at          = Sys.time(),
      deleted             = FALSE
    ) %>%
    dplyr::select(
      result_id, match_id, match_date, event_id, tournament_id, stage, stage_type,
      fighter_id, fighter_debut_fight, fighter_win_chance, fighter_elo, fighter_elo_gain,
      opponent_id, opponent_elo, result, result_source, created_at, updated_at, deleted
    )
  
  ## --- Process Fighters ---
  fighters <- data_list$hema_fighters %>%
    dplyr::left_join(
      countries %>% dplyr::select(name, country_id),
      by = c("fighter_nationality" = "name")
    ) %>%
    dplyr::left_join(
      data_list$hema_match_results %>%
        dplyr::filter(debut_fight) %>%
        dplyr::select(fighter_id, event_date) %>%
        dplyr::rename(debut_date = event_date),
      by = "fighter_id"
    ) %>%
    dplyr::mutate(
      fighter_nationality = country_id,
      fighter_source_id   = fighter_id,
      fighter_source      = "Hema Ratings",
      created_at          = dplyr::if_else(is.na(debut_date), Sys.time(), as.POSIXct(debut_date)),
      updated_at          = Sys.time(),
      deleted             = FALSE
    ) %>%
    dplyr::select(
      fighter_id, fighter_name, fighter_nationality, fighter_club_id,
      fighter_source_id, fighter_source, created_at, updated_at, deleted
    )
  
  ## --- Process Clubs ---
  clubs <- data_list$hema_clubs %>%
    dplyr::select(club_id, club_name, club_country, club_state, club_city, club_members, club_parent_id) %>%
    dplyr::left_join(
      countries %>% dplyr::select(name, country_id),
      by = c("club_country" = "name")
    ) %>%
    dplyr::rename(club_country_id = country_id) %>%
    dplyr::mutate(
      club_source    = "Hema Ratings",
      club_source_id = as.integer(club_id),
      created_at     = Sys.time(),
      updated_at     = Sys.time(),
      deleted        = FALSE
    ) %>%
    dplyr::select(
      club_id, club_name, club_country_id, club_state, club_city, club_members,
      club_parent_id, club_source_id, club_source, created_at, updated_at, deleted
    )
  
  ## --- Process Achievements ---
  achievements <- data_list$hema_achievements %>%
    dplyr::select(
      fighter_id, tier_id, achieved, percentile, achievement_tier,
      achievement_name, achievement_description, achievement_icon
    ) %>%
    dplyr::mutate(
      achievement_id = dplyr::row_number(),
      fighter_id     = as.integer(fighter_id),
      tier_id        = as.integer(tier_id),
      achieved       = as.logical(achieved),
      percentile     = as.numeric(percentile),
      achievement_tier         = as.character(achievement_tier),
      achievement_name         = as.character(achievement_name),
      achievement_description  = as.character(achievement_description),
      achievement_icon         = as.character(achievement_icon),
      created_at     = Sys.time(),
      updated_at     = Sys.time(),
      deleted        = FALSE
    ) %>%
    dplyr::select(
      achievement_id, fighter_id, tier_id, achieved, percentile, achievement_tier,
      achievement_name, achievement_description, achievement_icon, created_at, updated_at, deleted
    )
  
  ## --- Identify Missing Fighters and Merge New Records ---
  missing_fighter_ids <- dplyr::bind_rows(
    achievements %>% dplyr::select(fighter_id),
    ratings %>% dplyr::select(fighter_id),
    results %>% dplyr::select(fighter_id, opponent_id) %>%
      tidyr::pivot_longer(dplyr::everything(), values_to = "fighter_id") %>%
      dplyr::select(fighter_id)
  ) %>%
    dplyr::distinct(fighter_id) %>%
    dplyr::anti_join(fighters, by = "fighter_id") %>%
    dplyr::mutate(fighter_id = dplyr::if_else(is.na(fighter_id), 0L, fighter_id)) %>%
    dplyr::pull(fighter_id)
  
  known_fighters <- data_list$hema_match_results %>%
    dplyr::filter(fighter_id %in% missing_fighter_ids) %>%
    dplyr::distinct(fighter_id, fighter_name, club_id) %>%
    dplyr::mutate(fighter_source = "System Generated")
  
  remaining_missing_ids <- setdiff(missing_fighter_ids, known_fighters$fighter_id)
  
  deleted_users <- tibble::tibble(
    fighter_id   = remaining_missing_ids,
    fighter_name = "Deleted User",
    club_id      = NA_integer_,
    fighter_source = "System Generated"
  )
  
  new_fighters <- dplyr::bind_rows(known_fighters, deleted_users) %>%
    dplyr::mutate(
      fighter_nationality = NA_integer_,
      fighter_source_id  = fighter_id,
      created_at         = lubridate::now(),
      updated_at         = lubridate::now(),
      deleted            = TRUE
    )
  
  fighters <- dplyr::bind_rows(fighters, new_fighters)
  
  ## --- Validate the Data Model ---
  data_model <- dm::dm(
    achievements,
    categories,
    clubs,
    countries,
    events,
    fighters,
    ratings,
    results,
    tournaments,
    weapons
  ) %>%
    dm::dm_add_pk(achievements, achievement_id) %>%
    dm::dm_add_pk(categories, category_id) %>%
    dm::dm_add_pk(clubs, club_id) %>%
    dm::dm_add_pk(countries, country_id) %>%
    dm::dm_add_pk(events, event_id) %>%
    dm::dm_add_pk(fighters, fighter_id) %>%
    dm::dm_add_pk(ratings, rating_id) %>%
    dm::dm_add_pk(results, result_id) %>%
    dm::dm_add_pk(tournaments, tournament_id) %>%
    dm::dm_add_pk(weapons, weapon_id) %>%
    dm::dm_add_fk(achievements, fighter_id, fighters) %>%
    dm::dm_add_fk(clubs, club_country_id, countries) %>%
    dm::dm_add_fk(events, event_country_id, countries) %>%
    dm::dm_add_fk(fighters, fighter_club_id, clubs) %>%
    dm::dm_add_fk(fighters, fighter_nationality, countries) %>%
    dm::dm_add_fk(ratings, fighter_id, fighters) %>%
    dm::dm_add_fk(results, fighter_id, fighters) %>%
    dm::dm_add_fk(results, opponent_id, fighters) %>%
    dm::dm_add_fk(results, event_id, events) %>%
    dm::dm_add_fk(results, tournament_id, tournaments) %>%
    dm::dm_add_fk(tournaments, event_id, events) %>%
    dm::dm_add_fk(tournaments, weapon_id, weapons) %>%
    dm::dm_add_fk(tournaments, category_id, categories)

  
  # Check the data model for constraint issues.
  constraints <- dm::dm_examine_constraints(data_model) %>%
    dplyr::filter(problem != "")
  
  if (nrow(constraints) > 0) {
    warning(
      "Constraint issues found:\n",
      paste(capture.output(print(constraints)), collapse = "\n")
    )
  } else {
    message("Data model is correctly configured and has no constraint issues!")
  }

  
  ## --- Save Processed Data as CSV Files ---
  data_list_out <- list(
    clubs        = clubs,
    countries    = countries,
    events       = events,
    fighters     = fighters,
    results      = results,
    tournaments  = tournaments,
    ratings      = ratings,
    weapons      = weapons,
    categories   = categories,
    achievements = achievements
  )
  
  path2 <- paste0(path,"/v2")
  if (!dir.exists(path2)) {
    dir.create(path2, recursive = TRUE)
  }
  
  message(paste0("Saving the data in the ", path2, " directory"))
  
  purrr::walk(names(data_list_out), function(name) {
    file_path <- file.path(path2, paste0(name, ".csv"))
    readr::write_csv(data_list_out[[name]], file = file_path, na = "", quote = "needed", escape = "double")
  })
  
  invisible(NULL)
}