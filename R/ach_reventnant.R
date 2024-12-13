ach_revenant <- function(data) {
  # Define all tiers and their conditions
  tiers <- tribble(
    ~achievement_tier, ~tier_id, ~achievement_name,       ~achievement_description,                                             ~achievement_icon,
    "Bronze",           1,        "The Revenant",       "You come back to fight after at least two years of inactivity",       "bronze_revenant.png",
    "Epic",             4,        "The Revenant",       "You come back to fight after ten years of inactivity",                "epic_revenant.png"
  )
  
  total_fighters <- data %>% distinct(fighter_id) %>% nrow() # Total unique fighters
  
  ach <- data %>%
    select(fighter_id, event_date) %>%
    distinct() %>%
    arrange(fighter_id, event_date) %>%
    group_by(fighter_id) %>%
    mutate(interval_between_fights = as.numeric(event_date - lag(event_date), units = "days")) %>%
    filter(!is.na(interval_between_fights)) %>%
    summarize(max_interval = max(interval_between_fights, na.rm = TRUE)) %>%
    mutate(
      tier_id = case_when(
        max_interval >= 3650 ~ 4,  # Epic tier (10 years)
        max_interval >= 730 ~ 1,   # Bronze tier (2 years)
        TRUE ~ NA_integer_
      ),
      achieved = !is.na(tier_id)
    ) %>%
    filter(achieved) %>%
    mutate(percentile = n() / total_fighters) %>% # Percentile calculation
    left_join(tiers, by = "tier_id")%>%select(-max_interval)
  
  return(ach)
}
