# 
# 
# 
# year <- "2024"
# 
# # Create Nodes File
# nodes <- hema_match_results %>%
#   left_join(hema_clubs, by = "club_id") %>%
#   group_by(club_country) %>%
#   summarise(Population = n_distinct(fighter_id), .groups = "drop") %>%
#   left_join(hema_countries, by = c("club_country" = "name")) %>%
#   mutate(Id = row_number()) %>%
#   rename(Label = club_country,
#          Region = region,
#          Sub_Region = `sub_region`) %>%
#   select(Id, Label, Region, Sub_Region, Population)
# 
# # Create Undirected Edges File - will merge directions
# edges <- hema_match_results %>%
#   left_join(hema_clubs, by = "club_id") %>%
#   left_join(hema_events, by = "event_id") %>%
#   filter(club_country!=event_country)%>%
#   mutate(
#     Target = map2_chr(club_country,event_country ,function(x,y) {paste(sort(c(x, y)), collapse = " | ")})
#   ) %>%
#   group_by(Target, event_year) %>%
#   summarise(Weight = n_distinct(fighter_id), .groups = "drop") %>%
#   mutate(
#     Source_Label= str_split_i(Target, "\\|", 1)%>%str_squish,
#     Target_Label= str_split_i(Target, "\\|", 2)%>%str_squish
#   )%>%select(-Target)%>%
#   left_join(nodes, by = c("Source_Label" = "Label")) %>%
#   rename(Source = Id) %>%
#   left_join(nodes, by = c("Target_Label" = "Label")) %>%
#   rename(Target = Id) %>%
#   select(Source, Target, Source_Label, Target_Label, Weight)
# 
# # Create Nodes File for 2024
# nodes_2024 <- hema_match_results %>%
#   left_join(hema_clubs, by = "club_id") %>%
#   left_join(hema_events, by = "event_id") %>%
#   filter(event_year == year) %>%
#   group_by(club_country) %>%
#   summarise(Population = n_distinct(fighter_id), .groups = "drop") %>%
#   left_join(hema_countries, by = c("club_country" = "name")) %>%
#   mutate(Id = row_number()) %>%
#   rename(Label = club_country,
#          Region = region,
#          Sub_Region = `sub_region`) %>%
#   select(Id, Label, Region, Sub_Region, Population)
# 
# # Create Edges File for 2024
# edges_2024 <- hema_match_results %>%
#   left_join(hema_clubs, by = "club_id") %>%
#   left_join(hema_events, by = "event_id") %>%
#   filter(event_year == year) %>%
#   group_by(club_country, event_country) %>%
#   summarise(Weight = n_distinct(fighter_id), .groups = "drop") %>%
#   filter(club_country != event_country) %>%
#   left_join(nodes_2024, by = c("club_country" = "Label")) %>%
#   rename(Source = Id) %>%
#   left_join(nodes_2024, by = c("event_country" = "Label")) %>%
#   rename(Target = Id,
#          Club_Country=club_country,
#          Event_Country=event_country) %>%
#   select(Source, Target,Club_Country,Event_Country, Weight)
# 
# 
# nodes<-nodes%>%filter(!is.na(Label))
# nodes_2024<-nodes_2024%>%filter(!is.na(Label))
# 
# 
