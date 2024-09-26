#' HEMA Clubs Dataset
#'
#' This dataset contains information about Historical European Martial Arts (HEMA) clubs, including their locations, members, and parent organizations.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{\code{club_id}}{Unique identifier of the club. Integer.}
#'   \item{\code{club_name}}{Name of the club. Character.}
#'   \item{\code{club_country}}{Country where the club is located. Character.}
#'   \item{\code{club_state}}{State or region of the club, if applicable. Character.}
#'   \item{\code{club_city}}{City where the club is based. Character.}
#'   \item{\code{club_members}}{Number of registered fighters associated with the club. Integer.}
#'   \item{\code{club_parent_id}}{Identifier of the parent club, if the club is a sub-organization. Integer.}
#'   \item{\code{club_url}}{URL of the club's profile on the HEMA Ratings website. Character.}
#' }
#'
#' @details
#' The dataset is sourced from the HEMA Ratings website and provides comprehensive information on the clubs registered in the system.
#' It can be used for geographic visualizations, network analysis of club affiliations, or understanding the distribution of HEMA clubs.
#'
#' @source [HEMA Ratings Clubs Page](https://hemaratings.com/clubs/)
#'
#' @examples
#' data(hema_clubs)
#' head(hema_clubs)
#'
"hema_clubs"

#' HEMA Events Dataset
#'
#' This dataset contains information about Historical European Martial Arts (HEMA) events, including dates, locations, and the number of tournaments held.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{\code{event_id}}{Unique identifier of the event. Integer.}
#'   \item{\code{event_name}}{Name of the event. Character.}
#'   \item{\code{event_year}}{Year when the event took place. Integer.}
#'   \item{\code{event_date}}{Date of the event (format: "YYYY-MM-DD"). Date.}
#'   \item{\code{event_country}}{Country where the event was held. Character.}
#'   \item{\code{event_city}}{City where the event took place. Character.}
#'   \item{\code{event_url}}{URL of the event's profile on the HEMA Ratings website. Character.}
#' }
#'
#' @details
#' This dataset is sourced from the HEMA Ratings events page and provides a timeline of significant HEMA events worldwide.
#' It is useful for tracking the growth and spread of HEMA, visualizing event locations over time, and identifying prominent annual competitions.
#'
#' @source [HEMA Ratings Events Page](https://hemaratings.com/events/)
#'
#' @examples
#' data(hema_events)
#' summary(hema_events)
#'
"hema_events"

#' HEMA Fighters Dataset
#'
#' This dataset contains information about registered Historical European Martial Arts (HEMA) fighters, including their names, nationalities, and affiliated clubs.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{\code{fighter_id}}{Unique identifier of the fighter. Integer.}
#'   \item{\code{fighter_name}}{Name of the fighter. Character.}
#'   \item{\code{fighter_nationality}}{Nationality of the fighter (based on country flag displayed). Character.}
#'   \item{\code{fighter_club_id}}{Unique identifier of the club, if available. Integer.}
#'   \item{\code{fighter_club_name}}{Name of the fighter's club, if available. Character.}
#'   \item{\code{fighter_url}}{URL of the fighter's profile on the HEMA Ratings website. Character.}
#' }
#'
#' @details
#' This dataset provides a comprehensive overview of the fighters registered on the HEMA Ratings website.
#' It can be used for demographic analysis of HEMA practitioners, visualizing the distribution of fighters, and linking fighters to their clubs.
#'
#' @source [HEMA Ratings Fighters Page](https://hemaratings.com/fighters/)
#'
#' @examples
#' data(hema_fighters)
#' head(hema_fighters)
#'
"hema_fighters"

#' HEMA Fights Dataset
#'
#' This dataset contains information about individual fights in Historical European Martial Arts (HEMA) tournaments, including fighters' scores, match outcomes, and tournament details.
#'
#' @format A data frame with the following columns:
#' \describe{
#'   \item{\code{event_id}}{Unique identifier of the event. Integer.}
#'   \item{\code{event_name}}{Name of the event. Character.}
#'   \item{\code{tournament_name}}{Name of the specific tournament or discipline within the event. Character.}
#'   \item{\code{event_category}}{Category of the tournament (e.g., Mixed, Men's, Women's, Underrepresented Genders). Character.}
#'   \item{\code{event_note}}{Other notes. Character.}
#'   \item{\code{event_weapon}}{Weapon used in the tournament (e.g., Longsword, Rapier). Character.}
#'   \item{\code{fighter_id}}{Unique identifier of the first fighter in a match. Integer.}
#'   \item{\code{opponent_id}}{Unique identifier of the opponent fighter in a match. Integer.}
#'   \item{\code{stage}}{Name of the stage. Character.}
#'   \item{\code{fighter_1}}{Name of the fighter Character.}
#'   \item{\code{fighter_2}}{Name of the fighter Character.}
#'   \item{\code{fighter_1_result}}{Score of the first fighter. Character.}
#'   \item{\code{fighter_2_result}}{Score of the opponent fighter. Character.}
#' }
#'
#' @details
#' The dataset provides detailed match-level statistics for HEMA tournaments. It can be used to analyze match outcomes, fighter performance,
#' and trends across different weapon categories and tournament types.
#'
#' @source [HEMA Ratings Tournament Page](https://hemaratings.com/tournaments/)
#'
#' @examples
#' data(hema_fights)
#' summary(hema_fights)
#'
"hema_fights"
