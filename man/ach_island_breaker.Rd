% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ach_island_breaker.R
\name{ach_island_breaker}
\alias{ach_island_breaker}
\title{Island Breaker Achievement}
\usage{
ach_island_breaker(data)
}
\arguments{
\item{data}{A data frame with at least the following columns:
\describe{
  \item{fighter_id}{Identifier of the fighter.}
  \item{event_year}{Year of the event.}
  \item{event_community}{Community where the event was held.}
}}
}
\value{
A data frame with columns:
\describe{
  \item{fighter_id}{ID of the fighter.}
  \item{tier_id}{Numeric tier ID.}
  \item{achieved}{Logical, TRUE if achieved.}
  \item{percentile}{Proportion of fighters who achieved this tier.}
  \item{achievement_tier}{Tier name (Bronze, Silver, Gold, Epic).}
  \item{achievement_name}{Name of the achievement.}
  \item{achievement_description}{Description of the achievement.}
  \item{achievement_icon}{Filename of the achievement icon.}
}
}
\description{
Internal function to award the "Island Breaker" achievement to fighters who fought in different HEMA scenes in a given year.
- Bronze: Participated in 2 scenes.
- Silver: Participated in 3 scenes.
- Gold: Participated in 4 scenes.
- Epic: Participated in 5 or more scenes.
}
\keyword{internal}
