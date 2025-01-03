% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ach_hero_of_the_nation.R
\name{ach_hero_of_the_nation}
\alias{ach_hero_of_the_nation}
\title{Every Nation Needs a Hero Achievement}
\usage{
ach_hero_of_the_nation(data)
}
\arguments{
\item{data}{A data frame with at least the following columns:
\describe{
  \item{fighter_id}{Identifier of the fighter.}
  \item{event_year}{Year of the event.}
  \item{club_country}{Country of the fighter's club.}
  \item{result}{Result of the fight (e.g., "WIN").}
}}
}
\value{
A data frame with columns:
\describe{
  \item{fighter_id}{ID of the fighter.}
  \item{tier_id}{Numeric tier ID.}
  \item{achieved}{Logical, TRUE if achieved.}
  \item{percentile}{Proportion of fighters who achieved this tier.}
  \item{achievement_tier}{Tier name (Unique).}
  \item{achievement_name}{Name of the achievement.}
  \item{achievement_description}{Description of the achievement.}
  \item{achievement_icon}{Filename of the achievement icon.}
}
}
\description{
Internal function to award the "Every Nation Needs a Hero" achievement to fighters who won the most fights in their country (by `club_country`) in a given year.
}
\keyword{internal}
