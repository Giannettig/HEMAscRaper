% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ach_on_the_streak.R
\name{ach_on_the_streak}
\alias{ach_on_the_streak}
\title{On the Streak Achievement}
\usage{
ach_on_the_streak(data)
}
\arguments{
\item{data}{A data frame with at least the following columns:
\describe{
  \item{fighter_id}{Identifier of the fighter}
  \item{match_id}{Identifier of the match}
  \item{event_date}{Date of the event (as Date or character)}
  \item{result}{Result of the match ("WIN" or "LOSS")}
}}
}
\value{
A data frame with columns:
\describe{
  \item{fighter_id}{ID of the fighter}
  \item{tier_id}{Numeric tier ID (1-4)}
  \item{achieved}{Logical, TRUE if achieved}
  \item{percentile}{Proportion of fighters who achieved this tier or higher}
  \item{achievement_tier}{"Bronze", "Silver", "Gold", or "Epic"}
  \item{achievement_name}{"On the Streak!"}
  \item{achievement_description}{Includes the actual number of victories in the streak}
  \item{achievement_icon}{e.g., "on_the_streak_bronze.png"}
}
}
\description{
Internal function to award the "On the Streak!" achievement based on the longest uninterrupted winning streak.

Tiers:
- Epic (tier_id=4): ≥50 victories
- Gold (tier_id=3): ≥20 victories
- Silver (tier_id=2): ≥10 victories
- Bronze (tier_id=1): ≥5 victories

The achievement description dynamically includes the fighter's actual maximum streak.
}
\keyword{internal}
