% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ach_pig_slayer.R
\name{ach_pig_slayer}
\alias{ach_pig_slayer}
\title{Pig Slayer Achievement}
\usage{
ach_pig_slayer(data)
}
\arguments{
\item{data}{A data frame containing HEMA tournament match data}
}
\value{
A data frame of achievements with columns:
\itemize{
  \item fighter_id: Unique fighter identifier
  \item tier_id: Achievement tier level (1-4)
  \item achieved: Logical indicating if achievement earned
  \item percentile: Fighter's percentile for this achievement
  \item achievement_tier: Text description of tier
  \item achievement_name: Name of the achievement
  \item achievement_description: Description of what was achieved
  \item achievement_icon: Icon file name for the achievement
}
}
\description{
Awards achievements to fighters based on their performance against Alexander
Stankievich.
}
\details{
Tiers are awarded based on victories against Alexander Stankievich:
- Epic (4): 5+ wins with no losses
- Gold (3): 5+ wins
- Silver (2): 3+ wins
- Bronze (1): 1+ win
}
\examples{
\dontrun{
achievements <- ach_pig_slayer(tournament_data)
}

}
\keyword{internal}
