% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/datasets.R
\docType{data}
\name{hema_fights}
\alias{hema_fights}
\title{HEMA Fights Dataset}
\format{
A data frame with the following columns:
\describe{
  \item{match_id}{Integer. Unique identifier for each match.}
  \item{event_id}{Integer. Identifier for the event in which the match took place.}
  \item{event_name}{Character. Name of the event.}
  \item{event_date}{Date. Date of the event.}
  \item{fighter_1}{Character. Name of the first fighter.}
  \item{fighter_1_result}{Character. Result of the match for the first fighter (e.g., "Win", "Loss").}
  \item{fighter_2}{Character. Name of the second fighter.}
  \item{fighter_2_result}{Character. Result of the match for the second fighter.}
  \item{fighter_id}{Integer. Identifier for the primary fighter associated with the match record.}
  \item{opponent_id}{Integer. Identifier for the opponent fighter.}
  \item{stage}{Character. Stage of the tournament.}
  \item{tournament_category}{Character. Category of the tournament.}
  \item{tournament_id}{Integer. Identifier for the tournament.}
  \item{tournament_name}{Character. Name of the tournament.}
  \item{tournament_note}{Character. Additional notes regarding the tournament.}
  \item{tournament_weapon}{Character. Weapon category used in the tournament.}
  \item{elo_1}{Numeric. Elo rating of fighter 1.}
  \item{elo_2}{Numeric. Elo rating of fighter 2.}
  \item{update_1}{Numeric. Updated metric 1 (please adjust the description as needed).}
  \item{update_2}{Numeric. Updated metric 2 (please adjust the description as needed).}
  \item{win_chance_1}{Numeric. Win chance for fighter 1 (expressed as a proportion or percentage).}
  \item{win_chance_2}{Numeric. Win chance for fighter 2 (expressed as a proportion or percentage).}
}
}
\usage{
hema_fights
}
\description{
This dataset contains information about individual HEMA fights, including the fighters involved,
their performance metrics, and match outcomes.
}
\examples{
data(hema_fights)
head(hema_fights)
}
\keyword{datasets}
