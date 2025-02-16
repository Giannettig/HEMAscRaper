% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/normalize_events.R
\name{normalize_events}
\alias{normalize_events}
\title{Normalize Event Names}
\usage{
normalize_events(name)
}
\arguments{
\item{name}{A character vector of event names to be cleaned and normalized.}
}
\value{
A character vector of cleaned and normalized event names.
}
\description{
This function normalizes event names by:
- Removing non-ASCII characters.
- Converting text to lowercase.
- Removing unwanted patterns such as ordinal numbers, Roman numerals, and event-specific identifiers.
- Converting the text to title case.
- Trimming and squishing extra spaces.
}
\examples{
 \dontrun{
# Example usage:
event_names <- c(
  "1st Capitol Clash '22",
  "V. Annual Clash #123",
  "Capitol Clash Event 2023"
)
normalize_events(event_names)
# [1] "Capitol Clash" "Annual Clash" "Capitol Clash"
}

}
\keyword{internal}
