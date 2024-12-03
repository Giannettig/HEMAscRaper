#' Download and Extract HEMA Fighters Data
#'
#' This function downloads data from the *HEMA Ratings* website and extracts detailed information
#' about historical European martial arts fighters, including their names, nationalities, and associated clubs.
#'
#' @return A tibble containing fighters' details with the following columns:
#' \describe{
#'   \item{\code{fighter_id}}{Unique identifier of the fighter. Integer.}
#'   \item{\code{fighter_name}}{Name of the fighter. Character.}
#'   \item{\code{fighter_nationality}}{Nationality of the fighter (country flag displayed). Character.}
#'   \item{\code{fighter_club_id}}{Unique identifier of the club, if available. Integer.}
#'   \item{\code{fighter_club_name}}{Name of the fighter's club, if available. Character.}
#'   \item{\code{fighter_url}}{URL of the fighter's profile on the HEMA Ratings website. Character.}
#' }
#'
#' @details
#' This function scrapes the HEMA Ratings website, specifically the fighters table, and extracts
#' relevant details for each fighter. It uses the `rvest` and `dplyr` packages to parse the HTML structure and
#' organize the extracted information into a tibble.
#'
#' @note
#' The function assumes the structure of the HEMA Ratings webpage remains consistent. If the webpage layout changes,
#' this function may break.
#'
#' @examples
#' \dontrun{
#' # Download HEMA fighters data and display the first few rows
#' fighters_data <- get_fighters()
#' head(fighters_data)
#' }
#'
#' @importFrom rvest read_html html_element html_elements html_attr html_text
#' @importFrom dplyr tibble select
#' @importFrom purrr map_chr
#' @importFrom stringr str_extract
#' @export
get_fighters<- function(){

  fighters_url="https://hemaratings.com/fighters/"

  # Read the HTML content from the URL
  web_page <- rvest::read_html(fighters_url)


  fighters<-web_page%>%
    rvest::html_element("table")%>%rvest::html_elements("tr")%>%.[2:length(.)]%>%
    {row <- .;
    dplyr::tibble(
          fighter_url=rvest::html_element(row,"a")%>%rvest::html_attr("href"),
          fighter_name=rvest::html_element(row,"a")%>%rvest::html_text(),
          fighter_id=stringr::str_extract(fighter_url, "\\d+")%>%as.numeric(),
          fighter_nationality=rvest::html_element(row,"i")%>%rvest::html_attr("title"),
          fighter_club_url=row%>%purrr::map_chr(~ rvest::html_elements(.x, "a")%>%{links<-.;ifelse(length(links)<2,NA,links[2]%>%rvest::html_attr("href"))}),
          fighter_club_id=stringr::str_extract(fighter_club_url, "\\d+")%>%as.numeric(),
          fighter_club_name=row%>%purrr::map_chr(~ rvest::html_elements(.x, "a")%>%{links<-.;ifelse(length(links)<2,NA,links[2]%>%rvest::html_text())})
          )%>%dplyr::select(fighter_id,fighter_name,fighter_nationality,fighter_club_id,fighter_club_name,fighter_url)
    }

    return(fighters)
}



#' Download and Extract HEMA Clubs Data
#'
#' This function downloads data from the *HEMA Ratings* website and extracts detailed information
#' about historical European martial arts clubs, including their locations, member counts, and parent organizations.
#'
#' @return A tibble containing clubs' details with the following columns:
#' \describe{
#'   \item{\code{club_id}}{Unique identifier of the club. Integer.}
#'   \item{\code{club_name}}{Name of the club. Character.}
#'   \item{\code{club_country}}{Country where the club is located. Character.}
#'   \item{\code{club_state}}{State or region of the club (if applicable). Character.}
#'   \item{\code{club_city}}{City where the club is based. Character.}
#'   \item{\code{club_members}}{Number of registered fighters associated with the club. Integer.}
#'   \item{\code{club_parent_id}}{Unique identifier of the parent club, if the club is a sub-organization. Integer.}
#'   \item{\code{club_url}}{URL of the club's profile on the HEMA Ratings website. Character.}
#' }
#'
#' @details
#' This function scrapes the HEMA Ratings clubs page and compiles a list of all clubs with relevant metadata.
#' It identifies parent-child club relationships and merges them with additional club details such as state,
#' city, and number of members.
#'
#' The function assumes the structure of the HEMA Ratings webpage remains consistent. If the webpage layout changes,
#' this function may need adjustments.
#'
#' @note
#' The function uses `rvest` for web scraping and `dplyr` for data manipulation. It handles potential parent-child relationships between clubs.
#'
#' @examples
#' \dontrun{
#' # Download HEMA clubs data and display the first few rows
#' clubs_data <- get_clubs()
#' head(clubs_data)
#' }
#'
#' @importFrom rvest read_html html_element html_elements html_attr html_text html_table
#' @importFrom dplyr tibble select left_join rename
#' @importFrom purrr map_dfr
#' @importFrom stringr str_extract str_replace_all
#' @export
get_clubs<- function(){

  clubs_url="https://hemaratings.com/clubs/"
  # Read the HTML content from the URL
  web_page <- rvest::read_html(clubs_url)

  clubs_meta<-web_page%>%
    rvest::html_element("table")%>%rvest::html_elements("tr")%>%.[2:length(.)]%>%
    {row <- .;
    dplyr::tibble(
      club_url=rvest::html_element(row,"a")%>%rvest::html_attr("href"),
      club_name=rvest::html_element(row,"a")%>%rvest::html_text()%>%stringr::str_squish(),
      club_id=stringr::str_extract(club_url, "\\d+")%>%as.numeric(),
      club_country=rvest::html_element(row,"i")%>%rvest::html_attr("title"),
     )
    }

  clubs_table<-web_page%>%
    rvest::html_element("table")%>%
    rvest::html_table()%>%dplyr::select(-Country)%>%
    dplyr::mutate(Name=Name%>%stringr::str_remove( "^\\s*-|-$")%>%stringr::str_squish())%>%
    dplyr::rename(
      club_name=Name,
      club_state=State,
      club_city=City,
      club_members=Fighters)%>%.[!duplicated(.$club_name),]




  #I deal here with sub_clubs and their parent organization
  children_url<-web_page%>%
    as.character()%>%
    str_replace_all(.," - <a",'<a class="children"')%>%
    rvest::read_html()%>%
    rvest::html_element("table")%>%
    rvest::html_elements(".children")%>%
    rvest::html_attr("href")%>%paste0("http://hemaratings.com",.)

  parent_clubs<-purrr::map_dfr(children_url,get_parent)


  clubs<-clubs_meta%>%
    dplyr::left_join(parent_clubs, by="club_id")%>%
    dplyr::left_join(clubs_table, by="club_name")%>%
    dplyr::select(club_id,club_name,club_country,club_state,club_city,club_members,club_parent_id,club_url)


  return(clubs)

}




#' Download and Extract HEMA Events Data
#'
#' This function downloads data from the *HEMA Ratings* website and extracts detailed information
#' about historical European martial arts events, including their dates, locations, and corresponding URLs.
#'
#' @return A tibble containing events' details with the following columns:
#' \describe{
#'   \item{\code{event_id}}{Unique identifier of the event. Integer.}
#'   \item{\code{event_brand}}{Name of the event brand. Character.}
#'   \item{\code{event_name}}{Name of the event. Character.}
#'   \item{\code{event_year}}{Year in which the event took place. Integer.}
#'   \item{\code{event_date}}{Date of the event in "YYYY-MM-DD" format. Date.}
#'   \item{\code{event_country}}{Country where the event was held. Character.}
#'   \item{\code{event_city}}{City where the event took place, if available. Character.}
#'   \item{\code{event_url}}{URL of the event's page on the HEMA Ratings website. Character.}
#' }
#'
#' @details
#' This function scrapes the HEMA Ratings events page and compiles a list of all HEMA events, including
#' the event name, date, country, and other relevant details. It uses the `rvest` and `dplyr` packages
#' to parse the HTML structure and organize the extracted information into a tibble.
#'
#' The function assumes the structure of the HEMA Ratings webpage remains consistent. If the webpage layout changes,
#' this function may need adjustments.
#'
#' @note
#' The function handles cases where city information might be missing and defaults to `NA` when a city is not listed.
#'
#' @examples
#' \dontrun{
#' # Download HEMA events data and display the first few rows
#' events_data <- get_events()
#' head(events_data)
#' }
#'
#' @importFrom rvest read_html html_elements html_text html_attr html_element
#' @importFrom dplyr tibble select
#' @importFrom purrr map map_chr
#' @importFrom stringr str_remove str_extract
#' @importFrom lubridate as_date
#' @export
get_events<-function(){

  events_url="https://hemaratings.com/events/"

  # Read the HTML content from the URL
  web_page <- rvest::read_html(events_url)


  events<-web_page%>%rvest::html_elements(".panel-group")%>%.[1:length(.)]%>%

    {

    row<-.;

    dplyr::tibble(
      event_name=row%>%rvest::html_elements(".event-title")%>%rvest::html_text()%>%str_remove(".*-\\s*"),
      event_brand=event_name%>%str_remove("\\s*\\d{4}$"),
      event_day=row%>%rvest::html_elements(".panel-body")%>%purrr::map(~ rvest::html_elements(.x, "dd")[1])%>% purrr::map_chr(function(x) rvest::html_text(x)),
      event_year=row%>%rvest::html_attr("id")%>%stringr::str_extract( "\\d+")%>%as.numeric(),
      event_date=paste(event_year,event_day," ")%>%as.Date(format = "%Y %B %d"),
      event_country=row%>%rvest::html_elements(".panel-body")%>%purrr::map(~ rvest::html_elements(.x, "dd")[2])%>% purrr::map_chr(function(x) rvest::html_text(x)),
      event_city=row%>%rvest::html_elements(".panel-body")%>%purrr::map(~ rvest::html_elements(.x, "dd")%>%{links<-.;ifelse(length(links)<3,NA,links[3]%>%rvest::html_text(.))})%>%as.character(),
      event_url=row%>%rvest::html_elements(".panel-body")%>%rvest::html_element("a")%>%rvest::html_attr("href"),
      event_id=stringr::str_extract(event_url, "\\d+")%>%as.numeric()
    )%>%dplyr::select(event_id,event_brand,event_year,event_name,event_date,event_country,event_city,event_url)

    }

  return(events)

}



#' Download and Extract HEMA Tournament Data
#'
#' This function downloads data from a specified HEMA tournament page on the *HEMA Ratings* website and extracts detailed fight statistics,
#' including tournament names, disciplines, fighters, and fight results.
#'
#' @param tournament_url A character string specifying the URL of the tournament page on the HEMA Ratings website.
#' This URL should link directly to a specific tournament event.
#'
#' @return A tibble containing detailed tournament data with the following columns:
#' \describe{
#'   \item{\code{event_id}}{Unique identifier of the event. Integer.}
#'   \item{\code{event_name}}{Name of the event. Character.}
#'   \item{\code{tournament_name}}{Name of the specific tournament or discipline within the event. Character.}
#'   \item{\code{event_category}}{Category of the tournament (e.g., Mixed, Men's, Women's, Underrepresented Genders). Character.}
#'   \item{\code{event_note}}{Additional notes about the tournament (e.g., special rules or conditions), if available. Character.}
#'   \item{\code{event_weapon}}{Weapon used in the tournament (e.g., Longsword, Rapier). Character.}
#'   \item{\code{fighter_id}}{Unique identifier of the first fighter in a match. Integer.}
#'   \item{\code{opponent_id}}{Unique identifier of the opponent fighter in a match. Integer.}
#'   \item{\code{fighter_score}}{Score of the first fighter. Character.}
#'   \item{\code{opponent_score}}{Score of the opponent fighter. Character.}
#' }
#'
#' @details
#' This function scrapes the specified tournament page and extracts match-level information for all the fights listed.
#' It organizes the data into a tidy format, suitable for further analysis or visualization. If the page layout changes, adjustments to the scraping logic may be necessary.
#'
#' The function also attempts to handle anonymized fighters by assigning them a placeholder ID.
#'
#' @note
#' This function relies on the `rvest` and `dplyr` packages for web scraping and data manipulation.
#'
#' @examples
#' \dontrun{
#' # Download HEMA tournament data for a specific event and display the first few rows
#' tournament_data <- get_tournament("https://hemaratings.com/tournament/details/1234/")
#' head(tournament_data)
#' }
#'
#' @importFrom httr RETRY
#' @importFrom rvest read_html html_elements html_text html_attr html_element html_table
#' @importFrom dplyr tibble bind_cols bind_rows
#' @importFrom purrr map2
#' @importFrom stringr str_detect str_extract str_remove str_replace_all str_squish
#' @importFrom snakecase to_snake_case
#' @export
get_tournament<-function(tournament_url){

  #we will the individual events link to access fight statistics
  web_page <- httr::RETRY('GET',tournament_url)%>%rvest::read_html()

  result<-web_page%>%rvest::html_elements(".panel-default")%>%rvest::html_elements(".event-title")%>%rvest::html_text()

  result_index<-result%>%stringr::str_detect("fights")%>%which()

  tournaments<-result[result_index]%>%{

    dplyr::tibble(
      tournament_name=stringr::str_extract(., ".*(?= - )"),
      discipline=dplyr::if_else(stringr::str_detect(.,"\\("), stringr::str_extract(., ".*(?= \\()"),stringr::str_extract(., ".*(?= - )")) ,
      fights=stringr::str_extract(., "(?<= - ).*")%>%stringr::str_remove("fights")%>%as.numeric()
    )

  }


  tournament_tables<-
    web_page%>%
    as.character()%>%
    stringr::str_replace_all(.,"Anonymous fighter",'<a href="/fighters/details/0000/">Anonymous fighter</a>')%>%
    rvest::read_html()%>%
    rvest::html_elements(".panel-default")%>%
    rvest::html_element("table")%>%.[result_index]%>%purrr::map2(result[result_index], function(x,y)
    {
      row<-x

      dplyr::tibble(
        event_id=stringr::str_extract(tournament_url, "\\d+")%>%as.numeric(),
        event_name=web_page%>%rvest::html_element("h2")%>%rvest::html_text(),
        tournament_name=y,
        tournament_category=y%>%stringr::str_extract("Mixed|Men's|Women's|Underrepresented Genders"),
        tournament_note=y%>%stringr::str_extract("\\(.*\\)")%>%stringr::str_remove_all("[\\(\\)]"),
        tournament_weapon=y%>%stringr::str_remove_all("Mixed|Men's|Women's|Underrepresented Genders|\\(.*\\)|-\\s*\\d+\\s*fight[s]?")%>%stringr::str_squish(),
        fighter_id=row %>%rvest::html_elements("a")%>%{links<-.; links[seq(1, length(links), 2)] }%>%rvest::html_attr("href")%>%stringr::str_extract( "\\d+")%>%as.numeric(),
        opponent_id=row %>%rvest::html_elements("a")%>%{links<-.; links[seq(2, length(links), 2)] }%>%rvest::html_attr("href")%>%stringr::str_extract( "\\d+")%>%as.numeric()
      )%>%dplyr::bind_cols(row%>%rvest::html_table())%>%mutate(Stage=as.character(Stage))

    })%>%dplyr::bind_rows()

  names(tournament_tables)<-snakecase::to_snake_case(names(tournament_tables))

  return(tournament_tables)
}



#' Download and Extract HEMA Fights Data by Year
#'
#' This function downloads data from the *HEMA Ratings* website and extracts detailed fight statistics
#' for events that occurred within a specified year or set of years. If no year is specified, the function
#' gathers fight data for all available events.
#'
#' @param year An optional vector of years (as integers) to filter the events. If `NULL`, data for all available years is fetched. Defaults to `NULL`.
#'
#' @return A tibble containing detailed fight statistics for the specified events with columns that may include:
#' \describe{
#'   \item{\code{event_id}}{Unique identifier of the event. Integer.}
#'   \item{\code{event_name}}{Name of the event. Character.}
#'   \item{\code{tournament_name}}{Name of the specific tournament or discipline within the event. Character.}
#'   \item{\code{event_category}}{Category of the tournament (e.g., Mixed, Men's, Women's, Underrepresented Genders). Character.}
#'   \item{\code{event_weapon}}{Weapon used in the tournament (e.g., Longsword, Rapier). Character.}
#'   \item{\code{fighter_id}}{Unique identifier of the first fighter in a match. Integer.}
#'   \item{\code{opponent_id}}{Unique identifier of the opponent fighter in a match. Integer.}
#'   \item{\code{fighter_score}}{Score of the first fighter. Integer.}
#'   \item{\code{opponent_score}}{Score of the opponent fighter. Integer.}
#' }
#'
#' @details
#' This function scrapes the HEMA Ratings events page and compiles a list of all fights for the specified years.
#' If no `year` is provided, it extracts fight data for all events available on the website. For each event, it calls
#' the `get_tournament()` function to gather detailed fight-level information.
#'
#'
#' @note
#' The function uses `rvest` for HTML parsing and `purrr` for handling multiple event pages. It performs basic error handling
#' to check if events matching the specified years are available. Depending on the number of events and the size of the tournament data,
#' the function may take some time to run.
#'
#' @examples
#' \dontrun{
#' # Download all HEMA fight data for the year 2022
#' fights_2022 <- get_fights(2022)
#'
#' # Download HEMA fight data for multiple years (e.g., 2019 and 2020)
#' fights_2019_2020 <- get_fights(c(2019, 2020))
#'
#' # Download HEMA fight data for all available years
#' all_fights <- get_fights()
#' }
#'
#' @importFrom rvest read_html html_elements html_attr html_element
#' @importFrom purrr map_chr map_dfr is_empty
#' @importFrom stringr str_detect str_remove str_replace_all str_extract
#' @importFrom dplyr tibble
#' @export

get_fights<-function(year=NULL){

  events_url="https://hemaratings.com/events/"

  #we will the individual events link to access fight statistics
  web_page <- rvest::read_html(events_url)

  # Check if `year` is NULL, a single number, or a vector
  if (!is.null(year)) {


  # Ensure `year` is treated as a vector, even if it's a single number
    years_to_filter <- as.vector(year)

  # Create an XPath condition for multiple years
  year_conditions <- paste0("contains(@id, '", years_to_filter, "')", collapse = " or ")

  # Build the complete XPath expression
  xpath_expr <- paste0("//*[contains(@class, 'panel-group') and (", year_conditions, ")]")
  }else {xpath_expr<-"//*[contains(@class, 'panel-group')]"; years_to_filter="all time"}

  events_url<-web_page%>%
    rvest::html_elements(xpath = xpath_expr)%>%
    rvest::html_elements(".panel-body")%>%
    rvest::html_element("a")%>%rvest::html_attr("href")%>%
    purrr::map_chr(., function(x) paste0("https://hemaratings.com",x))

    # Testing that the events vector is not empty
    if(purrr::is_empty(events_url)){
      message("The events_url vector is empty. There are no events matching the selected timerange.")

    }else{

  message(paste("Searching Hema Ratings for fights in:", paste(years_to_filter, collapse = ",")))
  fights<-events_url%>%purrr::map_dfr(get_tournament,.progress = TRUE)
  
  return(fights)
    }
}

