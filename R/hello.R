
library("tidyverse")
library("rvest")
library("httr")


#' get_fighters
#'
#' @param fighters_url
#' @param path
#'
#' @return
#' @export
#'
#' @examples
get_fighters<- function(fighters_url="https://hemaratings.com/fighters/", path="./out_fighters_csv"){

  # Read the HTML content from the URL
  web_page <- read_html(fighters_url)


  fighters<-web_page%>%
    html_element("table")%>%html_elements("tr")%>%.[2:length(.)]%>%
    {row <- .;
    tibble(
          fighter_url=html_element(row,"a")%>%html_attr("href"),
          fighter_name=html_element(row,"a")%>%html_text,
          fighter_id=str_extract(fighter_url, "\\d+")%>%as.numeric(),
          fighter_country=html_element(row,"i")%>%html_attr("title"),
          fighter_club_url=row%>%map_chr(~ html_elements(.x, "a")%>%{links<-.;ifelse(length(links)<2,NA,links[2]%>%html_attr("href"))}),
          fighter_club_id=str_extract(fighter_club_url, "\\d+")%>%as.numeric(),
          fighter_club_name=row%>%map_chr(~ html_elements(.x, "a")%>%{links<-.;ifelse(length(links)<2,NA,links[2]%>%html_text)})
          )
    }

    write_csv(fighters,path)

}



get_clubs<- function(clubs_url="https://hemaratings.com/clubs/", path="./out_clubs_csv"){

  # Read the HTML content from the URL
  web_page <- read_html(clubs_url)

  clubs_meta<-web_page%>%
    html_element("table")%>%html_elements("tr")%>%.[2:length(.)]%>%
    {row <- .;
    tibble(
      club_url=html_element(row,"a")%>%html_attr("href"),
      club_name=html_element(row,"a")%>%html_text,
      club_id=str_extract(club_url, "\\d+")%>%as.numeric(),
      club_country=html_element(row,"i")%>%html_attr("title"),
      #fighter_club_url=row%>%map_chr(~ html_elements(.x, "a")%>%{links<-.;ifelse(length(links)<2,NA,links[2]%>%html_attr("href"))}),
      #fighter_club_id=str_extract(fighter_club_url, "\\d+")%>%as.numeric(),
      #fighter_club_name=row%>%map_chr(~ html_elements(.x, "a")%>%{links<-.;ifelse(length(links)<2,NA,links[2]%>%html_text)})
    )
    }

  clubs_table<-web_page%>%
    html_element("table")%>%html_table()%>%select(-Country)%>%rename(club_name=Name, club_state=State,club_city=City,club_members=Fighters)

  clubs<-clubs_meta%>%left_join(clubs_table)

  write_csv(clubs,path)

}



get_events<-function(events_url="https://hemaratings.com/events/", path="./out_events_csv"){

  # Read the HTML content from the URL
  web_page <- read_html(events_url)


  events<-web_page%>%html_elements(".panel-group")%>%.[1:length(.)]%>%

    {

    row<-.;

    tibble(
      event_year=row%>%html_attr("id")%>%str_extract( "\\d+")%>%as.numeric(),
      event_name=row%>%html_elements(".event-title")%>%html_text,
      event_date=row%>%html_elements(".panel-body")%>%map(~ html_elements(.x, "dd")[1])%>% map_chr(html_text),
      event_country=row%>%html_elements(".panel-body")%>%map(~ html_elements(.x, "dd")[2])%>% map_chr(html_text),
      event_city=row%>%html_elements(".panel-body")%>%map(~ html_elements(.x, "dd")%>%{links<-.;ifelse(length(links)<3,NA,links[3]%>%html_text)}),
      event_url=row%>%html_elements(".panel-body")%>%html_element("a")%>%html_attr("href"),
      event_id=str_extract(event_url, "\\d+")%>%as.numeric()
    )

    }

  write_csv(events,path)

}

