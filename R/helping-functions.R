
#These are functions that are not meant to be used outside a bigger functions


get_parent<-function(children_url){

  web_page <-  httr::RETRY('GET',children_url)%>%rvest::read_html()

  parent_url=web_page%>%rvest::html_element(".table-striped")%>%rvest::html_element("a")%>%rvest::html_attr("href")

  parent<-dplyr::tibble(
    club_id=stringr::str_extract(children_url, "\\d+")%>%as.numeric(),
    club_parent_id=stringr::str_extract(parent_url, "\\d+")%>%as.numeric(),

  )

  return(parent)

}
