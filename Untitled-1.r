base_url <- 'https://www.quakercloud.org/cloud/gainesville-quakers'
number_version <- 'https://www.quakercloud.org/cloud/8862/'

base_page <- rvest::read_html(base_url)
hrefs <- rvest::html_elements(base_page, css = "a")
hrefs |> rvest::html_attrs() -> temp

get_hrefs <- function(html) {
  html |> 
    rvest::html_elements(css = "a") |> 
    rvest::html_attrs() |> 
    purrr::flatten_chr() -> temp 
  temp[ names(temp) == 'href' ]
}
  
#some api stuff
filter_hrefs <- function(hrefs){
  purrr::discard(hrefs, \(x){ grepl("http", x)}) |> 
    purrr::discard( \(x){ grepl("login", x)}) |>
    purrr::discard( \(x){ grepl("mailto", x)}) |>
#   purrr::discard( \(x){ grepl("node", x)}) |>
#   purrr::discard( \(x){ grepl("discover", x)}) |>
#   purrr::discard( \(x){ grepl("video", x)}) |>
#   purrr::discard( \(x){ grepl("deepen", x)}) |>
#   purrr::discard( \(x){ grepl("grow", x)}) |>   
    stringr::str_remove("/cloud/gainesville-quakers")
}

link_to_name <- function(link){
  stringr::str_sub(
    stringr::str_replace_all(link, "\\/", "-")
    , start = 2
  )
}

href_q <- function(str_vec){
  stopifnot(typeof(str_vec) == "character")
  vec <- unique(str_vec)
  class(vec) <- "href_q"
  vec
}

add_q <- function(hrefs, href_q){
  href_q(c(hrefs, unclass(href_q)))
  }

tl <- purrr::partial(head, n = -1)
car <- purrr::partial(head, n = 1)
cdr <- purrr::partial(tail, n = -1)

recover_html <- purrr::safely(xml2::read_html, otherwise = "")

main <- function(starter_page, path =  here::here(), base_url){
  
  # takes queue and produces a new queue 
  advance_page <- function(href_q, dead_q = "", base_url){
    
    print(paste0(length(href_q), "-", length(dead_q)))
    
    href_q <- href_q[! href_q %in% dead_q]
    
    if(length(href_q) <= 1) {
      
      page <- recover_html(paste0(base_url, href_q))$result
      
      xml2::write_html(
        x = page
        , file = paste0(path, "/", link_to_name(href_q), ".html")
        )
      
      return()
      
    } else {
      
      url <- paste0(base_url, car(href_q))
      print(url)
      res <- recover_html(url)
      
      if(is.character(res$result)){ 
        print(paste("current page ", url, " is not working: ", res$error ))
        
        dead_q <- add_q(hrefs = car(href_q), dead_q)
        href_q <- cdr(href_q)
        
        advance_page( href_q = href_q, dead_q = dead_q, base_url = base_url )
        
        } else {
      
      xml2::write_html(
        x = res$result
        , file = paste0(path, "/", link_to_name(car(href_q)), ".html")
        )
        }
          
      dead_q <- add_q(hrefs = car(href_q), dead_q)
      href_q <- add_q(
        href_q = cdr(href_q) 
        , get_hrefs(
          recover_html( paste0(base_url, car(href_q)))$result ) |> 
          filter_hrefs()
      )
    
      advance_page( href_q = href_q, dead_q = dead_q, base_url = base_url )
    }
  }
  
  start_href <- get_hrefs(starter_page) |> unique()
  
  advance_page(start_href, base_url = base_url )
}