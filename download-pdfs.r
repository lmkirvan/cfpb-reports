# more recursion, more! bwahahaha.
options(expressions = 100000) 
base_url <- "https://www.consumerfinance.gov"
starter_page <- 'https://www.consumerfinance.gov/about-us/blog/'
file_url <- "https://files.consumerfinance.gov/"

## make this part follow the relative links too (i.e. those that start with ?)
## if they start with a query then you take the you paste the querty at the end of the 
## page you are on somehow. 

## i think that we can first sort the hrefs into absolute and relative 
## clean the url that produced the html so that it doesn't have queries in the last position
## and then paste the relative hrefs to the originating url
get_hrefs <- function(href) {
  html <- recover_html(href)$result

  if(is.character(html)){
    return("")
  }

  html |> 
    rvest::html_elements(css = "a") |>
    rvest::html_attrs() |> 
    purrr::flatten_chr() -> temp

  hrefs <- temp[names(temp) == 'href'] |>
    purrr::discard(\(x) {stringr::str_starts(x, "tel|https://www|https://x|https://oig|https://student")}) |>
    purrr::discard(\(x) {stringr::str_detect(x, "/consumer-complaints/search/")})

  queries <- hrefs[stringr::str_starts(hrefs, "\\?")] 
  
  clean_href <- stringr::str_split(href, "/")[[1]] |> 
    head(-1) |> 
    stringr::str_c(collapse = "/")
    
    files <- hrefs[grepl( "\\.pdf|\\.txt", hrefs)]
  
  remainder <- hrefs[! hrefs %in% c(queries, files)]
  queries <- paste0(clean_href, "/", queries)
  remainder <- paste0("https://www.consumerfinance.gov", remainder)
  
  c(queries, files, remainder)
}

safely_get_hrefs <- purrr::safely(get_hrefs, otherwise = "")

#some api stuff
filter_hrefs <- function(hrefs){
  purrr::discard(hrefs, \(x){ grepl("login", x)}) |>
    purrr::discard(\(x){ grepl("mailto", x)}) |>
    purrr::discard(\(x){grepl("/regulations/.+", x)}) |>
    purrr::discard(\(x){grepl("#[.]*", x)}) |> 
    purrr::discard(\(x){grepl(".zip", x)}) 
}

link_to_name <- function(url){
  stringr::str_split(url, "/")[[1]] |>
  tail(n = 1)
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

fix_long_name <- function(filename){
    
    if(nchar(filename) > 249){
        str_length <- nchar(filename)
        end_position =  249 - str_length
         stringr::str_sub(filename, end = end_position )
    } else {
        filename
    }
}

download_files <- function(url, name){
  
  if(!stringr::str_starts(url, "https://files")){
    return()
  }
  ext = tools::file_ext(url)
  
  switch(
    ext,
     "pdf" = download.file(url, destfile = name),
      "txt" = download.file(url, destfile = name),
      ""
  )
}  

main <- function(starter_page,  path =  here::here(),  restart = ''){
  
  if(logr::log_status() == "closed"){
    logr::log_open()
  }
  
  # takes queue and produces a new queue 
  advance_page <- function(href_q, dead_q = ""){
    
    print(paste0(length(href_q), "-", length(dead_q)))
    href_q <- href_q[! href_q %in% dead_q]
    
    if(length(href_q) <= 1 & length(dead_q) > 1) {
      return()      
    } else {
      
        url <-  car(href_q)
        print(url)
        res <- recover_html(url)
      
      if(is.character(res$result)){ 
        
        print(paste("current page ", url, " is not working: ", res$error))
        dead_q <- add_q(hrefs = car(href_q), dead_q)
        logr::log_print(paste0("fail: ", car(href_q)))
        href_q <- cdr(href_q)
        advance_page( href_q = href_q, dead_q = dead_q)
        } else {
            file_name = paste0(path, "/", link_to_name(car(href_q)))
            file_name = fix_long_name(file_name)
            download_files(url, file_name)
        }     
      dead_q <- add_q(hrefs = car(href_q), dead_q)
      logr::log_print(paste0("success: ", car(href_q)))
      href_q <- add_q(
        href_q = cdr(href_q)
        , safely_get_hrefs(car(href_q))$result |> filter_hrefs()
      )
      advance_page(href_q = href_q, dead_q = dead_q)
    }
  }

  start_href <- get_hrefs(starter_page) |> unique()
  advance_page(start_href, dead_q = href_q(restart))
}

restart <- function(starter_page, old_log, new_log){
  logr::log_open(new_log)
  main(starter_page, restart = old_log)
}

collect_log <- function(log){
  readr::read_lines(log) |>
  purrr::keep(\(x){stringr::str_starts(x, "success: |fail: ")}) |>
  purrr::map_chr(\(x){stringr::str_remove(x, "success: |fail: ")}) |>
  stringr::str_trim()
}

get_logs <- function(logs){
  purrr::map(logs, collect_log) |>
    purrr::reduce(c) |>
    unique()
} 

#main(starter_page = starter_page)

logs <- get_logs(c(
  "./log/dead_q.txt.log"
  , "./log/restart.txt.log"
  , "./log/restart2.txt.log"
  , "./log/restart3.txt.log"
  , "./log/restart4.txt.log"
  , "./log/restart5.txt.log"
  , "./log/restart6.txt.log"
  , "./log/restart7.txt.log"
  , "./log/restart8.txt.log"
  , "./log/restart9.txt.log"
  , "./log/restart10.txt.log"
  , "./log/restart11.txt.log"
  , "./log/restart12.txt.log"))

restart(
  starter_page = "https://www.consumerfinance.gov/compliance/supervision-examinations/"
   , new_log = "restart13.txt"
   , old_log = logs
   )

