library(readr)
library(httr2)
library(stringr)

get_aoc <- function(year_=NULL, day_=NULL){
  if (is.null(year_) || is.null(day_)){
    date_ <- strsplit(format(Sys.Date(), "%Y,%d"), ",")
    year_ <- date_[[1]][1]    
    day_  <- date_[[1]][2]    
  }

  file_in <- file.path("in", str_glue("{day_}"))
  url_day <- str_glue("https://adventofcode.com/{year_}/day/{as.numeric(day_)}")
  url_input <- str_glue("{url_day}/input")
  
  message(cli::style_hyperlink(url_day, url_day), " input >> ", file_in)
  if (!file.exists(file_in)){
    request(url_input) |> 
      req_headers(Cookie = str_glue("session={read_file('.aoc_session')}")) |>
      req_perform(path = file_in)
  }
  content_view <- read_lines(file_in) |> str_view(use_escapes = TRUE)

  message("# head / tail ------------------------------------------------------------")
  head(content_view) |> print()
  tail(content_view) |> print()
  message("# ------------------------------------------------------------------------")
  
  return(file_in)
}
