library(httr2)
library(magrittr) 
library(lubridate)
library(rlang)
library(data.table)
library(arrow)

make_request <- function(url,
                         ...,
                         proxy = TRUE,
                         verbosity = NULL,
                         wrapper = data.table::as.data.table) {
  req <- request(base_url = url)
  req <- req_user_agent(
    req = req,
    string = "Request made by R package rods (https://github.com/dreamRs/rods)"
  )
  if (isTRUE(proxy) && !identical(Sys.getenv("PROXY_URL"), "")) {
    req <- req_proxy(
      req = req,
      url = Sys.getenv("PROXY_URL"),
      port = as.integer(Sys.getenv("PROXY_PORT")),
      username = Sys.getenv("PROXY_ID"),
      password = Sys.getenv("PROXY_PWD"),
      auth = Sys.getenv("PROXY_AUTH", unset = "basic")
    )
  } else if (is_list(proxy)) {
    req <- req_proxy(
      req = req,
      url = proxy$url,
      port = proxy$port,
      username = proxy$username,
      password = proxy$password,
      auth = proxy$auth %||% "basic"
    )
  }
  req <- req_url_query(.req = req, ...)
  res <- req_perform(req, verbosity = verbosity)
  data <- fread(text = resp_body_string(res))
  wrapper(data)
}


download_dataset <- function(server,
                             dataset,
                             ...,
                             proxy = TRUE,
                             verbosity = NULL,
                             wrapper = data.table::as.data.table) {
  make_request(
    url = paste0(server, sprintf("/api/explore/v2.1/catalog/datasets/%s/exports/csv", dataset)),
    ...,
    proxy = proxy,
    verbosity = verbosity,
    wrapper = wrapper
  )
}

charger_donnees <- function(proxy = FALSE){
  if (file.exists("inputs/donnees.parquet")) {
    donnees_cache <- read_parquet("inputs/donnees.parquet")%>%
      mutate(date_cet = as.Date(date_cet)) %>%
      filter(date_heure_utc <= now(tzone = "UTC"), !is.na(intensite_emissions_conso)) %>%
      distinct(date_heure_utc, .keep_all = TRUE) %>%
      arrange(date_heure_utc)
  
    date_max <- max(donnees_cache$date_heure_utc)
    diff <- difftime(now(tzone = "UTC"), date_max,units = "hours") 
    if (diff >0) {
      nouvelle_donnees <- download_dataset(
        proxy = proxy,
        server = "https://odre.opendatasoft.com",
        dataset = "part-enr-intensite-ges-conso-tr",
        where = paste0("date_heure_utc >= '", format(date_max, "%Y-%m-%dT%H:%M:%S"), "'")
      )%>%mutate(date_cet = as.Date(date_cet),
                 heure_cet = as.character(heure_cet))
      
      donnees <- bind_rows(
        donnees_cache,
        nouvelle_donnees) %>% arrange(date_heure_utc)%>%
        filter(!is.na(intensite_emissions_conso)) %>%
        distinct(date_heure_utc, .keep_all = TRUE)
      
      write_parquet(donnees, "inputs/donnees.parquet")
      return(donnees)
        }
    else {
      return(donnees_cache)
    }
  
  }
  else {
    donnees <- download_dataset(
    proxy = proxy,
    server = "https://odre.opendatasoft.com",
    dataset = "part-enr-intensite-ges-conso-tr")%>%
      mutate(date_cet = as.Date(date_cet),
             heure_cet = as.character(heure_cet))
    
    write_parquet(donnees, "inputs/donnees.parquet")
    return(donnees) 
    }
}

