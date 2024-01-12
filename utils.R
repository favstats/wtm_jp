calc_targeting <- function(only_tags, exclude = NULL) {
  
  if(sets$cntry=="TW"){
    age_limit <- 46
  } else {
    age_limit <- 48
  }
  
  # only_tags <- election_dat30 %>%
  # mutate(total_spend = total_spend_formatted) %>%
  #   filter(main_currency == the_currency)
  # only_tags <- election_dat30  %>%
  #   # left_join(all_dat) %>%
  #   # rename(internal_id = page_id) %>%
  #   filter(party != "And")  %>%
  #   filter(is.na(no_data)) %>%
  #   mutate(party = ifelse(party %in% c("GroenLinks", "PvdA"), "GroenLinks-PvdA", party)) %>%
  #   mutate(total_spend = total_spend_formatted) %>%
  #   filter(is.na(no_data)) %>%
  #   filter(page_name == "Partij voor de Dieren Gemeente Groningen")
  # filter(party == "Volt Nederland")
  
  total_sppppeen <- only_tags %>%
    distinct(internal_id, .keep_all = T)  %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    select(internal_id, total_spend, total_num_ads) %>%
    arrange(desc(total_spend)) %>%
    summarize(total_spend = sum(total_spend),
              num_ads = sum(total_num_ads))
  
  if(!is.null(exclude)){
    if(exclude){
      only_tags <- only_tags %>% filter(is_exclusion)
    } else if(!exclude){
      only_tags <- only_tags %>% filter(!is_exclusion)      
    }
  }
  
  
  howmuchisinterest <- only_tags %>%
    filter(type == "detailed") %>%
    group_by(internal_id) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per, num_ads) %>%
    arrange(desc(spend_per)) %>%
    summarize(spend_per = sum(spend_per),
              ads_per = sum(num_ads)) %>%
    mutate(target = "interest")
  
  howmuchislocation <- only_tags %>%
    filter(type == "location") %>%
    group_by(internal_id, location_type) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per, location_type, num_ads) %>%
    arrange(desc(spend_per)) %>%
    group_by(location_type) %>%
    summarize(spend_per = sum(spend_per),
              ads_per = sum(num_ads)) %>%
    rename(target = location_type)
  
  # only_tags <- only_tags %>% filter(party == "Volt Nederland")
  howmuchisage <- only_tags %>%
    filter(type == "age") %>%
    filter(total_spend_pct != 0) %>%
    group_by(internal_id) %>%
    mutate(n_ages = n()) %>% #count(n_ages, sort = T)
    ungroup() %>%
    mutate(spending_age = sum(total_spend_pct)) 
  
  if(nrow(howmuchisage)==0){
    howmuchisage <- tibble(spend_per = 0, ads_per = 0,  target = "age")
  } else if(howmuchisage %>% slice(1) %>% pull(spending_age) >= age_limit){
    howmuchisage <- tibble(spend_per = 0, ads_per = 0,  target = "age")
  } else if (nrow(howmuchisage)<age_limit) {
    
    howmuchisage <- howmuchisage %>% mutate(spend_per = total_spend, ads_per = total_num_ads, target = "age") %>% select(spend_per, target, ads_per) %>% slice(1)
    
    
    
    ## TODO: BUT WHYYYYY?
  } else if (!all(howmuchisage$total_spend_pct==1)){
    howmuchisage <- howmuchisage %>% 
      # filter(n_ages <= 47) %>%
      group_by(internal_id) %>%
      filter(total_spend_pct == min(total_spend_pct)) %>%
      slice(1) %>%
      ungroup() %>%
      # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
      mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
      mutate(spend_per = total_spend * (1-total_spend_pct)) %>%
      select(internal_id, spend_per, num_ads) %>%
      summarize(spend_per = sum(spend_per),
                ads_per = sum(num_ads)) %>%
      mutate(ads_per = total_sppppeen$num_ads-ads_per) %>% 
      mutate(target = "age")
  } else if (all(howmuchisage$total_spend_pct==1)){
    
    howmuchisage <- howmuchisage %>% mutate(spend_per = total_spend, ads_per = total_num_ads, target = "age") %>% select(spend_per, target, ads_per) %>% slice(1)
    
  } else {
    
    howmuchisage <- tibble(spend_per = 0, ads_per = 0, target = "age")
    
  }
  
  
  
  
  
  # howmuchisgender <- only_tags %>%
  #     filter(type == "gender") %>%
  #     filter(total_spend_pct != 0) %>%
  #     filter(value != "All") %>%
  #     # group_by(internal_id) %>%
  #     # summarize()
  #     # # filter(total_spend_pct == max(total_spend_pct)) %>%
  #     # slice(1) %>%
  #     # ungroup() %>%
  #     # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
  #     mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
  #     mutate(spend_per = total_spend * total_spend_pct) %>%
  #     select(internal_id, spend_per) %>%
  #     summarize(spend_per = sum(spend_per))  %>%
  #     mutate(target = "gender")
  
  howmuchisgender <- only_tags %>%
    filter(type == "gender") %>%
    filter(value != "All") %>%
    group_by(internal_id, value) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per, value, num_ads) %>%
    arrange(desc(spend_per)) %>%
    group_by(value) %>%
    summarize(spend_per = sum(spend_per),
              ads_per = sum(num_ads)) %>%
    ungroup() %>% 
    mutate(target = paste0("Gender: ", value)) %>% 
    select(-value)
  
  howmuchcustom <- only_tags %>%
    filter(type == "custom_audience") %>%
    filter(total_spend_pct != 0) %>%
    # filter(value != "All") %>%
    group_by(internal_id) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per, num_ads) %>%
    summarize(spend_per = sum(spend_per),
              ads_per = sum(num_ads)) %>%
    mutate(target = "custom_audience")
  
  
  howmuchlookalike <- only_tags %>%
    filter(type == "lookalike_audience") %>%
    filter(total_spend_pct != 0) %>%
    # filter(value != "All") %>%
    group_by(internal_id) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per, num_ads) %>%
    summarize(spend_per = sum(spend_per),
              ads_per = sum(num_ads)) %>%    
    mutate(target = "lookalike_audience")
  
  howmuchlanguage <- only_tags %>%
    filter(type == "language") %>%
    filter(total_spend_pct != 0) %>%
    drop_na(value) %>%
    # filter(value != "All") %>%
    group_by(internal_id) %>%
    filter(total_spend_pct == max(total_spend_pct)) %>%
    slice(1) %>%
    ungroup() %>%
    # mutate(total_spend = readr::parse_number(total_spend_formatted)) %>%
    mutate(total_spend = ifelse(total_spend == 100, 50, total_spend)) %>%
    mutate(spend_per = total_spend * total_spend_pct) %>%
    select(internal_id, spend_per, num_ads) %>%
    summarize(spend_per = sum(spend_per),
              ads_per = sum(num_ads)) %>%
    mutate(target = "language")
  
  targeting_on_each <- howmuchisinterest %>%
    bind_rows(howmuchislocation) %>%
    bind_rows(howmuchisage) %>%
    bind_rows(howmuchisgender) %>%
    bind_rows(howmuchcustom) %>%
    bind_rows(howmuchlookalike) %>%
    bind_rows(howmuchlanguage) %>%
    mutate(total = total_sppppeen$total_spend) %>%
    mutate(total_ads = total_sppppeen$num_ads) %>%
    mutate(perc = spend_per/total*100) %>%
    mutate(perc_ads = ads_per/total_ads*100) %>%
    arrange(desc(perc))
  
  return(targeting_on_each)
}

relationshipstuff <- "Widowed|Recently moved|Away|[r|R]elationship|Parents|Partner|Separated|Divorced|Single|Complicated|Married|Engaged|Newlywed|Civil Union|Unspecified|Newly engaged"


add_ribbons <- function(x, adv, col) {
  x %>%
    tab_options(table.width = pct(100)) %>%
    tab_style(
      style = cell_borders(
        sides = c("left"),
        color = col,
        weight = px(18.5),
        style = "solid"
      ),
      locations = cells_body(
        columns = `Number of Advertisers`,
        rows = adv
      ))
}




get_targeting <- function(id, timeframe = "LAST_30_DAYS") {
  
  url <- "https://www.facebook.com/api/graphql/"
  
  heads_up <- httr::add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:93.0) Gecko/20100101 Firefox/93.0",
                                Accept = "*/*",
                                `Accept-Language` = 'en-US,en;q=0.5',
                                `X-FB-Friendly-Name` = "AdLibraryPageAudienceTabQuery",
                                `X-FB-LSD`= "AVrNiQCSUnA",
                                `Alt-Used`= "www.facebook.com",
                                `Sec-Fetch-Dest`= "empty",
                                `Sec-Fetch-Mode`= "cors",
                                `Sec-Fetch-Site`= "same-origin",
                                # `Accept-Encoding` = "gzip, deflate, br",
                                `Content-Type` = "application/x-www-form-urlencoded",
                                Connection = "keep-alive"
  )
  
  
  if(timeframe == "LAST_30_DAYS"){
    
    # audienceTimeframe <- "%7B%22audienceTimeframe%22%3A%22LAST_30_DAYS%22%2C%22"
    da_body <- glue::glue("av=0&__user=0&__a=1&__dyn=7xeUmxa3-Q8zo5ObwKBWobVo9E4a2i5U4e1FxebzEdF8ixy7EiwvoWdwJwCwAwgU2lxS6Ehwem0nCqbwgE3awbG78b87C1xwEwgolzUO0n2US2G3i1ywa-2l0Fwwwi831wnFokwyx2cw8WfK6E5i3e4U3mxOu2S2W2K7o725U4q0HUkyE9E11EbodEGdw46wbLwiU8U6C2-&__csr=&__req=m&__hs=19237.BP%3ADEFAULT.2.0.0.0.0&dpr=1&__ccg=EXCELLENT&__rev=1006139712&__s=ll61s1%3Axn89ey%3Admpplc&__hsi=7138774996758193009&__comet_req=0&lsd=AVrNiQCSYrc&jazoest=2981&__spin_r=1006139712&__spin_b=trunk&__spin_t=1662125577&__jssesw=1&fb_api_caller_class=RelayModern&fb_api_req_friendly_name=AdLibraryPageAudienceTabQuery&variables=%7B%22audienceTimeframe%22%3A%22LAST_30_DAYS%22%2C%22viewAllPageID%22%3A%22{id}%22%7D&server_timestamps=true&doc_id=4756112137823411") %>% as.character()
    
  } else if (timeframe == "LAST_7_DAYS"){
    
    # audienceTimeframe <- "%7B%22"
    da_body <- glue::glue("av=0&__user=0&__a=1&__dyn=7xeUmxa3-Q8zo5ObwKBWobVo9E4a2i5U4e1FxebzEdF8aUuxa1ZzES2S2q2i13w9m7oqx60Vo1upEK12wcG0KEswIwuo662y11xmfz81sbzoaEd86a0HU9k2C2218wc61uBxi2a48O0zE-Uqwl8cUjwdq79UbobEaUtws8nwhE2LxiawCw46wJwSyES0gq0K-1bwzwqobU&__csr=&__req=f&__hs=19245.BP%3ADEFAULT.2.0.0.0.0&dpr=1&__ccg=EXCELLENT&__rev=1006179750&__s=njkc5w%3A6o847a%3A9gcoa8&__hsi=7141736891942848978&__comet_req=0&lsd=AVrbeuAiHJg&jazoest=21000&__spin_r=1006179750&__spin_b=trunk&__spin_t=1662815197&__jssesw=1&fb_api_caller_class=RelayModern&fb_api_req_friendly_name=AdLibraryPageAudienceTabQuery&variables=%7B%22audienceTimeframe%22%3A%22LAST_7_DAYS%22%2C%22viewAllPageID%22%3A%22{id}%22%7D&server_timestamps=true&doc_id=4756112137823411") %>% as.character()
    
  } else if (timeframe == "LAST_90_DAYS"){
    
    da_body <- glue::glue("av=0&__user=0&__a=1&__dyn=7xeUmxa3-Q8zo5ObwKBWobVo9E4a2i5U4e1FxebzEdF8aUuxa1ZzES2S2q2i13w9m7oqx60Vo1upEK12wcG0KEswIwuo662y11xmfz81sbzoaEd86a0HU9k2C2218wc61uBxi2a48O3u1mzXxG1kwPxe3C0D8sDwJwKwHxS1Mxu16wa-58G2q0gq2S3qazo11E2XU4K2e1FwLw8O2i&__csr=&__req=h&__hs=19301.BP%3ADEFAULT.2.0.0.0.0&dpr=1&__ccg=EXCELLENT&__rev=1006553893&__s=20shv5%3A62a2bj%3A6goj90&__hsi=7162612241770415577&__comet_req=0&lsd=AVohzhTn68E&jazoest=2965&__spin_r=1006553893&__spin_b=trunk&__spin_t=1667675618&__jssesw=1&fb_api_caller_class=RelayModern&fb_api_req_friendly_name=AdLibraryPageAudienceTabQuery&variables=%7B%22audienceTimeframe%22%3A%22LAST_90_DAYS%22%2C%22viewAllPageID%22%3A%22{id}%22%7D&server_timestamps=true&doc_id=4756112137823411") %>% as.character()
    
    url <- "https://www.facebook.com/api/graphql/"
    
    heads_up <- httr::add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64; rv:93.0) Gecko/20100101 Firefox/93.0",
                                  Accept = "*/*",
                                  `Accept-Language` = 'en-US,en;q=0.5',
                                  `X-FB-Friendly-Name` = "AdLibraryPageAudienceTabQuery",
                                  `X-FB-LSD`= "AVrNiQCSUnA",
                                  `Alt-Used`= "www.facebook.com",
                                  `Sec-Fetch-Dest`= "empty",
                                  `Sec-Fetch-Mode`= "cors",
                                  `Sec-Fetch-Site`= "same-origin",
                                  # `Accept-Encoding` = "gzip, deflate, br",
                                  `Content-Type` = "application/x-www-form-urlencoded",
                                  Connection = "keep-alive"
    )
    
  }
  
  
  
  
  
  posted = httr::POST(url, heads_up, body = da_body)
  
  contentwise <- httr::content(posted)
  
  rate_limit <- str_detect(as.character(contentwise), "Rate limit exceeded")
  if(rate_limit){
    stop(as.character(contentwise))
  }
  
  
  
  out_raw <- contentwise %>%
    rvest::html_nodes("body") %>%
    rvest::html_nodes("p") %>%
    as.character() %>% str_remove_all("</p>|<p>") %>%
    jsonlite::fromJSON()  %>%
    purrr::pluck("data") %>%
    purrr::pluck("page") %>%
    purrr::pluck("ad_library_page_targeting_insight")
  
  
  summary_dat <- out_raw %>%
    purrr::pluck("ad_library_page_targeting_summary") %>%
    dplyr::bind_rows()
  
  if(nrow(summary_dat) > 1){
    
    summary_dat <- summary_dat %>%
      dplyr::slice(which(summary_dat$detailed_spend$currency == summary_dat$main_currency)) %>%
      dplyr::select(-detailed_spend)
    
  }
  
  targeting_details_raw <- out_raw[!(names(out_raw) %in% c("ad_library_page_targeting_summary", "ad_library_page_has_siep_ads"))]
  
  # names(targeting_details_raw)
  
  res <- targeting_details_raw %>%
    purrr::discard(purrr::is_empty) %>%
    purrr::imap_dfr(~{.x %>% dplyr::mutate(type = .y %>% stringr::str_remove("ad_library_page_targeting_"))}) %>%
    dplyr::bind_cols(summary_dat) %>%
    dplyr::mutate(internal_id = id)
  
  return(res)
  
}

get_targeting <- suppressWarnings(get_targeting)

append_date_suffix <- function(dates){
  dayy <- lubridate::day(dates)
  suff <- case_when(dayy %in% c(11,12,13) ~ "th",
                    dayy %% 10 == 1 ~ 'st',
                    dayy %% 10 == 2 ~ 'nd',
                    dayy %% 10 == 3 ~'rd',
                    TRUE ~ "th")
  paste0(dayy, suff)
}

create_date <- function(x) {
  the_date <- format(x, "%b %d")
  the_date <- ifelse(str_detect(the_date, " 0"),
                     str_remove(the_date, "0"),
                     the_date)
  str_replace(the_date, 
              as.character(lubridate::day(x)), 
              append_date_suffix(x))
}


scale_fill_parties <- function(...){
  ggplot2:::manual_scale(
    'fill', 
    values = setNames(color_dat$colors, color_dat$party), 
    ...
  )
}
scale_color_parties <- function(...){
  ggplot2:::manual_scale(
    'color', 
    values = setNames(color_dat$colors, color_dat$party), 
    ...
  )
}


walk_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::walk(.x, f, ...)
}

map_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map(.x, f, ...)
}

map_dfr_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map_dfr(.x, f, ...)
}

map_chr_progress <- function(.x, .f, ...) {
  .f <- purrr::as_mapper(.f, ...)
  pb <- progress::progress_bar$new(
    total = length(.x), 
    format = " (:spin) [:bar] :percent | :current / :total | eta: :eta",
    # format = " downloading [:bar] :percent eta: :eta",
    force = TRUE)
  
  f <- function(...) {
    pb$tick()
    .f(...)
  }
  purrr::map_chr(.x, f, ...)
}





retrieve_reports <- function(ds, coununtry) {
  
  
  
  unlink("node_modules", recursive = T, force = T)
  unlink("out", recursive = T, force = T)
  
  
  if(!("playwrightr" %in% tibble::as_tibble(installed.packages())$Package)){
    system("conda create -n pw python=3.7")
    system("conda activate pw")
    system("conda config --add channels conda-forge")
    system("conda config --add channels microsoft")
    system("conda install playwright")
    system("playwright install")
    remotes::install_github("benjaminguinaudeau/playwrightr")
    
  }
  
  library(playwrightr)
  # library(tidyverse)
  options(timeout=300)
  
  source("utils.R")
  
  options(python_init = TRUE)
  
  # cntry_str <- "NL"
  time_preset <- commandArgs(trailingOnly = TRUE)
  time_preset <- "last_30_days"
  
  # install.packages("pacman")
  pacman::p_load(
    reticulate,
    vroom,
    progress,
    janitor,
    fs,
    tidyr,
    # appendornot,
    countrycode,
    dplyr,
    stringr,
    lubridate,
    purrr,
    glue,
    rvest,
    cli,
    digest,
    readr,
    piggyback
  )

  
  if(Sys.info()[["sysname"]]=="Windows"){
    
    pw_init(use_xvfb = F)
  } else{
    
    conda_install(packages = "xvfbwrapper", pip = T)
    
    print("installed xvfbwrapper")
    conda_install(packages = "playwright", pip = T)
    print("installed playwright")
    
    pw_init(use_xvfb = T)
    system("playwright install")
  }
  
  
  browser_df <- browser_launch(
    headless = F,
    browser = "firefox",
    user_agent = NULL,
    user_data_dir = "out"
  )
  
  
  
  
  
  print("headlesss")
  # Create a new page
  
  # page_df <- new_page(browser_df)
  page_df <- browser_df %>%
    glimpse
  
  
  
  print("sooo22")
  
  on <- function(page_df, event, lambda_string) {
    playwrightr:::py_run(glue('{page_df$page_id}.on("{event}", {lambda_string})'))
    return(page_df)
  }
  off <- function(page_df, event, lambda_string) {
    playwrightr:::py_run(glue(
      '{page_df$page_id}.remove_listener("{event}", {lambda_string})'
    ))
    return(page_df)
  }
  
  print("soooxx")
  execute_script <- function (page_df, script) {
    playwrightr:::py_run(glue("d = {{page_df$page_id}}.evaluate('{{script}}')"))
  }
  
  page_df %>%
    goto("https://www.facebook.com/ads/library/report")
  print("visit website")
  Sys.sleep(2)
  
  # page_df %>% screenshot("/data/res/facebook_add_reports/test.png")
  
  try({
    page_df %>%
      get_by_test_id("cookie-policy-manage-dialog-accept-button") %>%
      slice(1) %>%
      click() %>%
      screenshot("/data/res/facebook_add_reports/test.png")
  })
  
  
  # Write post-data string to disk into tmp
  tmp_post_data_string <-
    paste0(digest::digest("YOOYOOo"), ".txt")#  tempfile(fileext = ".txt")
  # page_df %>% on("request", glue::glue('lambda request: print(request.url)'))
  # page_df %>% on("request", glue::glue('lambda request: print(request.post_data) if (request.method == "POST" and "report/v2/download" in request.url) else None'))
  # page_df %>% on("request", glue::glue('lambda request: print(request.post_data) if (request.method == "POST" and "graphql" in request.url) else None'))
  page_df %>% on(
    "request",
    glue::glue(
      'lambda request: open("{tmp_post_data_string}", "w").write(request.post_data) if (request.method == "POST" and "report/v2/download" in request.url) else None'
    )
  )
  page_df %>% on(
    "request",
    glue::glue(
      'lambda request: open("{tmp_post_data_string}", "w").write(request.post_data) if (request.method == "POST" and "graphql" in request.url) else None'
    )
  )
  print("some other stuff")
  # Print Console
  # tmp_download_link <- tempfile()
  tmp_download_link <-
    paste0(digest::digest("sdff"), ".txt")#  tempfile(fileext = ".txt")
  
  page_df %>% on("console",
                 "lambda msg: open('{tmp_download_link}', 'w').write(msg.text)")
  
  # First click to obtain the request post-body-data
  page_df %>%
    get_by_text("Download report") %>%
    slice(2) %>%
    click()
  
  # Print download path
  tmp_download_path <-
    paste0(digest::digest("sdsdfsdfdff"), ".txt")#
  page_df %>% on(
    "download",
    glue::glue(
      'lambda download: open("{tmp_download_path}", "w").write(download.path())'
    )
  )
  print("some other stuff 2")
  
  data_string <- readLines(tmp_post_data_string, warn = F) %>%
    str_squish() %>%
    glimpse
  
  # full_cntry_list$iso2c
  # countries <- tibble::tibble(country = c("NL", "DE", "CA", "FR", "US"))
  # countries <-
  #   tibble::tibble(country = countrycode::codelist$iso2c) %>%
  #   filter(!is.na(country)) %>%
  #   glimpse
  # countries <- fs::file_info(dir("/data", recursive = T, full.names = T)) %>%
  #   filter(size > 1) %>%
  #   pull(path) %>%
  #   fs::path_dir() %>%
  #   fs::path_file() %>%
  #   unique
  # readr::write_rds(countries, "data/countries.rds")
  #
  # countries <- tibble::tibble(country = readr::read_rds("data/countries.rds")) %>%
  #   filter(!is.na(country)) %>%
  #   glimpse
  
  daysies <-
    tibble::tibble(day = lubridate::as_date(seq.int(
      lubridate::dmy("01-07-2019"), lubridate::today(), by = 1
    ))) %>%
    # days <- tibble::tibble(day = lubridate::as_date(seq.int(lubridate::dmy("15-07-2023"), lubridate::today(), by = 1))) #%>%
    head(-2)
  print("afterdaises")
  
  
  dt <- expand_grid(coununtry, daysies) %>%
    rename(country = coununtry) %>% 
    glimpse
  
  
  # try({
  #   all_reports_old <- readRDS(paste0("logs/all_reports_", time_preset, ".rds"))
  # })
  # 
  # if(!exists("all_reports_old")){
  #   all_reports_old <- c()
  # }
  
  # dir("report/ES", full.names = T, recursive = T) %>% sort
  dir.create("extracted")
  dir.create("report")
  print("creation")
  
  library(tidyverse)
  # readRDS("reports/US/last_30_days.rds") %>% count(date)
  
  # thosearethere <- dir("reports") %>% 
  #   map_dfr_progress(~{
  #     
  #     if(file.exists(paste0("reports/", .x,"/", time_preset, ".rds"))){
  #       return(read_rds(paste0("reports/", .x,"/", time_preset, ".rds")))
  #     }
  #     
  #     }) 
  # 
  # if(nrow(thosearethere)!=0){
  #   thosearethere <- thosearethere %>% 
  #     distinct(cntry, date) %>% 
  #     rename(day = date) %>% 
  #     mutate(day = lubridate::ymd(day))
  # } else {
  #   thosearethere <- thosearethere %>% mutate(cntry = NA, day = lubridate::ymd("2020-01-01"))
  # }
  
  
  download_it <- function(download_url, file_name) {
    download.file(download_url,
                  file_name,
                  quiet = T,
                  mode = "wb")   
  }
  
  download_it_now <- safely(download_it, quiet = F)
  
  # if(file.exists("blacklist.csv")) {
  #   blacklist <- read_csv("blacklist.csv") %>% mutate(day = lubridate::ymd(day)) %>% filter(day <= lubridate::ymd("2023-12-31"))
  # } else {
  #   blacklist <- tibble(country = "", day = lubridate::ymd("2020-01-01"))
  # }
  
  rawlings <- dt %>%
    filter(country == coununtry) %>% 
    # arrange(day, country != "RU") %>%
    # filter(country %in% cntries) %>%
    arrange(desc(day), country) #%>%
  # anti_join(thosearethere) %>%
  # anti_join(blacklist)
  
  thoseneedtobehere <- rawlings %>%
    # filter(day >= (lubridate::today() - lubridate::days(7))) %>% 
    filter(day == (lubridate::ymd(ds))) 
  
  # nicetohave <- rawlings %>% 
  #   # filter(day >= (lubridate::today() - lubridate::days(7))) %>% 
  #   filter(day >= (lubridate::ymd("2022-01-01"))) %>%
  #   arrange(desc(day), country) %>% 
  #   sample_n(1000)
  
  
  
  thoseneedtobehere %>%
    # bind_rows(nicetohave) %>%
    # filter(country == "BA") %>% 
    # filter(day>=lubridate::ymd("2024-01-01")) %>% 
    # tibble(country = "BA", 
    #        day = seq.Date(from = lubridate::ymd("2024-01-07"), 
    #                              to = lubridate::ymd("2024-01-07"), by = "1 day")) %>%
    # filter(day <= (lubridate::ymd("2024-01-01"))) %>% 
    slice(1:5000) %>%
    # sample_n(10) %>%
    split(1:nrow(.)) %>% #bashR::simule_map(1)
    walk_progress( ~ {
      
      
      # browser()
      file_name <-
        glue::glue("report/{.x$country}/{as.character(.x$day)}-{time_preset}.zip")
      # if (file_name %in% all_reports_old)
      #   return()
      
      cli::cli_alert_info(glue::glue("{.x$country} - {.x$day}"))
      
      path_dir <- fs::path_dir(file_name)
      if (!fs::dir_exists(path_dir))
        fs::dir_create(path_dir)
      
      # print(time_preset)
      
      if(length(time_preset)==0){
        
        print("ATTENTION FOR SOME REASON NO TIMEPRESET")
        
        # time_preset <- "last_7_days"
        time_preset <- "last_90_days"
        # time_preset <- "yesterday"
        # time_preset <- "lifelong"
        
      }
      
      
      js_code <-
        paste0(
          'fetch("https://www.facebook.com/api/graphql/',
          '", {"headers": {"accept": "*/*", "content-type": "application/x-www-form-urlencoded"}, "body": "',
          paste0(data_string, "&variables=%7B%22country%22%3A%22", .x$country ,"%22%2C%22reportDS%22%3A%22", as.character(.x$day) ,"%22%2C%22timePreset%22%3A%22", time_preset,"%22%7D"),
          '", "method": "POST", "mode": "cors", "credentials": "include" }).then(resp => resp.text()).then(data => console.log(data));'
        )
      
      print("twitch")
      
      page_df %>% execute_script(js_code)
      Sys.sleep(.1)
      print("apple sauce")
      
      download_url <- readLines(tmp_download_link, warn = F) %>%
        str_extract("\"https.*?\"") %>%
        str_remove_all("(^\")|(\"$)") %>%
        str_remove_all("\\\\") %>%
        glimpse
      if (is.na(download_url)) {
        if (!(.x$day %in% lubridate::as_date((lubridate::today() - lubridate::days(10)):lubridate::today()
        ))) {
          write(list(), file_name)
        }       
        
        print("ho")
        # print(.x)
        # debugonce(save_csv)
        # save_csv(.x, path = "blacklist.csv")
        # print("ho")
        
      } else if (str_detect(download_url, "facebook.com/help/contact/")) {
        cli::cli_alert_danger("Blocked")
        Sys.sleep(10)
        return("Blocked")
      } else {
        
        # try({
        res <- download_it_now(download_url, file_name)       
        # })
        if(!is.null(res$error)) return("Waitlisted")
        
      }
      
      
      
      Sys.sleep(runif(1, 0, .3))
    })
  
  
  
  
  
  # latest_available_date <- dir("extracted") %>% 
  #   keep(~str_detect(.x, cntry_str)) %>% 
  #   sort(decreasing = T) %>% 
  #   str_split("_") %>% unlist %>% .[2]
  # 
  # print("whats the latest available date")
  # 
  # 
  # if(length(latest_available_date)==0){
  #   print("its actually zero why")
  #   
  #   latest_available_date <- as.character(lubridate::today()-lubridate::days(4))
  # }
  
  
  print("NL DOWNLOADED")
  # dir.create("reports")
  # 
  # tat_path <- thosearethere %>% 
  #   mutate(path = paste0("report/", country, "/", day, "-", timeframe, ".zip")) %>% 
  #   drop_na(day)
  
  report_paths <- dir(paste0("report"), full.names = T, recursive = T) %>%
    sort(decreasing = T) %>%
    # setdiff(tat_path$path) %>%
    sort()
  # keep(~str_detect(.x, "2024-01-01")) %>% 
  # keep(~str_detect(.x, "last_7_days"))
  # .[200:202]
  
  # latest_dat <- tat_path %>%
  #   group_by(country) %>% 
  #   arrange(desc(day)) %>% 
  #   slice(1) %>% 
  #   ungroup()
  
  # session("https://github.com/favstats/meta_ad_reports/releases/tag/ZW-lifelong") %>% 
  #   html_elements(".mb-3") %>% 
  #   html_text() %>% str_squish()
  #   # html_children() %>% 
  #   str_detect("2024-01-01")
  # 
  #   https://github.com/favstats/meta_ad_reports/releases/download/ZW-lifelong/2024-01-01.rds
  
  progress_bar <- function(current, total, bar_width = 50) {
    # Calculate the percentage completed
    percent_done <- round(current / total * 100)
    
    # Number of filled positions in the bar
    filled_positions <- round(bar_width * percent_done / 100)
    
    # Create the progress bar string
    bar <- paste0("[", 
                  strrep("=", filled_positions), 
                  ">", 
                  strrep(" ", bar_width - filled_positions), 
                  "] ", 
                  percent_done, "%")
    
    # Print the progress bar and use carriage return to stay on the same line
    cat("\r", bar, sep = "")
    flush.console()
  }
  
  
  # full_repos$tag %>% unique %>% 
  #   walk(~{pb_release_delete(tag= .x)})
  
  # report_path <- report_paths[3]
  # report_path <- report_paths[str_detect(report_paths, "OM")][1]
  
  # releases <- pb_releases()
  # release_names <- full_repos$tag %>% unique
  
  for (report_path in report_paths) {
    print(report_path)
    progress_bar(which(report_path==report_paths, report_paths), total = length(report_paths))
    
    unzip(report_path, exdir = "extracted")
    
    rawww <-  str_split(report_path, "/") %>% unlist 
    
    cntry_str <- rawww[2]
    
    tframe <- str_remove(str_split(rawww, "-") %>% unlist() %>% .[length(.)], ".zip")
    the_date <- str_remove_all(rawww[3], paste0(".zip|-", tframe))   
    
    # cntry_name <- full_cntry_list %>% 
    #   filter(iso2c == cntry_str) %>% 
    #   pull(country)
    
    extracted_path <- dir("extracted", full.names = T, recursive = F) %>% 
      keep(~ str_detect(.x, "advert")) %>%
      keep(~ str_detect(.x, cntry_str) & str_detect(.x, as.character(lubridate::ymd(the_date)-1)) & str_detect(.x, tframe)) 
    
    if(length(extracted_path)==0){
      print("no data")
      next
    }
    
    # tframe <- str_extract(extracted_path, "yesterday|last_7_days|last_30_days|last_90_days|lifelong")
    
    thedata <- vroom::vroom(extracted_path, show_col_types = F) %>%
      janitor::clean_names() %>%
      mutate(date = str_extract(extracted_path, "\\d{4}-\\d{2}-\\d{2}")) %>%
      mutate_all(as.character) %>%
      mutate(path = extracted_path) %>%
      mutate(tf = tframe) %>%
      mutate(cntry = cntry_str)
    
    if (any(c("name_disclaimer_amount") %in% names(thedata))) {
      print("##within1")
      print(thedata)
      thedata <- thedata %>%
        filter(is.na(name_disclaimer_amount))  %>%
        janitor::remove_empty()
      print("##within2")
      print(thedata)
    }
    
    # print("helloo")
    
    if(nrow(thedata)==0){
      print("no data for some reason")
      next
    }
    
    thedata %>%
      readr::write_rds(paste0("report", ".rds"), compress = "xz")
    
    
    gc()
    
  }
  
  
  
  
  
  unlink("node_modules", recursive = T, force = T)
  unlink("out", recursive = T, force = T)
  
  print("################6")
  
  dir() %>%
    keep( ~ str_detect(.x, ".txt")) %>%
    discard( ~ str_detect(.x, "n_advertisers.txt|tstamp.txt")) %>%
    walk(file.remove)
  
  # all_reports_old <- readRDS("logs/all_reports.rds")
  
  print("################9")
  
  # all_reports <- dir("report", full.names = T, recursive = T)
  
  print("################10")
  
  # all_reports <- all_reports_old %>% 
  #   c(all_reports) %>% 
  #   unique()
  # print("################11")
  # 
  # saveRDS(all_reports, file = paste0("logs/all_reports_", time_preset, ".rds"))
  
  print("################12")
  
  unlink("report", recursive = T, force = T)
  unlink("extracted", recursive = T, force = T)
  
  
}
