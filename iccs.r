library(V8)
library(stringi)
library(httr)
library(rvest)
library(robotstxt)
library(jwatr) # github/hrbrmstr/jwatr
library(hrbrthemes)
library(purrrlyr)
library(rprojroot)
library(tidyverse)

# make field names great again
mfga <- function(x) {
  x <- tolower(x)
  x <- gsub("[[:punct:][:space:]]+", "_", x)
  x <- gsub("_+", "_", x)
  x <- gsub("(^_|_$)", "", x)
  x <- make.unique(x, sep = "_")
  x
}

report_urls <- read.csv(stringsAsFactors=FALSE, header=TRUE, text="url,start,end
https://www.icc-ccs.org/index.php/piracy-reporting-centre/live-piracy-report/details/169/, 1345, 1459
https://www.icc-ccs.org/piracy-reporting-centre/live-piracy-report/details/151/, 1137, 1339
https://www.icc-ccs.org/piracy-reporting-centre/live-piracy-map/details/146/, 885, 1138
https://www.icc-ccs.org/piracy-reporting-centre/live-piracy-report/details/144/, 625, 884
https://www.icc-ccs.org/index.php/piracy-reporting-centre/live-piracy-report/details/133/, 337, 623")

by_row(report_urls, ~sprintf(.x$url %s+% "%s", .x$start:.x$end), .to="url_list") %>%
  pull(url_list) %>%
  flatten_chr() -> target_urls

head(target_urls)
## [1] "https://www.icc-ccs.org/index.php/piracy-reporting-centre/live-piracy-report/details/169/1345"
## [2] "https://www.icc-ccs.org/index.php/piracy-reporting-centre/live-piracy-report/details/169/1346"
## [3] "https://www.icc-ccs.org/index.php/piracy-reporting-centre/live-piracy-report/details/169/1347"
## [4] "https://www.icc-ccs.org/index.php/piracy-reporting-centre/live-piracy-report/details/169/1348"
## [5] "https://www.icc-ccs.org/index.php/piracy-reporting-centre/live-piracy-report/details/169/1349"
## [6] "https://www.icc-ccs.org/index.php/piracy-reporting-centre/live-piracy-report/details/169/1350"

robotstxt::get_robotstxt("https://www.icc-ccs.org/")
## # If the Joomla site is installed within a folder such as at
## # e.g. www.example.com/joomla/ the robots.txt file MUST be
## # moved to the site root at e.g. www.example.com/robots.txt
## # AND the joomla folder name MUST be prefixed to the disallowed
## # path, e.g. the Disallow rule for the /administrator/ folder
## # MUST be changed to read Disallow: /joomla/administrator/
## #
## # For more information about the robots.txt standard, see:
## # http://www.robotstxt.org/orig.html
## #
## # For syntax checking, see:
## # http://www.sxw.org.uk/computing/robots/check.html
##
## User-agent: *
## Disallow: /administrator/
## Disallow: /cache/
## Disallow: /cli/
## Disallow: /components/
## Disallow: /images/
## Disallow: /includes/
## Disallow: /installation/
## Disallow: /language/
## Disallow: /libraries/
## Disallow: /logs/
## Disallow: /media/
## Disallow: /modules/
## Disallow: /plugins/
## Disallow: /templates/
## Disallow: /tmp/

s_GET <- safely(GET)

if (!file.exists("data/2017-icc-ccs-raw-httr-responses.rds")) {

  pb <- progress_estimated(length(target_urls))
  map(target_urls, ~{
    pb$tick()$print()
    Sys.sleep(5)
    s_GET(.x)
  }) -> httr_raw_responses

  write_rds(httr_raw_responses, "data/2017-icc-ccs-raw-httr-responses.rds")
} else {
  httr_raw_responses <- read_rds("data/2017-icc-ccs-raw-httr-responses.rds")
}

ctx <- v8()

good_responses <- keep(httr_raw_responses, ~!is.null(.x$result))

jwatr::response_list_to_warc_file(good_responses, "data/icc-good")

cols(
  attack_number = col_character(),
  attack_posn_map = col_character(),
  date = col_datetime(format = ""),
  date_time = col_datetime(format = ""),
  id = col_integer(),
  location_detail = col_character(),
  narrations = col_character(),
  type_of_attack = col_character(),
  type_of_vessel = col_character()
) -> pirate_cols

pb <- progress_estimated(length(good_responses))
map_df(good_responses, ~{

  pb$tick()$print()

  doc <- content(.x$result)

  html_nodes(doc, xpath=".//script[contains(., 'requirejs')]") %>%
    html_text() %>%
    stri_split_lines() %>%
    .[[1]] %>%
    grep("narrations_ro", ., value=TRUE) %>%
    sprintf("var dat = %s;", .) %>%
    ctx$eval()

  p <- ctx$get("dat", flatten=TRUE)

  keep(p[[1]], is.list) %>%
    map_df(~{
      list(
        field = mfga(.x[[3]]$label),
        value = .x[[3]]$value
      )
    }) %>%
    filter(value != "") %>%
    distinct(field, .keep_all = TRUE) %>%
    spread(field, value)

}) %>%
  type_convert(col_types = pirate_cols) %>%
  filter(stri_detect_regex(attack_number, "^[[:digit:]]")) %>%
  filter(lubridate::year(date) > 2012) %>%
  mutate(
    attack_posn_map = stri_replace_last_regex(attack_posn_map, ":.*$", ""),
    attack_posn_map = stri_replace_all_regex(attack_posn_map, "[\\(\\) ]", "")
  ) %>%
  separate(attack_posn_map, sep=",", into=c("lat", "lng")) %>%
  mutate(lng = as.numeric(lng), lat = as.numeric(lat)) -> pirate_df

write_rds(pirate_df, "data/pirate_df.rds")

mutate(pirate_df, year = lubridate::year(date), year_mon = as.Date(format(date, "%Y-%m-01"))) %>%
  count(year_mon) %>%
  ggplot(aes(year_mon, n)) +
  geom_segment(aes(xend=year_mon, yend=0)) +
  scale_y_comma() +
  labs(x=NULL, y=NULL,
       title="(Confirmed) Piracy Incidents per Month",
       caption="Source: International Chamber of Commerce Commercial Crime Services <https://www.icc-ccs.org/>") +
  theme_ipsum_rc(grid="Y")

world <- map_data("world")

mutate(pirate_df, year = lubridate::year(date)) %>%
  arrange(year) %>%
  mutate(year = factor(year)) -> plot_df

ggplot() +
  geom_map(data = world, map = world, aes(x=long, y=lat, map_id=region), fill="#b2b2b2") +
  geom_point(data = plot_df, aes(lng, lat, color=year), size=2, alpha=1/3) +
  ggalt::coord_proj("+proj=wintri") +
  viridis::scale_color_viridis(name=NULL, discrete=TRUE) +
  labs(x=NULL, y=NULL,
       title="Piracy Incidents per Month (Confirmed)",
       caption="Source: International Chamber of Commerce Commercial Crime Services <https://www.icc-ccs.org/>") +
  theme_ipsum_rc(grid="XY") +
  theme(legend.position = "bottom")
