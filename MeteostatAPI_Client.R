#-----------------------------------------
# Building an API Client for Meteostat
#-----------------------------------------

# required packages for our client
library(httr)
library(xml2)

library(magrittr)
library(pbapply)
library(magick)

library(ggplot2)
# Base Url of the API
url_api <- 'https://api.meteostat.net/'

# My personal API key
key <- 'OAqQp2LQ'

#-----------------------------------------
## Part 1: Extracting the products and API version by parsing the overview page's HTML

# check what we get back from there
reply <- GET(url_api)
http_status(reply)

# extract info from body
reply_content <- content(reply)
class(reply_content)
#View(reply_content)

# crawl for table
xml_prod <- xml_find_first(reply_content, "//table")

# extract the head of the table
prod_head <- xml_children(xml_prod)[[1]] %>%
  xml_contents() %>%
  xml_children() %>%
  xml_text()

# extract the body of the table
prod_body <- xml_children(xml_prod)[[2]] %>% 
  xml_children() %>%
  
  lapply(function(x){
    x <- xml_contents(x)
    xml_text(x)[seq(1, length(x), by = 2)]
  })

# create products data.frame
products <- do.call(rbind.data.frame, prod_body)
colnames(products) <- prod_head
#View(products)


#-----------------------------------------------------
## Part 2: communicate with one of the products APIs


# Meteostat Part 1: Search for a weather station
#------------------

# extract the search method parameters
method <- products[products$Method=='search', ]
# Enter a search parameter (?q= for a query, ?country= code for a country)
search.param <- '?q=Stockholm'

# assemble the request URL: 
#base URL and version, cast package and method to string, assure lowercase search parameter and append key on the end
url.search <-  paste0(url_api,'v1/',toString(method$Package),'/',toString(method$Method),tolower(search.param),'&key=',key)

# do the search
search.resp <- GET(url.search) %>% content()

# extract the station ID for the actual request
station.id <- search.resp$data[[4]][['id']] # Give number of station entry you wish to extract


# Meteostat Part 2: Query the data
#------------------

# extract the desired method parameters
method <- products[products$Method=='monthly', ]

# define paramters: start and end data for historic data in year-month format
start <- 'start=2010-01'
end <-  'end=2019-12'

# assemble the request URL: 
#base URL and version, cast package and method to string, assure lowercase search parameter and append key on the end
url.req <-  paste0(url_api,'v1/',toString(method$Package),'/',toString(method$Method),'?station=',station.id,'&',start,'&',end,'&key=',key)
url.req

# do the request
avail.resp <- GET(url.req) %>% content()

# Extract the values from the content:
records <- do.call(rbind.data.frame, avail.resp$data)


# Lastly: Plot the temperature and precipitation for the selected Weather Station
#---------------------

station.name <- search.resp$data[[4]][['name']]

library(lubridate)
records$date <-  as.Date(as.character(records$month), "%Y-%m")

ggplot(records,aes(x=month, y=temperature_mean))+
  geom_smooth() +
  ggtitle(label=paste0(station.name,' monthly temperature'))
