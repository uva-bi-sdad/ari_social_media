#2019-07-10
#Mapping tools for King George County, VA

#devtools::install_github("dads2busy/dataplumbr")

library(data.table)
library(jsonlite)
library(dataplumbr)
library(dplyr)
library(stringr)

########FAST FOOD########
# get fast food search from google api
url <- paste0("https://maps.googleapis.com/maps/api/place/textsearch/json?query=fast+food+in+King+George+County+Virginia&key=", Sys.getenv("GOOGLE_API_KEY"))
goog1 <- fromJSON(url)

# create data.table of fast food locations
name <- goog1$results$name
addr <- goog1$results$formatted_address
placeid <- goog1$results$place_id
loc <- goog1$results$geometry$location
gresult <- data.table(name, addr, placeid, loc)

# get geographies from fcc api
locations <- setDT(dataplumbr::loc.lats_lons2geo_areas(gresult$placeid, gresult$lat, gresult$lng), keep.rownames = T)
setnames(locations, "rn", "placeid")

# merge
fnl <- merge(gresult, locations, by = "placeid")

# get the ones from King George County
fast_food <- rbind(fast_food, fnl[county_name=="King George", .(name, addr, lat, lng, county_fips, county_name)][order(name)])



########PLACES OF WORSHIP#######
# Catholic churches
# get places of worship search from google api
url2 <- paste0("https://maps.googleapis.com/maps/api/place/textsearch/json?query=catholic+church+in+King+George+County+Virginia&key=", Sys.getenv("GOOGLE_API_KEY"))
goog2 <- fromJSON(url2)
name <- goog2$results$name
addr <- goog2$results$formatted_address
placeid <- goog2$results$place_id
loc <- goog2$results$geometry$location
gresult2 <- data.table(name, addr, placeid, loc)
# get geographies from fcc api
locations <- setDT(dataplumbr::loc.lats_lons2geo_areas(gresult2$placeid, gresult2$lat, gresult2$lng), keep.rownames = T)
setnames(locations, "rn", "placeid")
# merge
fnl2 <- merge(gresult2, locations, by = "placeid")
# get the ones from King George county
places_of_worship <- rbind(places_of_worship, fnl2[county_name=="King George", .(name, addr, lat, lng, county_fips, county_name)][order(name)])



# Jehovas Witnesses - none in King George County


# Apostolic churches
url4 <- paste0("https://maps.googleapis.com/maps/api/place/textsearch/json?query=apostolic+in+King+George+County+Virginia&key=", Sys.getenv("GOOGLE_API_KEY"))
goog4 <- fromJSON(url4)
name <- goog4$results$name
addr <- goog4$results$formatted_address
placeid <- goog4$results$place_id
loc <- goog4$results$geometry$location
gresult4 <- data.table(name, addr, placeid, loc)
# get geographies from fcc api
locations <- setDT(dataplumbr::loc.lats_lons2geo_areas(gresult4$placeid, gresult4$lat, gresult4$lng), keep.rownames = T)
setnames(locations, "rn", "placeid")
# merge
fnl4 <- merge(gresult4, locations, by = "placeid")
# get the ones from King George County
places_of_worship <- rbind(places_of_worship, fnl4[county_name=="King George", .(name, addr, lat, lng, county_fips, county_name)][order(name)])


# Baptist churches
url5 <- paste0("https://maps.googleapis.com/maps/api/place/textsearch/json?query=baptist+in+King+George+County+Virginia&key=", Sys.getenv("GOOGLE_API_KEY"))
goog5 <- fromJSON(url5)
name <- goog5$results$name
addr <- goog5$results$formatted_address
placeid <- goog5$results$place_id
loc <- goog5$results$geometry$location
gresult5 <- data.table(name, addr, placeid, loc)
# get geographies from fcc api
locations <- setDT(dataplumbr::loc.lats_lons2geo_areas(gresult5$placeid, gresult5$lat, gresult5$lng), keep.rownames = T)
setnames(locations, "rn", "placeid")
# merge
fnl5 <- merge(gresult5, locations, by = "placeid")
# get the ones from King George County
data <- fnl5[county_name=="King George", .(name, addr, lat, lng, county_fips, county_name)][order(name)]
data <- data %>%
  filter(str_detect(name, "Baptist"))
places_of_worship <- rbind(places_of_worship, data)



# Episcopal churches
url6 <- paste0("https://maps.googleapis.com/maps/api/place/textsearch/json?query=episcopal+in+King+George+County+Virginia&key=", Sys.getenv("GOOGLE_API_KEY"))
goog6 <- fromJSON(url6)
name <- goog6$results$name
addr <- goog6$results$formatted_address
placeid <- goog6$results$place_id
loc <- goog6$results$geometry$location
gresult6 <- data.table(name, addr, placeid, loc)
# get geographies from fcc api
locations <- setDT(dataplumbr::loc.lats_lons2geo_areas(gresult6$placeid, gresult6$lat, gresult6$lng), keep.rownames = T)
setnames(locations, "rn", "placeid")
# merge
fnl6 <- merge(gresult6, locations, by = "placeid")
# get the ones from King George County
data <- fnl6[county_name=="King George", .(name, addr, lat, lng, county_fips, county_name)][order(name)]


# Lutheran churches
url7 <- paste0("https://maps.googleapis.com/maps/api/place/textsearch/json?query=lutheran+in+King+George+County+Virginia&key=", Sys.getenv("GOOGLE_API_KEY"))
goog7 <- fromJSON(url7)
name <- goog7$results$name
addr <- goog7$results$formatted_address
placeid <- goog7$results$place_id
loc <- goog7$results$geometry$location
gresult7 <- data.table(name, addr, placeid, loc)
# get geographies from fcc api
locations <- setDT(dataplumbr::loc.lats_lons2geo_areas(gresult7$placeid, gresult7$lat, gresult7$lng), keep.rownames = T)
setnames(locations, "rn", "placeid")
# merge
fnl7 <- merge(gresult7, locations, by = "placeid")
# get the ones from King George County
places_of_worship <- rbind(places_of_worship, fnl7[county_name=="King George", .(name, addr, lat, lng, county_fips, county_name)][order(name)])


# Christian centers
url8 <- paste0("https://maps.googleapis.com/maps/api/place/textsearch/json?query=christian+center+in+King+George+County+Virginia&key=", Sys.getenv("GOOGLE_API_KEY"))
goog8 <- fromJSON(url8)
name <- goog8$results$name
addr <- goog8$results$formatted_address
placeid <- goog8$results$place_id
loc <- goog8$results$geometry$location
gresult8 <- data.table(name, addr, placeid, loc)
# get geographies from fcc api
locations <- setDT(dataplumbr::loc.lats_lons2geo_areas(gresult8$placeid, gresult8$lat, gresult8$lng), keep.rownames = T)
setnames(locations, "rn", "placeid")
# merge
fnl8 <- merge(gresult8, locations, by = "placeid")
# get the ones from King George County
data <- fnl8[county_name=="King George", .(name, addr, lat, lng, county_fips, county_name)][order(name)]
data <- data %>%
  filter(str_detect(name, "Christian Center"))
places_of_worship <- rbind(places_of_worship, data)


# Pentecostal
url9 <- paste0("https://maps.googleapis.com/maps/api/place/textsearch/json?query=pentecostal+in+King+George+County+Virginia&key=", Sys.getenv("GOOGLE_API_KEY"))
goog9 <- fromJSON(url9)
name <- goog9$results$name
addr <- goog9$results$formatted_address
placeid <- goog9$results$place_id
loc <- goog9$results$geometry$location
gresult9 <- data.table(name, addr, placeid, loc)
# get geographies from fcc api
locations <- setDT(dataplumbr::loc.lats_lons2geo_areas(gresult9$placeid, gresult9$lat, gresult9$lng), keep.rownames = T)
setnames(locations, "rn", "placeid")
# merge
fnl9 <- merge(gresult9, locations, by = "placeid")
# get the ones from King George County
data <- fnl9[county_name=="King George", .(name, addr, lat, lng, county_fips, county_name)][order(name)]
data <- data %>%
  filter(str_detect(name, "Pentecostal")|str_detect(name, "Family Life"))
places_of_worship <- rbind(places_of_worship, data)


# Presbyterian
url10 <- paste0("https://maps.googleapis.com/maps/api/place/textsearch/json?query=presbyterian+in+Caroline+County+Virginia&key=", Sys.getenv("GOOGLE_API_KEY"))
goog10 <- fromJSON(url10)
name <- goog10$results$name
addr <- goog10$results$formatted_address
placeid <- goog10$results$place_id
loc <- goog10$results$geometry$location
gresult10 <- data.table(name, addr, placeid, loc)
# get geographies from fcc api
locations <- setDT(dataplumbr::loc.lats_lons2geo_areas(gresult10$placeid, gresult10$lat, gresult10$lng), keep.rownames = T)
setnames(locations, "rn", "placeid")
# merge
fnl10 <- merge(gresult10, locations, by = "placeid")
# get the ones from Caroline County
places_of_worship <- rbind(places_of_worship, fnl10[county_name=="Caroline", .(name, addr, lat, lng, county_fips, county_name)][order(name)])


# Methodist
url11 <- paste0("https://maps.googleapis.com/maps/api/place/textsearch/json?query=methodist+in+Caroline+County+Virginia&key=", Sys.getenv("GOOGLE_API_KEY"))
goog11 <- fromJSON(url11)
name <- goog11$results$name
addr <- goog11$results$formatted_address
placeid <- goog11$results$place_id
loc <- goog11$results$geometry$location
gresult11 <- data.table(name, addr, placeid, loc)
# get geographies from fcc api
locations <- setDT(dataplumbr::loc.lats_lons2geo_areas(gresult11$placeid, gresult11$lat, gresult11$lng), keep.rownames = T)
setnames(locations, "rn", "placeid")
# merge
fnl11 <- merge(gresult11, locations, by = "placeid")
# get the ones from Caroline County
places_of_worship <- rbind(places_of_worship, fnl11[county_name=="Caroline", .(name, addr, lat, lng, county_fips, county_name)][order(name)])



########LIQUOR STORES########
# get liquor stores search from google api
url12 <- paste0("https://maps.googleapis.com/maps/api/place/textsearch/json?query=liquor+store+in+Caroline+County+Virginia&key=", Sys.getenv("GOOGLE_API_KEY"))
goog12 <- fromJSON(url12)

# create data.table of fast food locations
name <- goog12$results$name
addr <- goog12$results$formatted_address
placeid <- goog12$results$place_id
loc <- goog12$results$geometry$location
gresult12 <- data.table(name, addr, placeid, loc)

# get geographies from fcc api
locations <- setDT(dataplumbr::loc.lats_lons2geo_areas(gresult12$placeid, gresult12$lat, gresult12$lng), keep.rownames = T)
setnames(locations, "rn", "placeid")

# merge
fnl12 <- merge(gresult12, locations, by = "placeid")

# get the ones from Caroline County
liquor_stores <- fnl12[county_name=="Caroline", .(name, addr, lat, lng, county_fips, county_name)][order(name)]

