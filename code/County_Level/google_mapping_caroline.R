#2019-07-09
#Mapping tools

#devtools::install_github("dads2busy/dataplumbr")
#install.packages("urltools")

library(data.table)
library(jsonlite)
library(dataplumbr)
library(dplyr)
library(stringr)
library(urltools)
library(tidyr)
library(purrr)
library(readr)
library(viridis)
library(readxl)

queries <- c("fast+food", "catholic+church", "kingdom+hall+of+jehovas+witnesses", "apostolic", "baptist", "episcopal", "lutheran", "christian+center", "pentecostal", "presbyterian", "methodist", "synagogue", "temple", "liquor+store", "church")
counties <- c("Caroline+County+Virginia", "King+George+County+Virginia", "Stafford+County+Virginia", "Spotsylvania+County+Virginia", "Hanover+County+Virginia", "King+William+County+Virginia", "King+and+Queen+County+Virginia", "Essex+County+Virginia", "Comanche+County+Oklahoma", "Cotton+County+Oklahoma", "Stephens+County+Oklahoma", "Grady+County+Oklahoma", "Caddo+County+Oklahoma", "Kiowa+County+Oklahoma", "Tillman+County+Oklahoma")

get_query <- function(county, query){
  
  url <- str_c("https://maps.googleapis.com/maps/api/place/textsearch/json?query=", query, "+in+", county, "&key=", sep = "", Sys.getenv("GOOGLE_API_KEY"))
  
  goog1 <- try.try_try_try(fromJSON(url)) 
  
  # create data.table of locations
  name <- goog1$results$name
  addr <- goog1$results$formatted_address
  placeid <- goog1$results$place_id
  loc <- goog1$results$geometry$location
  type <- rep(str_replace_all(query, "\\+", " "), length(name))
  gresult <- data.table(name, addr, placeid, loc, type)
  
  # get geographies from fcc api
  locations <- setDT(dataplumbr::loc.lats_lons2geo_areas(gresult$placeid, gresult$lat, gresult$lng), keep.rownames = T)
  setnames(locations, "rn", "placeid")
  
  # merge
  fnl <- merge(gresult, locations, by = "placeid")
  
  #verify in correct county
  query_results <- fnl[county_name==str_replace(str_extract(county, ".+?(?=\\+County)"), "\\+", " "), .(name, addr, lat, lng, county_fips, county_name, type)][order(name)]
  
  return(query_results)
}

churches_food <- vector(mode = "list", length = 210)
length_vector <- vector()
index <- 1

for (i in c(1:20)){
  for (county in counties){
    for (query in queries){
      if(county==counties[15] & query==queries[13] | is.na(county==counties[15] & query==queries[13])){
        index <- index + 1
        next
      }
      churches_food[[index]] <- get_query(county, query)
      print(index)
      index <- index + 1
    }
  }
  
  length_vector <- c(length_vector, length(unique(as.data.frame(do.call(rbind, churches_food)))))
  
}

df <- as.data.frame(unique(as.data.frame(do.call(rbind, churches_food))))
library(maditr)
df2 <- df %>%
  dt_mutate(county_fips = unlist(county_fips),
            county_name = unlist(county_name))
fwrite(df2, "/home/jk9ra/ari_social_media/data/working/County_Level/churchs_fast_good_liquor.csv")

# churches_food <- map_df(.x = counties, .f = function(county) {
#   map2_df(.x = county, .y = queries, .f = get_query)
# })


food_churches <- read_csv("/home/jk9ra/ari_social_media/data/working/County_Level/churchs_fast_good_liquor.csv")%>%
  mutate(type = ifelse(type != "liquor store" & type != "fast food", "place of worship", type))

food_churches <- unique(food_churches)

#write_csv(food_churches, "/home/jk9ra/ari_social_media/data/working/County_Level/places_of_worship_fast_food_liquor.csv")


##Creating maps of places of worship, fast food locations, and liquor stores for focus counties in Virginia and Oklahoma

counties <- map_data("county")
va_county <- subset(counties, region == 'virginia')
va_county_list <- c('caroline', 'king george', 'stafford', 'spotsylvania', 'hanover', 'king william', 'king and queen', 'essex')
va_county_zoom <- subset(va_county, subregion %in% va_county_list)

ok_county <- subset(counties, region == 'oklahoma')

food_churches$type <- as.factor(food_churches$type)


#merging created database with database of army posts
bases <- read_excel("/home/jk9ra/ari_social_media/data/working/County_Level/military-bases.xlsx")

to_merge_bases <- bases %>%
  mutate(lat = as.numeric(str_extract(`Geo Point`, ".+?(?=,)")),
         lon = as.numeric(str_extract(`Geo Point`, "[^,]+$"))) %>%
  filter(`Site Name` == "Fort A P Hill" | `Site Name` == "Fort Sill")%>%
  select("COMPONENT", "Site Name", "lat", "lon")
names(to_merge_bases) <- c("type", "name", "lat", "lng")

to_merge_churches <- food_churches %>%
  select("name", "lat", "lng", "type")

merged_bases_churches <- rbind(to_merge_bases, to_merge_churches)

va_churches_et_al <- merged_bases_churches%>%
  filter(lat > 36.5)

ok_churches_et_al <- merged_bases_churches %>%
  filter(lat < 36.5)

#map for virginia
va_map <- ggplot() +
  theme_void() +
  geom_polygon(data = va_county, aes(x=long, y=lat, group = group), size = 1, fill = NA, color = "dark gray") + 
  geom_point(aes(lng, lat, color = type), alpha = 0.8, data = va_churches_et_al)+
  scale_color_viridis(discrete = TRUE)
va_map

va_map_zoom <- ggplot()+
  theme_void() +
  geom_polygon(data = va_county_zoom, aes(x=long, y=lat, group = group), size = 1, fill = NA, color = "dark gray") + 
  geom_point(aes(lng, lat, color = type), alpha = 0.8, data = va_churches_et_al)+
  scale_color_viridis(discrete = TRUE)
va_map_zoom

#map for oklahoma
ok_map <- ggplot() +
  theme_void() +
  geom_polygon(data = ok_county, aes(x=long, y=lat, group = group), size = 1, fill = NA, color = "dark gray") + 
  geom_point(aes(lng, lat, color = type), alpha = 0.8, data = ok_churches_et_al)+
  scale_color_viridis(discrete = TRUE)
ok_map
