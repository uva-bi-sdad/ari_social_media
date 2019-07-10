library(readr)
library(readxl)
library(tidyverse)

#Read in American Community Survey Social Characteristics for 2017 dataset
ACS_social_import <- read_csv("data/working/County_Level/ACS_17_Social_with_ann.csv")
ACS_social <- ACS_social_import[-c(1),] %>%
  select(GEO.id, GEO.id2, `GEO.display-label`, HC01_VC76, HC01_VC77, HC01_VC78, HC01_VC79, HC01_VC80, HC01_VC81, HC03_VC86, HC03_VC87, HC03_VC88, HC03_VC89, HC03_VC90, 
         HC03_VC91, HC03_VC92, HC03_VC95, HC03_VC96)

names(ACS_social) <- c("geo_id", "geo_id2", "geo_label", "number_enrolled", "number_enrolled_prek", "number_enrolled_k", "number_enrolled_elementary", "number_enrolled_hs",
                       "number_enrolled_college", "pct_under_9th", "pct_9th_12th", "pct_attained_hs", "pct_some_college", "pct_associates",
                       "pct_bachelors", "pct_grad_degree", "pct_hs_or_higher", "pct_college_or_higher")


#Read in American Community Survey Housing Characteristics for 2017 dataset
ACS_housing_import <- read_csv("data/working/County_Level/ACS_17_Housing_with_ann.csv")
ACS_housing <- ACS_housing_import[-c(1),] %>%
  select(GEO.id, GEO.id2, `GEO.display-label`, HC03_VC65, HC03_VC66, HC03_VC75, HC03_VC76, HC03_VC77, HC03_VC78, HC03_VC79, HC03_VC80)

names(ACS_housing) <- c("geo_id", "geo_id2", "geo_label", "pct_owner_occupied_homes", "pct_renter_occupied_homes", "pct_moved_after_2014", "pct_moved_2010_2014",
                        "pct_moved_2000_2009", "pct_moved_1990_1999", "pct_moved_1980_1989", "pct_moved_1979_earlier")


#Read in American Community Survey Economic Characteristics for 2017 dataset
ACS_economic_import <- read_csv("data/working/County_Level/ACS_17_Economic_with_ann.csv")
ACS_economic <- ACS_economic_import[-c(1),] %>%
  select(GEO.id, GEO.id2, `GEO.display-label`, HC03_VC12, HC03_VC28, HC03_VC29, HC03_VC30, HC03_VC31, HC03_VC32, HC03_VC33, HC01_VC36, HC03_VC75, HC03_VC76, 
         HC03_VC77, HC03_VC78, HC03_VC79, HC03_VC80, HC03_VC81, HC03_VC82, HC03_VC83, HC03_VC84, HC01_VC85, HC03_VC131, HC03_VC132, HC03_VC133, HC03_VC134, HC03_VC161)

names(ACS_economic) <- c("geo_id", "geo_id2", "geo_label", "pct_unemployed", "pct_commute_drive", "pct_commute_carpool", "pct_commute_public", 
                         "pct_commute_walk", "pct_commute_other", "pct_work_home", "commute_time_mean", "pct_income_under_10k", "pct_income_10k_15k",
                         "pct_income_15k_25k", "pct_income_25k_35k", "pct_income_35k_50k", "pct_income_50k_75k", "pct_income_75k_100k", "pct_income_100k_150k",
                         "pct_income_150k_200k", "pct_income_over_200k", "median_income", "pct_health_ins", "pct_private_health_ins", "pct_public_health_ins", "pct_no_health_ins",
                         "pct_impoverished")


#Read in American Community Survey Demographic and Housing Characteristics for 2017 dataset
ACS_demographic_import <- read_csv("data/working/County_Level/ACS_17_Demographic_Housing_with_ann.csv")
ACS_demographic <- ACS_demographic_import[-c(1),] %>%
  select(GEO.id, GEO.id2, `GEO.display-label`, HC01_VC03, HC01_VC09, HC01_VC10, HC01_VC11, HC01_VC12, HC01_VC13, HC03_VC83, HC03_VC84, HC03_VC85, HC03_VC86,
         HC03_VC87, HC03_VC88, HC03_VC93)

names(ACS_demographic) <- c("geo_id", "geo_id2", "geo_label", "population", "pop_under_5", "pop_5_9", "pop_10_14", "pop_15_19", "pop_20_24", "pct_white",
                            "pct_black", "pct_native", "pct_asian", "pct_pacific_islander", "pct_race_other", "pct_hispanic_latinx")


#merge all seperate ACS datasets with factors we want 
ACS <- ACS_demographic %>%
  right_join(ACS_economic, by=c("geo_id","geo_id2", "geo_label"))%>%
  right_join(ACS_housing, by=c("geo_id","geo_id2", "geo_label"))%>%
  right_join(ACS_social, by=c("geo_id","geo_id2", "geo_label"))
  





first_row <- read_excel("data/working/County_Level/2017_County_Health_Rankings_Virginia_Data-v2.xls", sheet = 4)
write.csv(first_row,'first_row.csv')

header <- sapply(read.csv("first_row.csv", nrow=2), paste, collapse="_")
header <- sapply(read.csv("first_row.csv", header=FALSE, nrow=2), paste, collapse="_")

county_health_ranked_measure_data <- read.csv("first_row.csv", header = FALSE, skip=2, col.names=header) %>% 
  select("X__1_FIPS", "X__2_State", "X__3_County","Poor.or.fair.health_..Fair.Poor", "X__45_Teen.Birth.Rate", "X__54_PCP.Ratio", "X__62_Preventable.Hosp..Rate",
         "X__88_..Unemployed", "Children.in.poverty_..Children.in.Poverty", "X__97_Income.Ratio", "X__104_Association.Rate", "Air.pollution...particulate.matter_Average.Daily.PM2.5",
         "Drinking.water.violations_Presence.of.violation", "X__114_..Severe.Housing.Problems", "X__124_..Long.Commute...Drives.Alone")

names(county_health_ranked_measure_data) <- c("fips", "state", "county", "pct_fair_poor_health", "teen_birth_rate", "pcp_ratio", "preventable_hosp_rate", "pct_unemployed",
                            "pct_children_poverty", "income_ratio", "assoc_rate", "air_pollution", "water_violations", "severe_housing_problems", "drive_along_long_commute")

