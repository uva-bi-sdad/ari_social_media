#2019-07-16
#lollipop plotting

library(tidyverse)
library(ggplot2)
library(dplyr)
library(viridis)
library(reshape2)

setwd("~/ari_social_media/data/working/County_Level")
data <- read_csv("county_level_indicators_w_imputation.csv")

###### Lollipop plot for Virginia #####
# read in counties of interest
# Caroline, Essex, Hanover, King and Queen, King George, King William, Spotsylvania, Stafford
data_virginia <- data[c(17, 28, 42, 48, 49, 50, 85, 86),]
data_all_va <- data[1:133,]
total_va_population <- sum(data_all_va$population)
data_virginia$incomesmall <- (data_virginia$median_income / 1000)

#selecting variables of interest
data_virginia2 <- melt(data_virginia %>% select(county, incomesmall, pct_no_health_ins, pct_impoverished,
                                                pct_owner_occupied_homes, pct_unemployed, severe_housing_problems))

data_virginia2$county <- factor(data_virginia2$county, levels = c("Caroline County", "Stafford County", "Spotsylvania County", "King William County", "King George County", "King and Queen County", "Hanover County", "Essex County"))


# converting percentages to number of people to convert back to % of all Virginians to use for dashed line
incomesmall <- ((sum(data_all_va$median_income * data_all_va$population)) / total_va_population) / 1000
people_no_health_ins <- ((data_all_va$pct_no_health_ins / 100) * data_all_va$population)
people_impoverished <- ((data_all_va$pct_impoverished / 100) * data_all_va$population)
people_owner_occupied <- ((data_all_va$pct_owner_occupied_homes / 100) * data_all_va$population)
people_unemployed <- ((data_all_va$pct_unemployed / 100) * data_all_va$population)
people_severe_housing <- ((data_all_va$severe_housing_problems / 100) * data_all_va$population)

# converting back to percentages
pct_no_health_ins <- ((sum(people_no_health_ins)) / total_va_population) * 100
pct_impoverished <- ((sum(people_impoverished)) / total_va_population) * 100
pct_owner_occupied_homes <- ((sum(people_owner_occupied)) / total_va_population) * 100
pct_unemployed <- ((sum(people_unemployed)) / total_va_population) * 100
severe_housing_problems <- ((sum(people_severe_housing)) / total_va_population) * 100
all_mean <- data.frame(variable = c("incomesmall", "pct_no_health_ins", "pct_impoverished", 
                                    "pct_owner_occupied_homes", "pct_unemployed", "severe_housing_problems"),
                       value = c(incomesmall, pct_no_health_ins, pct_impoverished,
                                 pct_owner_occupied_homes, pct_unemployed, severe_housing_problems))

data_virginia2names<-c(
  "county"="County",
  "incomesmall"="Median Income (in thousands)",
  "pct_no_health_ins"="% No Health Insurance",
  "pct_impoverished"="% Impoverished",
  "pct_owner_occupied_homes"="% Owner Occupied Homes",
  "pct_unemployed"="% Unemployed",
  "severe_housing_problems"="% Severe Housing Problems"
)


# Virginia lollipop plot
plot_virginia <- ggplot(data_virginia2, aes(x=county, y=value)) +
  geom_segment(aes(x=county, xend=county, y=0, yend=value), color="#EB5F0C", lwd = 2) +
  geom_point(color="#EB5F0C", size=5, alpha=0.6) +
  theme_light() +
  coord_flip() +
  facet_wrap(~variable, scales="free", labeller = as_labeller(data_virginia2names), nrow=2, ncol=3) +
  geom_hline(data=all_mean, aes(yintercept=value), colour="#232D4B", linetype="dotted", size = 1) +
  labs(title = "Comparing Counties of Interest to all of Virginia",
       subtitle = "(Dashed line indicates value for all counties in Virginia)") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    strip.text=element_text(size=10, face="bold"),
    strip.background=element_rect(fill= "#232D4B"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.x=element_text(size=15),
    axis.text.y=element_text(size=15, colour = c("#EB5F0C", rep("#232D4B", 7)))
  )
plot_virginia






##### Lollipop plot for Oklahoma ####
# read in counties of interest
# Caddo, Comanche, Cotton, Grady, Kiowa, Stephens, Tillman
data_oklahoma <- data[c(141, 149, 150, 159, 171, 202, 204),]
data_all_ok <- data[134:210,]
total_ok_population <- sum(data_all_ok$population)
data_oklahoma$incomesmall <- (data_oklahoma$median_income / 1000)

data_oklahoma2 <- melt(data_oklahoma %>% select(county, incomesmall, pct_no_health_ins, pct_impoverished,
                                                pct_owner_occupied_homes, pct_unemployed, severe_housing_problems))

# converting percentages to number of people to convert back to % of all Oklahomans to use for dashed line
incomesmall <- ((sum(data_all_ok$median_income * data_all_ok$population)) / total_ok_population) / 1000
people_no_health_ins <- ((data_all_ok$pct_no_health_ins / 100) * data_all_ok$population)
people_impoverished <- ((data_all_ok$pct_impoverished / 100) * data_all_ok$population)
people_owner_occupied <- ((data_all_ok$pct_owner_occupied_homes / 100) * data_all_ok$population)
people_unemployed <- ((data_all_ok$pct_unemployed / 100) * data_all_ok$population)
people_severe_housing <- ((data_all_ok$severe_housing_problems / 100) * data_all_ok$population)

# converting back to percentages
pct_no_health_ins <- ((sum(people_no_health_ins)) / total_ok_population) * 100
pct_impoverished <- ((sum(people_impoverished)) / total_ok_population) * 100
pct_owner_occupied_homes <- ((sum(people_owner_occupied)) / total_ok_population) * 100
pct_unemployed <- ((sum(people_unemployed)) / total_ok_population) * 100
severe_housing_problems <- ((sum(people_severe_housing)) / total_ok_population) * 100
all_mean <- data.frame(variable = c("incomesmall", "pct_no_health_ins", "pct_impoverished", 
                                    "pct_owner_occupied_homes", "pct_unemployed", "severe_housing_problems"),
                       value = c(incomesmall, pct_no_health_ins, pct_impoverished,
                                 pct_owner_occupied_homes, pct_unemployed, severe_housing_problems))

data_oklahoma2names<-c(
  "county"="County",
  "incomesmall"="Median Income (in thousands)",
  "pct_no_health_ins"="% No Health Insurance",
  "pct_impoverished"="% Impoverished",
  "pct_owner_occupied_homes"="% Owner Occupied Homes",
  "pct_unemployed"="% Unemployed",
  "severe_housing_problems"="% Severe Housing Problems"
)


# Oklahoma lollipop plot
data_oklahoma2$county <- factor(data_oklahoma2$county, levels = c("Comanche County", "Tillman County", "Stephens County", "Kiowa County", "Grady County", "Cotton County", "Caddo County"))

plot_oklahoma <- ggplot(data_oklahoma2, aes(x=county, y=value)) +
  geom_segment(aes(x=county, xend=county, y=0, yend=value), color="#EB5F0C", lwd = 2) +
  geom_point(color="#EB5F0C", size=5, alpha=0.6) +
  theme_light() +
  coord_flip() +
  facet_wrap(~variable, scales="free", labeller = as_labeller(data_oklahoma2names), nrow=2, ncol=3) +
  geom_hline(data=all_mean, aes(yintercept=value), colour="#232D4B", linetype="dotted", size = 1) +
  labs(title = "Comparing Counties of Interest to all of Oklahoma",
       subtitle = "(Dashed line indicates value for all counties in Oklahoma)") +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 25, hjust = 0.5),
    plot.subtitle = element_text(size = 18, hjust = 0.5),
    strip.text=element_text(size=10, face="bold"),
    strip.background=element_rect(fill= "#232D4B"),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    axis.text.x=element_text(size=15),
    axis.text.y=element_text(size=15, colour = c("#EB5F0C", rep("#232D4B", 6)))
  )
plot_oklahoma
