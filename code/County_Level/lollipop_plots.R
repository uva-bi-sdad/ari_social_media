#2019-07-16
#lollipop plotting

library(tidyverse)
library(ggplot2)
library(dplyr)
library(viridis)
library(reshape2)
library(readxl)

setwd("~/ari_social_media/data/final")
data <- read_excel("community_embeddedness.xlsx")

###### Lollipop plot for Virginia #####
# read in counties of interest
# Caroline, Essex, Hanover, King and Queen, King George, King William, Spotsylvania, Stafford
data_virginia <- data[c(17, 28, 42, 48, 49, 50, 85, 86),]
# data_all_va <- data[1:133,]
# total_va_population <- sum(data_all_va$population)
# data_virginia$incomesmall <- (data_virginia$median_income / 1000)
names(data_virginia)[2] <- "county"
names(data_virginia)[14] <- "embeddedness"
mean_va <- mean(data_virginia$embeddedness)

#selecting variables of interest
data_virginia2 <- melt(data_virginia %>% select(county, embeddedness))

data_virginia2$county <- factor(data_virginia2$county, levels = c("Caroline County", "Essex County", "Hanover County", "King and Queen County", "King William County", "King George County", "Spotsylvania County", "Stafford County"))


# converting percentages to number of people to convert back to % of all Virginians to use for dashed line
# incomesmall <- ((sum(data_all_va$median_income * data_all_va$population)) / total_va_population) / 1000
# people_no_health_ins <- ((data_all_va$pct_no_health_ins / 100) * data_all_va$population)
# people_impoverished <- ((data_all_va$pct_impoverished / 100) * data_all_va$population)
# people_owner_occupied <- ((data_all_va$pct_owner_occupied_homes / 100) * data_all_va$population)
# people_unemployed <- ((data_all_va$pct_unemployed / 100) * data_all_va$population)
# people_severe_housing <- ((data_all_va$severe_housing_problems / 100) * data_all_va$population)

# converting back to percentages
# pct_no_health_ins <- ((sum(people_no_health_ins)) / total_va_population) * 100 
# pct_impoverished <- ((sum(people_impoverished)) / total_va_population) * 100
# pct_owner_occupied_homes <- ((sum(people_owner_occupied)) / total_va_population) * 100
# pct_unemployed <- ((sum(people_unemployed)) / total_va_population) * 100
# severe_housing_problems <- ((sum(people_severe_housing)) / total_va_population) * 100
# all_mean <- data.frame(variable = c("incomesmall", "pct_no_health_ins", "pct_impoverished", 
#                                    "pct_owner_occupied_homes", "pct_unemployed", "severe_housing_problems"),
#                       value = c(incomesmall, pct_no_health_ins, pct_impoverished,
#                                 pct_owner_occupied_homes, pct_unemployed, severe_housing_problems))

data_virginia2names<-c(
  "county"="County",
  "embeddedness"="Embeddedness score"
)


# Virginia lollipop plot
# plot_virginia <- ggplot(data_virginia2, aes(x=county, y=value)) +
#   geom_segment(aes(x=county, xend=county, y=value, yend=value), color="#EB5F0C", lwd = 2) +
#   geom_point(color="#EB5F0C", size=5, alpha=0.6) +
#   theme_light() +
#   coord_flip() +
#   facet_wrap(~variable, scales="free", labeller = as_labeller(data_virginia2names), nrow=2, ncol=3) +
#   geom_hline(aes(yintercept=0), colour="#232D4B", linetype="dotted", size = 1) +
#   labs(title = "Embeddedness Scores for Counties of Interest in Virginia") +
#   theme(
#     panel.grid.major.y = element_blank(),
#     panel.border = element_blank(),
#     axis.ticks.y = element_blank(),
#     plot.title = element_text(size = 25, hjust = 0.5),
#     plot.subtitle = element_text(size = 18, hjust = 0.5),
#     strip.text=element_text(size=10, face="bold"),
#     strip.background=element_rect(fill= "#232D4B"),
#     axis.title.x=element_blank(),
#     axis.title.y=element_blank(),
#     axis.text.x=element_text(size=15, angle=3, hjust=1),
#     axis.text.y=element_text(size=15, colour = c("#EB5F0C", rep("#232D4B", 7)))
#   )
# plot_virginia

##### Bar chart with embeddedness values for Virginia#####
ggplot(data_virginia2, aes(x=county, y=value)) +
  geom_bar(fill = c("#EB5F0C", rep("#232D4B", 7)), stat="identity") +
  theme_light() +
  geom_hline(aes(yintercept=mean_va), colour="#232D4B", linetype="dotted", size = 1) +
  geom_hline(aes(yintercept=0), colour="#232D4B", size = 0.1) +
  expand_limits(y = 1) +
  labs(title = "Embeddedness Scores for Counties of Interest in Virginia",
       subtitle = "(Dashed line indicates average for Virginia)") +
  labs(x = "County", y = "Embeddedness score") +
  theme(
    panel.grid = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x=element_text(size = 22, angle=20, hjust=1, color = c("#EB5F0C", rep("#232D4B", 7))),
    plot.title = element_text(size = 28, hjust = 0.5, color = "#232D4B"),
    plot.subtitle = element_text(size=24, hjust=0.5, color = "#232D4B"),
    axis.title.x = element_text(color = "#232D4B", size = 22),
    axis.title.y = element_text(color = "#232D4B", size = 22)
  ) + geom_text(data = filter(data_virginia2, value > 0), aes(x = county, y = value, label=round(value, 2)), vjust = -.5, size=12, color=c("#EB5F0C", rep("#232D4B", 7))) + 
  scale_x_discrete(labels=c("Caroline County" = "Caroline", "Essex County" = "Essex", "Hanover County" = "Hanover", "King and Queen County" = "King and Queen", "King William County" = "King William", "King George County" = "King George", "Spotsylvania County" = "Spotsylvania", "Stafford County" = "Stafford"))






##### Lollipop plot for Oklahoma ####
# read in counties of interest
# Caddo, Comanche, Cotton, Grady, Kiowa, Stephens, Tillman
data_oklahoma <- data[c(141, 149, 150, 159, 171, 202, 204),]
data_all_ok <- data[134:210,]
#total_ok_population <- sum(data_all_ok$population)
#data_oklahoma$incomesmall <- (data_oklahoma$median_income / 1000)
names(data_oklahoma)[2] <- "county"
names(data_oklahoma)[14] <- "embeddedness"
mean_ok <- mean(data_oklahoma$embeddedness)

data_oklahoma2 <- melt(data_oklahoma %>% select(county, embeddedness))

# converting percentages to number of people to convert back to % of all Oklahomans to use for dashed line
#incomesmall <- ((sum(data_all_ok$median_income * data_all_ok$population)) / total_ok_population) / 1000
# people_no_health_ins <- ((data_all_ok$pct_no_health_ins / 100) * data_all_ok$population)
# people_impoverished <- ((data_all_ok$pct_impoverished / 100) * data_all_ok$population)
# people_owner_occupied <- ((data_all_ok$pct_owner_occupied_homes / 100) * data_all_ok$population)
# people_unemployed <- ((data_all_ok$pct_unemployed / 100) * data_all_ok$population)
# people_severe_housing <- ((data_all_ok$severe_housing_problems / 100) * data_all_ok$population)

# converting back to percentages
# pct_no_health_ins <- ((sum(people_no_health_ins)) / total_ok_population) * 100
# pct_impoverished <- ((sum(people_impoverished)) / total_ok_population) * 100
# pct_owner_occupied_homes <- ((sum(people_owner_occupied)) / total_ok_population) * 100
# pct_unemployed <- ((sum(people_unemployed)) / total_ok_population) * 100
# severe_housing_problems <- ((sum(people_severe_housing)) / total_ok_population) * 100
# all_mean <- data.frame(variable = c("incomesmall", "pct_no_health_ins", "pct_impoverished", 
#                                     "pct_owner_occupied_homes", "pct_unemployed", "severe_housing_problems"),
#                        value = c(incomesmall, pct_no_health_ins, pct_impoverished,
#                                  pct_owner_occupied_homes, pct_unemployed, severe_housing_problems))

data_oklahoma2names<-c(
  "county"="County",
  "embeddedness"="Embeddedness score"
)


# Oklahoma lollipop plot
data_oklahoma2$county <- factor(data_oklahoma2$county, levels = c("Comanche County", "Caddo County", "Cotton County", "Grady County", "Kiowa County", "Stephens County", "Tillman County"))

# plot_oklahoma <- ggplot(data_oklahoma2, aes(x=county, y=value)) +
#   geom_segment(aes(x=county, xend=county, y=0, yend=value), color="#EB5F0C", lwd = 2) +
#   geom_point(color="#EB5F0C", size=5, alpha=0.6) +
#   theme_light() +
#   coord_flip() +
#   facet_wrap(~variable, scales="free", labeller = as_labeller(data_oklahoma2names), nrow=2, ncol=3) +
#   geom_hline(data=all_mean, aes(yintercept=value), colour="#232D4B", linetype="dotted", size = 1) +
#   labs(title = "Comparing Counties of Interest to all of Oklahoma",
#        subtitle = "(Dashed line indicates value for all counties in Oklahoma)") +
#   theme(
#     panel.grid.major.y = element_blank(),
#     panel.border = element_blank(),
#     axis.ticks.y = element_blank(),
#     plot.title = element_text(size = 25, hjust = 0.5),
#     plot.subtitle = element_text(size = 18, hjust = 0.5),
#     strip.text=element_text(size=10, face="bold"),
#     strip.background=element_rect(fill= "#232D4B"),
#     axis.title.x=element_blank(),
#     axis.title.y=element_blank(),
#     axis.text.x=element_text(size=15),
#     axis.text.y=element_text(size=15, colour = c("#EB5F0C", rep("#232D4B", 6)))
#   )
# plot_oklahoma


##### Bar chart with embeddedness values for Oklahoma#####
ggplot(data_oklahoma2, aes(x=county, y=value)) +
  geom_bar(fill = c("#EB5F0C", rep("#232D4B", 6)), stat="identity") +
  theme_light() +
  expand_limits(y = 1) +
  geom_hline(aes(yintercept=mean_ok), colour="#232D4B", linetype="dotted", size = 1) +
  geom_hline(aes(yintercept=0), colour="#232D4B", size = 0.1) +
  labs(title = "Embeddedness Scores for Counties of Interest in Oklahoma",
       subtitle = "(Dashed line indicates average for Oklahoma)") +
  labs(x = "County", y = "Embeddedness score") +
  theme(
    panel.grid = element_blank(),
    axis.ticks.y = element_blank(),
    axis.text.x=element_text(size = 22, angle=20, hjust=1, color = c("#EB5F0C", rep("#232D4B", 6))),
    plot.title = element_text(size = 28, hjust = 0.5, color = "#232D4B"),
    plot.subtitle = element_text(size=24, hjust=0.5, color = "#232D4B"),
    axis.title.x = element_text(color = "#232D4B", size = 22),
    axis.title.y = element_text(color = "#232D4B", size = 22)
  ) + geom_text(data = filter(data_oklahoma2, value > 0), aes(x = county, y = value, label=round(value, 2)), vjust = -.5, color = c("#232D4B", "#EB5F0C", rep("#232D4B", 5)), size = 12) +
  scale_x_discrete(labels=c("Caddo County" = "Caddo", "Comanche County" = "Comanche", "Cotton County" = "Cotton", "Grady County" = "Grady", "Kiowa County" = "Kiowa", "Stephens County" = "Stephens", "Tillman County" = "Tillman"))


