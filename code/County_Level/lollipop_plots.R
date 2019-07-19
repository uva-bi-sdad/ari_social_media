#2019-07-16
#lollipop plotting

install.packages("reshape2")
library(tidyverse)
library(ggplot2)
library(dplyr)
library(viridis)
library(reshape2)

setwd("~/ari_social_media/data/working/County_Level")
data <- read_csv("county_level_indicators.csv")


#read in counties of interest
#Caroline, Essex, Hanover, King and Queen, King George, King William, Spotsylvania, Stafford
data_virginia <- data[c(17, 28, 42, 48, 49, 50, 85, 86),]
data_virginia2 <- melt(data_virginia %>% select(county, median_income, pct_no_health_ins, pct_impoverished,
                                                pct_renter_occupied_homes, pct_unemployed, severe_housing_problems))

data_all_va <- data[1:133,]
#calculate means for all va for variables we're interested in, then append them to data_virginia2


data_virginia2names<-c(
  "county"="County",
  "median_income"="Median Income",
  "pct_no_health_ins"="No Health Insurance",
  "pct_impoverished"="Impoverished",
  "pct_renter_occupied_homes"="Renter Occupied Homes",
  "pct_unemployed"="Unemployed",
  "severe_housing_problems"="Severe Housing Problems"
)

# code to make 3x2 panel lollipop plots using facet_wrap
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p_virginia <- ggplot(data_virginia2, aes(x=county, y=value)) +
  geom_point(shape=21, colour="black", fill=cbPalette[7], size=4) +
  geom_segment(aes(x=county, xend=county, y=value, yend=value), color=cbPalette[1], lwd=2) +
  coord_flip() +
  theme_bw() +
  facet_wrap(~variable, scales="free", labeller = as_labeller(data_virginia2names), nrow=2, ncol=3) +
  geom_point(shape=21, colour="black", fill=cbPalette[7], size=4) +
  theme(strip.text=element_text(size=16, face="bold"),
        strip.background=element_rect(fill=cbPalette[5]),
        axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15)) +
  xlab("") + 
  ylab("")+
  theme(axis.text.y = element_text(colour = c("red",rep("blue",7))))
p_virginia
