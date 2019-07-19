options("scipen"=100)

if (!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
BiocManager::install("BiocInstaller")

BiocManager::install("pcaMethods")

options(max.print=999999)

county_indicators <- rbind(ok_indicators, va_indicators)

indicators_2 <- county_indicators %>%
  select(-c(county, pct_renter_occupied_homes, 
            pct_moved_after_2014,  pct_moved_2010_2014, pct_moved_2000_2009, pct_moved_1990_1999, pct_moved_1980_1989, pct_moved_1979_earlier, 
            pct_movers_within_county, pct_health_ins, pct_college_or_higher, 
            lib_hours_per_person, lib_visit_rate, pct_lib_patrons, pct_lib_program_atten))

indicators_3 <- drop_na(indicators_2)

county_scale <- prcomp(indicators_3, scale=TRUE, center=TRUE)
View(county_scale)

biplot(county_scale)
par("mar")
par(mar=c(1,1,1,1))

# scree plot
pr.var <- county_scale$sdev^2
pve <- pr.var / sum(pr.var)
par(mfrow=c(1,2))
plot(pve, xlab = "Principal Component",
     ylab="Proportion of Variance Explained", ylim=c(0,1), type="b")
plot(cumsum(pve), xlab = "Principal Component",
     ylab="Cumulative Variance Explained", ylim=c(0,1), type="b")

# new model with top six principal components
new_model<-lm(pct_stayed~county_scale$x[,1]+county_scale$x[,2]+county_scale$x[,3]+
                         county_scale$x[,4]+county_scale$x[,5]+county_scale$x[,6],data=indicators_3)
summary(new_model)

##using PCA
library(FactoMineR)
library(pcaMethods)

indicators_3 <- scale(indicators_2)

pca(indicators_3)
pc <- pca(indicators_3, method="ppca")
imputed <- completeObs(pc)
pca(imputed, scale = c(colnames(indicators_3)))
PCA(imputed)
pca_whatever <- PCA(imputed)
pca_whatever$eig
pca_whatever$eig




