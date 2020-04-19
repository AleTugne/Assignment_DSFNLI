# --------------------------- 0. Setup --------------------------------------
#installing glmulti package 
install.packages("glmulti")

# Importing libraries
library(glmulti)
library(tidyverse)
library(rgdal)
library(mapview)
library(sf)
library(tmap)
library(gridExtra)
library(rgeos)
library(mapview)
library(leaflet)
library(mgcv)
library(rgdal)
library(rstudioapi)
library(gridExtra)
library(ggplot2)
library(dplyr)

# Setting the directory
dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

#Setting graphs' colour
KULbg <- "#116E8A"

#---------------------------- 1. Data Exploration ------------------------------

# Importing the two DataBase
DB1 <- read.csv("./DB1.csv")
DB1 <- as_tibble(DB1)
DB2 <- read.csv("./inspost.csv")
DB2 <- as_tibble(DB2)

# Merging the two DB by Postal Code
DB <- left_join(DB1, DB2, by = c("CODPOSS" = "CODPOSS"))

# Re-arranging the Columns to reflect: infos about the city, ph, car, policy
col_order <- c("CODPOSS", "COMMUNE", "LAT", "LONG", "?..INS", "AGEPH", "sexp", 
               "agecar", "fuelc", "split", "usec", "duree", "lnexpo", "nbrtotc",
               "nbrtotan", "chargtot")
DB <- DB[, col_order]

# Rename the columns in lowercase and spaces with underscores
DB <- DB %>% rename_all(function(.name) {
  .name %>% tolower 
})
DB <- rename(DB, expo = duree)

DB %>% slice(1:3) 

# Some analysis
mean(DB$nbrtotc)

(mean <- sum(DB$nbrtotc)/sum(DB$expo))
(variance <- sum((DB$nbrtotc - mean * DB$expo)^2)/sum(DB$expo))

DB %>% summarize(emp_freq = sum(nbrtotc) / sum(expo)) 
freq_by_sex <- DB %>% group_by(sexp) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))
freq_by_age <- DB %>% group_by(ageph) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))

# Some graphs
g1 <- ggplot(freq_by_age, aes(x = ageph, y = emp_freq)) + 
  geom_density2d(stat = "identity", color = KULbg) + 
  geom_area(fill = KULbg) + 
  labs(title = "MTPL - empirical claim freq per age policyholder", x = "Policyholder's age", 
       y = "Emprirical claim frequency") +
  theme()
g1

g2 <- ggplot(freq_by_sex, aes(x = sexp, y = emp_freq, width = 0.4)) +
  geom_bar(stat = "identity", position = "identity", color = KULbg, 
           fill = KULbg) + 
  labs(title = "MTPL - empirical claim freq per sex policyholder", x = "Policyholder's sex", 
       y = "Emprirical claim frequency") + theme(panel.grid.major.x = element_blank())
g2

# Graph types
ggplot.bar <- function(DT, variable, xlab) {
  ggplot(data = DT, aes(as.factor(variable))) + theme_bw() + 
    geom_bar(aes(y = (..count..)/sum(..count..)), col = KULbg, fill = KULbg) + 
    labs(x = xlab, y = "Relative frequency")
}

ggplot.hist <- function(DT, variable, xlab, binwidth){
  ggplot(data = DT, aes(variable)) + theme_bw() + 
    geom_histogram(aes(y = (..count..)/sum(..count..)), col = KULbg, fill = KULbg) +
    labs(x = xlab, y = "Relative frequency")
}

# Frequency of claim 
plot.eda.nclaims <- ggplot.bar(DB, variable = DB$nbrtotc, xlab = "Number of claims")
# Exposure
plot.eda.exp <- ggplot.hist(DB, DB$expo, "Exposure", 0.05)
# Severity
plot.eda.amount <- DB %>% filter(chargtot > 0 & nbrtotan <= 81000) %>% 
  ggplot(aes(chargtot)) + geom_density(adjust = 3, col = KULbg, fill = KULbg) + 
  xlim(0, 1e4) + labs(x = "Severity", y = "Relative frequency") + theme_bw()

(g3 <- grid.arrange(plot.eda.nclaims, plot.eda.exp, plot.eda.amount))


#----------
plot.eda.fuel <- ggplot.bar(DB, DB$fuelc, "Type of fuel")
plot.eda.sex <- ggplot.bar(DB, DB$sexp, "Policyholder's sex")
plot.eda.use <- ggplot.bar(DB, DB$usec, "Type of use")
DB$split <- factor(DB$split, ordered = TRUE, levels = c("Once", "Twice", "Thrice", "Monthly"))
plot.eda.split <- ggplot.bar(DB, DB$split, "split")
DB$agecar <- factor(DB$agecar, ordered = TRUE, levels = c("0-1", "2-5", "6-10", ">10"))
plot.eda.agecar <- ggplot.bar(DB, DB$agecar, "Age of the car")
(plot.eda.ageph <- ggplot.hist(DB, DB$ageph, "Policyholder's age", 1))

#-----------

#Types of pie chart we will use
pie.chart.2.levels <- function(variable, name_1, name_2, chart_title) {
  attach(DB)
  slices <- by(DB, variable, count);  pct <- c(NA, NA)
  lbls <- c(name_1, name_2)
  pct[1] <- round(slices[[1]]/(slices[[1]]+slices[[2]])*100)
  pct[2] <- round(slices[[2]]/(slices[[1]]+slices[[2]])*100)
  lbls <- paste(lbls, pct, "%", sep = " ")
  pie(table(variable), labels = lbls, radius = 1, col = c("red1", KULbg), main = chart_title)
}

pie.chart.4.levels <- function(variable, name_1, name_2, name_3, name_4, chart_title) {
  attach(DB)
  slices <- by(DB, variable, count);  pct <- c(NA, NA, NA, NA)
  lbls <- c(name_1, name_2, name_3, name_4)
  pct[1] <- round(slices[[1]]/(slices[[1]]+slices[[2]]+slices[[3]]+slices[[4]])*100)
  pct[2] <- round(slices[[2]]/(slices[[1]]+slices[[2]]+slices[[3]]+slices[[4]])*100)
  pct[3] <- round(slices[[3]]/(slices[[1]]+slices[[2]]+slices[[3]]+slices[[4]])*100)
  pct[4] <- round(slices[[4]]/(slices[[1]]+slices[[2]]+slices[[3]]+slices[[4]])*100)
  lbls <- paste(lbls, pct, "%", sep = " ")
  pie(table(variable), labels = lbls, radius = 1, col = c(KULbg, "light blue", "white", "red1"), main = chart_title)
}

#STESSI GRAFICI DI PRIMA MA IN PIE CHARTS
plot.eda.fuel <- pie.chart.2.levels(fuelc, "Gasoil", "Petrol", "Type of fuel")
plot.eda.sex <- pie.chart.2.levels(sexp, "Female", "Male", "Policyholder's sex")
plot.eda.use <- pie.chart.2.levels(usec, "Private", "Professional", "Type of use")
DB$split <- factor(DB$split, ordered = TRUE, levels = c("Once", "Twice", "Thrice", "Monthly"))
plot.eda.split <- pie.chart.4.levels(split, "Once", "Twice", "Thrice", "Monthly", "Split")
DB$agecar <- factor(DB$agecar, ordered = TRUE, levels = c("0-1", "2-5", "6-10", ">10"))
plot.eda.agecar <- pie.chart.4.levels(agecar, "0-1 year;", "2-5 years;", "6-10 years;", "> 10 years;", "Age of the car")

#grafico pol's AGE: SECONDO TIPO: Histogram con Kernel (al posto di usare geom_hist ho usato _density, rimane più pulito e
# caotico rispetto a come era prima; 2° me)

(plot.eda.ageph <- ggplot(DB, aes(ageph)) + 
  geom_density(aes(y = (..count..)/sum(..count..)), bw = 0.6, color = KULbg, fill = KULbg) + 
  labs(title = "Proportions of Policyholders' age ", x = "Policyholder's age", 
       y = "Proportions") +
  theme())

#grafico pol's AGE: TERZO TIPO: Waffle chart

install.packages("waffle")
library(waffle)

age_counts <- DB %>% group_by(ageph) %>% filter(ageph >= 22 & ageph < 78) %>% summarise(age_levels = n()) %>%
  mutate(percent = round(age_levels/sum(age_levels)*100))
ph_age_counts <- age_counts$percent
names(ph_age_counts) <- age_counts$ageph
waffle(ph_age_counts, rows = 5, size = 0.5, colors = palette(rainbow(57)),
       title = "Proportions of Policyholders' age", xlab = "1 square = 1% of the popolation")

rm(years_group)
years_group <- DB %>% group_by(ageph) %>% filter(ageph >= 22 & ageph < 78) %>% summarise(obs_per_year = n())
years_group

years_group$ageph[years_group$ageph<26] = "<25"
years_group$ageph[years_group$ageph>25 & years_group$ageph<=30] = "26-30"
years_group$ageph[years_group$ageph>30 & years_group$ageph<=35] = "31-35"
years_group$ageph[years_group$ageph>35 & years_group$ageph<=40] = "36-40"
years_group$ageph[years_group$ageph>40 & years_group$ageph<=45] = "41-45"
years_group$ageph[years_group$ageph>45 & years_group$ageph<=50] = "46-50"
years_group$ageph[years_group$ageph>50 & years_group$ageph<=55] = "51-55"
years_group$ageph[years_group$ageph>55 & years_group$ageph<=60] = "56-60"
years_group$ageph[years_group$ageph>60 & years_group$ageph<=65] = "61-65"
years_group$ageph[years_group$ageph>65 & years_group$ageph<=70] = "66-70"
years_group$ageph[years_group$ageph>70 & years_group$ageph<=75] = "71-75"
years_group$ageph[years_group$ageph>=76] = ">76"

years_group$ageph <- ordered(years_group$ageph, 
                         levels=c("<25", "26-30","31-35","36-40","41-45","46-50", 
                                  "51-55", "56-60", "61-65", "66-70", "71-75", ">76"))
years_group <- years_group %>% group_by(ageph) %>% summarise(obs_per_year = sum(obs_per_year))

age_counts <- years_group %>%
  mutate(percent = round(obs_per_year/sum(obs_per_year)*200))
ph_age_counts <- age_counts$percent
names(ph_age_counts) <- age_counts$ageph
waffle(ph_age_counts, rows = 12, size = 1, colors = palette(rainbow(10)),
       title = "Proportions of Policyholders' age", xlab = "1 square = 1% of the popolation")

# IN GRUPPI DI 4 ANNI

# years_group$ageph[years_group$ageph<26] = "<25"
# years_group$ageph[years_group$ageph>25 & years_group$ageph<=28] = "25-28"
# years_group$ageph[years_group$ageph>28 & years_group$ageph<=32] = "29-32"
# years_group$ageph[years_group$ageph>32 & years_group$ageph<=36] = "33-36"
# years_group$ageph[years_group$ageph>36 & years_group$ageph<=40] = "37-40"
# years_group$ageph[years_group$ageph>40 & years_group$ageph<=44] = "41-44"
# years_group$ageph[years_group$ageph>44 & years_group$ageph<=48] = "45-48"
# years_group$ageph[years_group$ageph>48 & years_group$ageph<=52] = "49-52"
# years_group$ageph[years_group$ageph>52 & years_group$ageph<=56] = "53-56"
# years_group$ageph[years_group$ageph>56 & years_group$ageph<=60] = "57-60"
# years_group$ageph[years_group$ageph>60 & years_group$ageph<=64] = "61-64"
# years_group$ageph[years_group$ageph>64 & years_group$ageph<=68] = "65-68"
# years_group$ageph[years_group$ageph>68 & years_group$ageph<=72] = "69-72"
# years_group$ageph[years_group$ageph>72 & years_group$ageph<=76] = "73-76"
# years_group$ageph[years_group$ageph>76] = ">76"
# 
# years_group$ageph <- ordered(years_group$ageph, 
#                              levels=c("<25", "25-28","29-32","33-36","37-40","41-44", "45-48", "49-52",
#                                       "53-56", "57-60", "61-64", "65-68", "69-72", "73-76", ">76"))
# years_group <- years_group %>% group_by(ageph) %>% summarise(obs_per_year = sum(obs_per_year))
# 
# age_counts <- years_group %>%
#   mutate(percent = round(obs_per_year/sum(obs_per_year)*100))
# ph_age_counts <- age_counts$percent
# names(ph_age_counts) <- age_counts$ageph
# waffle(ph_age_counts, rows = 5, size = 1, colors = palette(rainbow(15)),
#        title = "Proportions of Policyholders' age", xlab = "1 square = 1% of the popolation")
#----------

#IN GRUPPI DI 3 ANNI
# rm(years_group)
# years_group <- DB %>% group_by(ageph) %>% filter(ageph >= 22 & ageph < 78) %>% summarise(obs_per_year = n())
# years_group
# 
# years_group$ageph[years_group$ageph<=23] = "<24"
# years_group$ageph[years_group$ageph>23 & years_group$ageph<=26] = "24-26"
# years_group$ageph[years_group$ageph>26 & years_group$ageph<=29] = "27-29"
# years_group$ageph[years_group$ageph>29 & years_group$ageph<=32] = "30-32"
# years_group$ageph[years_group$ageph>32 & years_group$ageph<=35] = "33-35"
# years_group$ageph[years_group$ageph>35 & years_group$ageph<=38] = "36-38"
# years_group$ageph[years_group$ageph>38 & years_group$ageph<=41] = "39-41"
# years_group$ageph[years_group$ageph>41 & years_group$ageph<=44] = "42-44"
# years_group$ageph[years_group$ageph>44 & years_group$ageph<=47] = "45-47"
# years_group$ageph[years_group$ageph>47 & years_group$ageph<=50] = "48-50"
# years_group$ageph[years_group$ageph>50 & years_group$ageph<=53] = "51-53"
# years_group$ageph[years_group$ageph>53 & years_group$ageph<=56] = "54-56"
# years_group$ageph[years_group$ageph>56 & years_group$ageph<=59] = "57-59"
# years_group$ageph[years_group$ageph>59 & years_group$ageph<=62] = "60-62"
# years_group$ageph[years_group$ageph>62 & years_group$ageph<=65] = "63-65"
# years_group$ageph[years_group$ageph>65 & years_group$ageph<=68] = "66-68"
# years_group$ageph[years_group$ageph>68 & years_group$ageph<=71] = "69-71"
# years_group$ageph[years_group$ageph>71 & years_group$ageph<=74] = "72-74"
# years_group$ageph[years_group$ageph>74] = ">74"
# 
# years_group$ageph <- ordered(years_group$ageph, 
#                              levels=c("<24", "24-26","27-29","30-32","33-35","36-38", "39-41", "42-44",
#                                       "45-47", "48-50", "51-53", "54-56", "57-59", "60-62", "63-65", 
#                                       "66-68", "69-71", "72-74", ">74"))
# 
# years_group <- years_group %>% group_by(ageph) %>% summarise(obs_per_year = sum(obs_per_year))

# age_counts <- years_group %>%
#   mutate(percent = round(obs_per_year/sum(obs_per_year)*200))
# ph_age_counts <- age_counts$percent
# names(ph_age_counts) <- age_counts$ageph
# waffle(ph_age_counts, rows = 8, size = 1.1, colors = palette(rainbow(18)), legend_pos = "bottom",
#        title = "Proportions of Policyholders' age", xlab = "1 square = 0.5% of the popolation")

#  --
<#NON POSSO USARLO COI PIE CHARTS 
# g4 <- grid.arrange(plot.eda.fuel, plot.eda.sex, plot.eda.use, plot.eda.split, 
#                     plot.eda.ageph, plot.eda.agecar)

#---------------------------- 2. Spatial Data ------------------------------

readShapefile = function(){
  belgium_shape <- readOGR(dsn = path.expand("./shape file Belgie postcodes"), 
                           layer = "npc96_region_Project1")
  belgium_shape <- spTransform(belgium_shape, CRS("+proj=longlat +datum=WGS84"))
  belgium_shape$id <- row.names(belgium_shape)
  return(belgium_shape)
}
belgium_shape = readShapefile()
plot.eda.map = ggplot(belgium_shape, aes(long, lat, group = group)) + 
  geom_polygon(fill = NA, colour = "black", size = 0.1) + 
  theme_bw()
plot.eda.map
mapview(belgium_shape)
fortify(belgium_shape)%>% slice(1:3)

post_expo = DB %>% group_by(codposs) %>% summarize(num = n(), total_expo = sum(expo)) 
post_expo %>% slice(1:5)

belgium_shape@data = left_join(belgium_shape@data, post_expo, by = c("POSTCODE" = "codposs"))
belgium_shape@data %>% slice(1:3) 

belgium_shape@data$freq = belgium_shape@data$total_expo/belgium_shape@data$Shape_Area
belgium_shape@data$freq_class = cut(belgium_shape@data$freq, breaks = quantile(belgium_shape@data$freq, c(0,0.2,0.8,1), na.rm = TRUE), right = FALSE, include.lowest = TRUE, labels = c("low","average","high"))
belgium_shape@data %>% slice(1:3) 

belgium_shape_f = fortify(belgium_shape)
belgium_shape_f = left_join(belgium_shape_f, belgium_shape@data)

plot.eda.map = ggplot(belgium_shape_f, aes(long, lat, group = group)) + 
  geom_polygon(aes(fill = belgium_shape_f$freq_class), colour = "black", size = 0.1)
plot.eda.map = plot.eda.map + theme_bw() + labs(fill = "Relative\nfrequency") + 
  scale_fill_brewer(palette = "Blues", na.value = "white")
plot.eda.map

#---------------------------- 3. GLM ------------------------------
# plotting the frequency and severity
plot.eda.nclaims
plot.eda.amount

### GLM model selection (without commune and expo)
### freq_GLM = glmulti(nbrtotc ~ lat+long+ageph+agecar+usec+sexp+fuelc+split+offset(lnexpo), family = poisson(link = "log"), confsetsize = 200, crit = bic, data = DB, intercept=TRUE, level=1, plotty=TRUE, report=TRUE, method = "g", deltaB = 0.5, deltaM = 0.5, conseq=7)
### After 550 generations:
### Best model: nbrtotc~agecar+fuelc+split+lat+ageph
### Crit= 127019.247424733
### Mean crit= 127387.616469234
### Improvements in best and average IC have bebingo en below the specified goals.
### Algorithm is declared to have converged.
### Completed.
### summary(freq_GLM)
### plot(freq_GLM, type = "r")

# Resulting model + representation of fitted values for each claim class
F_GLM=glm(nbrtotc~agecar+fuelc+split+lat+ageph+offset(lnexpo), data=DB, fam = poisson(link = log))
summary(F_GLM)
box1 = ggplot(DB, aes(F_GLM$fitted.values, group=nbrtotc)) + 
        geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=0.5) + 
        xlab("Claims") + ylab("Fitted Values")+coord_flip()

box1

# Analysis of deviance
anova(F_GLM, test="Chisq")

#diostronzo
##