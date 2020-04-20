# --------------------------- 0. Setup --------------------------------------

# Importing libraries
library(pscl)
library(glmulti)
library(rgdal)
library(caret)
# The previous packages have to be installed
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
dir = dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(dir)

#---------------------------- 1. Data Exploration ------------------------------

# Importing the two DataBase
DB1 = read.csv("./DB1.csv")
DB1 = as_tibble(DB1)
DB2 = read.csv("./inspost.csv")
DB2 = as_tibble(DB2)
# colnames(DB2)[1] <- gsub('^...','',colnames(DB2)[1]) # riga da aggiungere se in DB2 colonna 'INS' = 'i..INS'

# Merging the two DB by Postal Code
DB = left_join(DB1, DB2, by = c("CODPOSS" = "CODPOSS"))

# Re-arranging the Columns to reflect: infos about the city, ph, car, policy
col_order <- c("CODPOSS", "COMMUNE", "LAT", "LONG", "INS", "AGEPH", "sexp", 
               "agecar", "fuelc", "split", "usec", "duree", "lnexpo", "nbrtotc",
               "nbrtotan", "chargtot")
DB = DB[, col_order]

# Rename the columns in lowercase and spaces with underscores
DB = DB %>% rename_all(function(.name) {
  .name %>% tolower 
})
DB = rename(DB, expo = duree)
DB %>% slice(1:3) 

# First some univariate analysis: see how each variable is distributed
col = "red"
fill = "orange"
ylab = "Relative frequency"
ggplot.bar = function(DT, variable, xlab){
  ggplot(data = DT, aes(as.factor(variable))) + theme_bw() + 
    geom_bar(aes(y = (..count..)/sum(..count..)), col = col, fill = fill, alpha = 0.5) + labs(x = xlab, y = "%")
}
ggplot.hist = function(DT, variable, xlab, binwidth){
  ggplot(data = DT, aes(variable)) + theme_bw() + 
    geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = binwidth, col = col, fill = fill, alpha = 0.5) + 
    labs(x = xlab, y = ylab)
}

plot.eda.fuel = ggplot.bar(DB, DB$fuelc, "fuel") + xlab("Fuel")
plot.eda.sex = ggplot.bar(DB, DB$sexp, "sex") + xlab("P/h sex")
plot.eda.use = ggplot.bar(DB, DB$usec, "use") + xlab("Type of use")
DB$split = factor(DB$split, ordered = TRUE, levels = c("Once", "Twice", "Thrice", "Monthly"))
plot.eda.split = ggplot.bar(DB, DB$split, "split") + xlab("Payment split")
DB$agecar = factor(DB$agecar, ordered = TRUE, levels = c("0-1", "2-5", "6-10", ">10"))
plot.eda.agecar = ggplot.bar(DB, DB$agecar, "agec") + xlab("Age of the car")
plot.eda.ageph = ggplot.hist(DB, DB$ageph, "ageph", 1) + scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + xlab("P/h age")

g1 = grid.arrange(plot.eda.fuel, plot.eda.sex, plot.eda.use, plot.eda.split, plot.eda.ageph, plot.eda.agecar)
g1

#Frequency --> it will be modelled in section 3.1 - 3.2
plot.eda.nclaims = ggplot.bar(DB, variable = DB$nbrtotc, "nclaims") + xlab("Number of claims") + ylab("%")
#Exposure
plot.eda.exp = ggplot.hist(DB, DB$expo, "expo", 0.05) + xlab("Exposure to risk") + ylab("%")
#Severity --> it wil be modelled in section 4.1 - 4.2
DB.sev = DB %>% filter(chargtot > 0 & nbrtotan <= 81000)
plot.eda.sev = ggplot(data = DB.sev, aes(chargtot)) + 
  geom_density(adjust = 3, col = col, fill = fill, alpha = 0.5) + 
  xlim(0, 1e4) + ylab("%") + xlab("Total claim amount") + theme_bw()

g2 = grid.arrange(plot.eda.nclaims, plot.eda.exp, plot.eda.sev)
g2

# Some analysis
# Let us count the proportion of 0 in chargtot, lnexpo and nbrtotc
100*sum(DB$chargtot == 0)/nrow(DB)  #88%
g1=ggplot(DB, aes(chargtot)) + geom_histogram(bins=100) + scale_y_log10()
g1
100*sum(DB$lnexpo == 0)/nrow(DB)  #77%
100*sum(DB$nbrtotc == 0)/nrow(DB)  #88%

# From here we can see that the number of 0 is important, so we have to think about switching from a 
# Poisson GLM to a Zero-Inflated Poisson GLM

mean(DB$nbrtotc)
mean = sum(DB$nbrtotc)/sum(DB$expo)
mean
variance = sum((DB$nbrtotc - mean * DB$expo)^2)/sum(DB$expo)
variance

DB %>% summarize(emp_freq = sum(nbrtotc) / sum(expo)) 
freq_by_sex = DB %>% group_by(sexp) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))
freq_by_age = DB %>% group_by(ageph) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))

# Some graphs
g3 = ggplot(freq_by_age, aes(x = ageph, y = emp_freq)) + theme_bw() + 
  geom_bar(stat = "identity", color = "red",
           fill = "orange", alpha = .5) + 
  ggtitle("Empirical claim freq per age policyholder")
g3

g4 = ggplot(freq_by_sex, aes(x = sexp, y = emp_freq)) + theme_bw() +
  geom_bar(stat = "identity", color = "red", 
           fill = "orange", alpha = .5) + 
  ggtitle("Empirical claim freq per sex policyholder")
g4

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

#---------------------------- 3. Frequency Modelling ------------------------------
#---------------------------- 3.1 GLM ------------------------------
# plotting the frequency and severity
plot.eda.nclaims
plot.eda.amount

### Now, given the high proportion of 0, we will fit the best Cross-Validated 
### (using glmulti and Validation approach with {caret}, splitting the data into training (80%) and test (20%) set) 
### Poisson GLM model

# Let's define the training and test data that will be used from now on
set.seed(123)
training.samples <- DB$nbrtotc %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- DB[training.samples, ]
test.data <- DB[-training.samples, ]

g5 <- ggplot(train.data, aes(x = nbrtotc)) + theme_bw() + geom_density(trim = TRUE) +
  geom_density(data = test.data, trim = TRUE, col = "red") + 
  theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  ggtitle("Caret splitting") 
g5

## 1 - Classical Poisson Model (without commune, INS, codeposs and chargtot because the latter is deterministic wrt nbrtotc)
# C_GLM = glmulti(nbrtotc ~ lat+long+ageph+agecar+usec+sexp+fuelc+split+offset(lnexpo), family = poisson(link = "log"), confsetsize = 200, crit = bic, data = train.data, intercept=TRUE, level=1, plotty=TRUE, report=TRUE, method = "g", deltaB = 0.5, deltaM = 0.5, conseq=7)
# summary(Classical_GLM)
# plot(freq_GLM, type = "r")

# After 400 generations:
# Best model: nbrtotc~1+fuelc+split+lat+ageph
# Crit= 101441.988027927
# Mean crit= 101737.199031362
# Improvements in best and average IC have bebingo en below the specified goals.
# Algorithm is declared to have converged.
# Completed

C_Poi=glm(nbrtotc~1+fuelc+split+lat+ageph+offset(lnexpo), data=train.data, fam = poisson(link = log))
summary(C_Poi)
#plot(C_Poi)

# representation of fitted values (predictions) for each claim class
fitted_C_Poi = C_Poi %>% fitted(test.data)
box1 = ggplot(train.data, aes(group=nbrtotc, exp(fitted_C_Poi))) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=0.5) + 
  xlab("Claims") + ylab("Fitted Values")+coord_flip()

box1

# Let's calculate the Test MSE which will be used to compare the GLM with the Machine Learning Method
train_MSE_C_Poi = mean(C_Poi$residuals^2)
test_MSE_C_Poi = mean((test.data$nbrtotc - predict(C_Poi, test.data, type="response")) ^ 2) 
#actually Mean Squared Prediction Error

# Check for over/underdispersion in the model
E2 <- resid(C_Poi, type = "pearson")
N  <- nrow(train.data)
p  <- length(coef(C_Poi))   
sum(E2^2) / (N - p) 
# 1.196218 <--- Poor overdispersion: This means that there is extra variance 
# not accounted for by the model or by the error structure. Indeed the Poisson family has dispersion parameter = 1. 
mean(DB$nbrtotc)
var(DB$nbrtotc)
# The fact that the variance is greater than the mean in our dependent variable confirm the assumption of overd.
# We can actually try to reduce it by using mixed models as the Zero Inflated Model
# but actually we can conclude that the GLM is not so efficient to fit the data, so we will use machine learning
# techniques to improve the fit


#---------------------------- 3.2 Gradient Boosting ------------------------------