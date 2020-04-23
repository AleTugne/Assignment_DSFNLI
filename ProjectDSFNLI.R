# --------------------------- 0. Setup --------------------------------------

# Importing libraries
library(pscl)
library(glmulti)
library(rgdal)
library(caret)
library(classInt)
library(gbm)
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
               "agecar", "fuelc", "split", "usec", "fleetc", "sportc", "powerc", "coverp", "duree", "lnexpo", "nbrtotc",
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

DB$split = factor(DB$split, ordered = TRUE, levels = c("Once", "Twice", "Thrice", "Monthly"))
DB$agecar = factor(DB$agecar, ordered = TRUE, levels = c("0-1", "2-5", "6-10", ">10"))
DB$powerc = factor(DB$powerc, ordered = TRUE, levels = c("<66", "66-110", ">110"))

plot.eda.sex = ggplot.bar(DB, DB$sexp, "sex") + xlab("P/h sex")
plot.eda.ageph = ggplot.hist(DB, DB$ageph, "ageph", 1) + scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + xlab("P/h age")
plot.eda.fuel = ggplot.bar(DB, DB$fuelc, "fuel") + xlab("Fuel")
plot.eda.use = ggplot.bar(DB, DB$usec, "use") + xlab("Type of use")
plot.eda.split = ggplot.bar(DB, DB$split, "split") + xlab("Payment split")
plot.eda.agecar = ggplot.bar(DB, DB$agecar, "agec") + xlab("Age of the car")
plot.eda.fleet = ggplot.bar(DB, DB$fleetc, "fleet") + xlab("Fleet")
plot.eda.sport = ggplot.bar(DB, DB$sportc, "sport") + xlab("Sport Car")
plot.eda.cover = ggplot.bar(DB, DB$coverp, "cover") + xlab("Type of Coverage")
plot.eda.power = ggplot.bar(DB, DB$powerc, "power") + xlab("Power of the car")

g1 = grid.arrange(plot.eda.sex, plot.eda.ageph, plot.eda.fuel, plot.eda.use, plot.eda.split, plot.eda.agecar, plot.eda.fleet, plot.eda.sport, plot.eda.cover, plot.eda.power)
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

#---------------------------- 2. Spatial Data ------------------------------
belgium_shape_sf <- st_read('./shape file Belgie postcodes/npc96_region_Project1.shp', quiet = TRUE)
belgium_shape_sf <- st_transform(belgium_shape_sf, CRS("+proj=longlat +datum=WGS84"))
belgium_shape_sf %>% as_tibble() %>% slice(1:3) 

# Now we will plot the relative exposure per area unit
post_expo = DB %>% group_by(codposs) %>% summarize(num = n(), total_expo = sum(expo)) 
post_expo %>% slice(1:5)

belgium_shape_sf <- left_join(belgium_shape_sf, post_expo, by = c("POSTCODE" = "codposs"))
belgium_shape_sf$freq <- belgium_shape_sf$total_expo/belgium_shape_sf$Shape_Area

belgium_shape_sf$freq_class <- cut(belgium_shape_sf$freq, breaks = quantile(belgium_shape_sf$freq, c(0,0.2,0.8,1), na.rm = TRUE),
                                  right = FALSE, include.lowest = TRUE, labels = c("low", "average", "high"))
ggplot(belgium_shape_sf) +
  geom_sf(aes(fill = freq_class), colour = "black", size = 0.1) +
  ggtitle("DB claim frequency data") + labs(fill = "Relative\nexposure") +
  scale_fill_brewer(palette = "Blues", na.value = "white") + 
  theme_bw()

belgium_shape_sf <- st_simplify(belgium_shape_sf, dTolerance = 0.00001)
tm_shape(belgium_shape_sf) + tm_borders(col = "black") + 
        tm_fill(col = "freq_class", style = "cont", palette = "Blues", colorNA = "white")
tmap_leaflet(tmap_last())

#---------------------------- 3. Frequency Modelling ------------------------------
#---------------------------- 3.1 GLM ------------------------------
# plotting the frequency and severity
plot.eda.nclaims
plot.eda.sev

### Now, given the high proportion of 0, we will fit the best Cross-Validated 
### (using glmulti and Validation approach with {caret}, splitting the data into training (80%) and test (20%) set) 
### Poisson GLM model

# Let's define the training and test data that will be used from now on
set.seed(123)
training.samples <- DB$nbrtotc %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- DB[training.samples, ]
test.data <- DB[-training.samples, ]

g3 <- ggplot(train.data, aes(x = nbrtotc)) + theme_bw() + geom_density(trim = TRUE) +
  geom_density(data = test.data, trim = TRUE, col = "red") + 
  theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
  ggtitle("Caret splitting") 
g3

## 1 - Classical Poisson Model (without commune, INS, codeposs and chargtot because the latter is deterministic wrt nbrtotc)
# C_GLM = glmulti(nbrtotc ~ lat+long+ageph+agecar+usec+sexp+fuelc+split+fleetc+sportc+powerc+coverp+offset(lnexpo), family = poisson(link = "log"), confsetsize = 200, crit = bic, data = train.data, intercept=TRUE, level=1, plotty=TRUE, report=TRUE, method = "g", deltaB = 0.5, deltaM = 0.5, conseq=7)
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

# Graphs representing the various covariates used wrt number of claim frequency

DB %>% summarize(emp_freq = sum(nbrtotc) / sum(expo)) 
freq_by_fuel = DB %>% group_by(fuelc) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))
freq_by_split = DB %>% group_by(split) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))
freq_by_lat = DB %>% group_by(lat) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))
freq_by_ageph = DB %>% group_by(ageph) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))

g4 = ggplot(freq_by_fuel, aes(x = fuelc, y = emp_freq)) + theme_bw() + 
  geom_bar(stat = "identity", color = "red",
           fill = "orange", alpha = .5) + 
  ggtitle("Empirical claim freq per fuel of the car")
g4

g5 = ggplot(freq_by_split, aes(x = split, y = emp_freq)) + theme_bw() +
  geom_bar(stat = "identity", color = "red", 
           fill = "orange", alpha = .5) + 
  ggtitle("Empirical claim freq per payment split")
g5

g6 = ggplot(freq_by_lat, aes(x = lat, y = emp_freq)) + theme_bw() +
  geom_bar(stat = "identity", color = "red", 
           fill = "orange", alpha = .5) + 
  ggtitle("Empirical claim freq per latitude")
g6

g7 = ggplot(freq_by_ageph, aes(x = ageph, y = emp_freq)) + theme_bw() +
  geom_bar(stat = "identity", color = "red", 
           fill = "orange", alpha = .5) + 
  ggtitle("Empirical claim freq per ageph")
g7

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

# Let's predict the annual expected claim frequency for some profiles extracted from test.data
pred_C_Poi=predict(C_Poi, newdata = test.data[23:25,], type = "response")

# Let's represent the GLM results by Spatial Data
post_dt <- st_centroid(belgium_shape_sf)
post_dt$long <- do.call(rbind, post_dt$geometry)[,1]
post_dt$lat <- do.call(rbind, post_dt$geometry)[,2]
post_dt$fuelc <- test.data$fuelc[1]
post_dt$split <- test.data$split[1]
post_dt$lnexpo <- test.data$lnexpo[1]
post_dt$ageph <- test.data$ageph[1]

pred <- predict(C_Poi, newdata = post_dt, type = "terms", terms = "lat")
dt_pred <- tibble(pc = post_dt$POSTCODE, long = post_dt$long, lat = post_dt$lat, pred)
names(dt_pred)[4] <- "fit_spatial" 

dt_pred <- dplyr::arrange(dt_pred, post_dt$POSTCODE)

num_bins <- 5
classint_fisher <- classIntervals(dt_pred$fit_spatial, num_bins, style = "fisher")
classint_fisher$brks
min(dt_pred$fit_spatial)
max(dt_pred$fit_spatial)

belgium_shape_sf <- left_join(belgium_shape_sf, dt_pred,  by = c("POSTCODE" = "pc"))
belgium_shape_sf$class_fisher <- cut(belgium_shape_sf$fit_spatial, breaks = classint_fisher$brks, right = FALSE, 
                                     include.lowest = TRUE, dig.lab = 2)

ggplot(belgium_shape_sf) + theme_bw() + labs(fill = "Fisher") +
  geom_sf(aes(fill = class_fisher), colour = NA) +
  ggtitle("DB claim frequency data") +
  scale_fill_brewer(palette = "Blues", na.value = "white") +
  theme_bw()

#---------------------------- 3.2 Gradient Boosting ------------------------------
tgrid <- expand.grid('depth' = c(1,3,5), 'ntrees' = NA, 'oob_err' = NA)

for(i in seq_len(nrow(tgrid))){
  set.seed(76539) # reproducibility
  # Fit a GBM
  GB_GLM <- gbm(nbrtotc ~ lat+long+ageph+agecar+usec+sexp+fuelc+split+offset(lnexpo),
           data = train.data, distribution = 'poisson', var.monotone = NULL,
           n.trees = 200, interaction.depth = tgrid$depth[i], n.minobsinnode = 1000, shrinkage = 0.1,
           bag.fraction = 0.75, cv.folds = 0)

  # Retrieve the optimal number of trees
  opt <- which.max(cumsum(GB_GLM$oobag.improve))
  tgrid$ntrees[i] <- opt
  tgrid$oob_err[i] <- sum(GB_GLM$oobag.improve[1:opt])
}

# Order results on the OOB error
tgrid %>% arrange(oob_err)

# Fit the optimal GBM
set.seed(123)
GB_GLM <- gbm(nbrtotc ~ lat+long+ageph+agecar+usec+sexp+fuelc+split+fleetc+sportc+powerc+coverp+offset(lnexpo),
              data = train.data, distribution = 'poisson', var.monotone = NULL,
              n.trees = tgrid$ntrees[1], interaction.depth = tgrid$depth[1], n.minobsinnode = 1000, shrinkage = 0.1,
              bag.fraction = 0.75, cv.folds = 0)

summary(GB_GLM)
print(GB_GLM)

# Partial Dependence Plot (PDP) for ageph
g8=plot(GB_GLM, i.var = 3, lwd = 2, col = "blue", main = "")
g8

# Let's check the correlation coef
cor(train.data$ageph,train.data$nbrtotc) #negative correlation coeff --> proven

# Partial Dependence Plot (PDP) for ageph:sexp
g9=plot(GB_GLM, i.var = c(3,6), lwd = 2, col = "blue", main = "")
g9

# Let's predict the annual expected claim frequency for some profiles extracted from test.data
pred_GB_GLM=predict(GB_GLM, newdata = test.data[25:27,], type = "response", n.trees = 111) + test.data$lnexpo[23:25]

# compute the test error as a function of number of trees
n.trees = seq(from=1 ,to=111, by=1) #no of trees-a vector of 111 values 
#Generating a Prediction matrix for each Tree
predmatrix<-predict(GB_GLM,test.data,n.trees = n.trees, type = "response")
dim(predmatrix) #dimentions of the Prediction Matrix

#Calculating The Mean Squared Test Error
test_MSE_GB_GLM<-with(test.data,apply((predmatrix-nbrtotc)^2,2,mean))
head(test_MSE_GB_GLM) #contains the Mean squared test error for each of the 100 trees averaged
Min_test_MSE_GB_GLM=min(test_MSE_GB_GLM)
#Plotting the test error vs number of trees
plot(n.trees , test_MSE_GB_GLM , pch=19,col="blue",xlab="Number of Trees",ylab="Test Error", main = "Perfomance of Boosting on Test Set")
abline(h = min(test_MSE_GB_GLM),col="red")
legend("topright",c("Min. MSTE"),col="red",lty=1,lwd=1)

#---------------------------- 4. Severity Modelling ------------------------------
#---------------------------- 4.1 GLM ------------------------------

# Claim severity is highly skewed (to the right), so to model it we can use the Gamma distribution
# This is used when the waiting times between two events in a process follow a Poisson Process
# This distribution has two parameters: 
# - k = number of occurrencies of an event
# - θ = 1/λ is the mean number of events per time unit (λ is the mean time between events)

# Let's plot the severity
plot.eda.sev
100*sum(DB$chargtot == 0)/nrow(DB)  #88%
# the main characteristic of the Gamma is that all the outcomes must be positive,
# Here we have that the 88% of the observations are 0, so we have to split the DB 
# into observations without loss and observations with losses
#










#---------------------------- 4.2 Gradient Boosting ------------------------------