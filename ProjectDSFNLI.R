# --------------------------- 0. Setup --------------------------------------

# Importing libraries
library(pscl)
library(glmnet)
library(rgdal)
library(caret)
library(classInt)
library(gbm)
library(plotmo)
library(car)
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

#Setting graphs' colour
KULbg <- "#116E8A"

#---------------------------- 1. Data Exploration ------------------------------

# Importing the two DataBase
DB1 <- read.csv("./DB1.csv")
DB1 <- as_tibble(DB1)
DB2 <- read.csv("./inspost.csv")
DB2 <- as_tibble(DB2)
# colnames(DB2)[1] <- gsub('^...','',colnames(DB2)[1]) # riga da aggiungere se in DB2 colonna 'INS' = 'i..INS'

# Merging the two DB by Postal Code
DB <- left_join(DB1, DB2, by = c("CODPOSS" = "CODPOSS"))

# Re-arranging the Columns to reflect: infos about the city, ph, car, policy
col_order <- c("CODPOSS", "COMMUNE", "LAT", "LONG", "INS", "AGEPH", "sexp", 
               "agecar", "fuelc", "split", "usec", "fleetc", "sportc", "powerc", "coverp", "duree", "lnexpo", "nbrtotc",
               "nbrtotan", "chargtot")
DB <- DB[, col_order]

# Rename the columns in lowercase and spaces with underscores
DB <- DB %>% rename_all(function(.name) {
  .name %>% tolower 
})
DB <- rename(DB, expo = duree)
DB %>% slice(1:3) 

# First some univariate analysis: see how each variable is distributed
# Proposta 1 - Barcharts
ylab <- "Relative frequency"
ggplot.bar <- function(DT, variable, xlab){
  ggplot(data = DT, aes(as.factor(variable))) + theme_bw() + 
    geom_bar(aes(y = (..count..)/sum(..count..)), col = KULbg, fill = KULbg, alpha = 0.5) + labs(x = xlab, y = "%")
}
ggplot.hist <- function(DT, variable, xlab, binwidth){
  ggplot(data = DT, aes(variable)) + theme_bw() + 
    geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = binwidth, col = KULbg, fill = KULbg, alpha = 0.5) + 
    labs(x = xlab, y = ylab)
}

DB$split <- factor(DB$split, ordered = TRUE, levels = c("Once", "Twice", "Thrice", "Monthly"))
DB$agecar <- factor(DB$agecar, ordered = TRUE, levels = c("0-1", "2-5", "6-10", ">10"))
DB$powerc <- factor(DB$powerc, ordered = TRUE, levels = c("<66", "66-110", ">110"))

plot.eda.sex <- ggplot.bar(DB, DB$sexp, "sex") + xlab("P/h sex")
plot.eda.ageph <- ggplot.hist(DB, DB$ageph, "ageph", 1) + scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + xlab("P/h age")
plot.eda.fuel <- ggplot.bar(DB, DB$fuelc, "fuel") + xlab("Fuel")
plot.eda.use <- ggplot.bar(DB, DB$usec, "use") + xlab("Type of use")
plot.eda.split <- ggplot.bar(DB, DB$split, "split") + xlab("Payment split")
plot.eda.agecar <- ggplot.bar(DB, DB$agecar, "agec") + xlab("Age of the car")
plot.eda.fleet <- ggplot.bar(DB, DB$fleetc, "fleet") + xlab("Fleet")
plot.eda.sport <- ggplot.bar(DB, DB$sportc, "sport") + xlab("Sport Car")
plot.eda.cover <- ggplot.bar(DB, DB$coverp, "cover") + xlab("Type of Coverage")
plot.eda.power <- ggplot.bar(DB, DB$powerc, "power") + xlab("Power of the car")
#Chargtot*ageph, left skewed distribution 
DB_chargtot <- DB %>% filter(chargtot > 0)
plot.eda.chargtot_ageph <- ggplot(DB_chargtot, aes(x=ageph))+geom_density(adjust = 5, col = KULbg, fill = KULbg, alpha = 0.5)+
xlim(5,95)+xlab("Chargtot per age")+scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

#Chargtot*carPower , I thought more powerful car could have higher severities.
plot.eda.chargtot_powcar <-ggplot(DB_chargtot,aes( x = powerc, y = chargtot))+geom_boxplot(col = KULbg, fill = KULbg, alpha = 0.5)+ylab("claim severity")
plot.eda.chargtot_powcar

g1 <- grid.arrange(plot.eda.sex, plot.eda.ageph, plot.eda.fuel, plot.eda.use,
                   plot.eda.split, plot.eda.agecar, plot.eda.fleet, plot.eda.sport, plot.eda.cover,
                   plot.eda.power,plot.eda.chargtot_ageph,plot.eda.chargtot_powcar)
g1

# Proposta 2 - Piecharts
pie.chart.2lvs <- function(DT, variable, title, values) {
  ggplot(DT, aes(x = "", fill = variable)) + 
    geom_bar(color = "white") + 
    coord_polar(theta = "y") +
    scale_fill_manual(values = values) +
    labs(title = title) + theme_void()
}
pie.chart.3lvs <- function(DT, variable, title) {
  ggplot(DT, aes(x = "", fill = variable)) + 
    geom_bar(color = "white") + 
    coord_polar(theta = "y") +
    scale_fill_manual(values = c(KULbg, "red2", "lightblue2")) +
    labs(title = title) + theme_void()
}
pie.chart.4lvs <- function(DT, variable, title) {
  ggplot(DT, aes(x = "", fill = variable)) + 
    geom_bar(color = "white") + 
    coord_polar(theta = "y") +
    scale_fill_manual(values = c(KULbg, "red2", "lightblue3", "lightblue2")) +
    labs(title = title) + theme_void()
}

plot.eda.sex2 <- pie.chart.2lvs(DB, DB$sexp, "P/H's gender", c("red2", KULbg))
plot.eda.fuel2 <- pie.chart.2lvs(DB, DB$fuelc, "Type of fuel", c("red2", KULbg))
plot.eda.use2 <- pie.chart.2lvs(DB, DB$usec, "Type of use", c(KULbg, "red1"))
plot.eda.split2 <- pie.chart.4lvs(DB, DB$split, "Payments' split")
plot.eda.agecar2 <- pie.chart.4lvs(DB, DB$agecar, "Age of the car")
plot.eda.fleet2 <- pie.chart.2lvs(DB, DB$fleetc, "Fleet", c(KULbg, "red1"))
plot.eda.sport2 <- pie.chart.2lvs(DB, DB$sportc, "Sport Car", c(KULbg, "red1"))
plot.eda.cover2 <- pie.chart.3lvs(DB, DB$coverp, "Type of Coverage")
plot.eda.power2 <- pie.chart.3lvs(DB, DB$powerc, "Power of the car")

g2 <- grid.arrange(plot.eda.sex2, plot.eda.ageph, plot.eda.fuel2, plot.eda.use2, plot.eda.split2, 
                   plot.eda.agecar2, plot.eda.fleet2, plot.eda.sport2, plot.eda.cover2, plot.eda.power2, 
                   ncol = 4)
g2

#Frequency --> it will be modelled in section 3.1 - 3.2
plot.eda.nclaims <- ggplot.bar(DB, variable = DB$nbrtotc, "nclaims") + xlab("Number of claims") + ylab("%")

mean(DB$nbrtotc)
mean <- sum(DB$nbrtotc)/sum(DB$expo)
mean
variance <- sum((DB$nbrtotc - mean * DB$expo)^2)/sum(DB$expo)
variance

#Exposure
plot.eda.exp <- ggplot.hist(DB, DB$expo, "expo", 0.05) + xlab("Exposure to risk") + ylab("%")
#Severity (in reality is the total amount charged) --> it wil be modelled in section 4.1 - 4.2
severity = DB %>% filter(chargtot > 0)
plot.eda.sev <- severity %>% 
  ggplot(aes(chargtot)) + 
  geom_density(adjust = 3, col = KULbg, fill = KULbg, alpha = 0.5) + 
  xlim(0, 1e4) + ylab("%") + xlab("Total claim amount") + theme_bw()

g3 <- grid.arrange(plot.eda.nclaims, plot.eda.exp, plot.eda.sev)
g3

#---------------------------- 2. Spatial Data ------------------------------
belgium_shape_sf <- st_read('./shape file Belgie postcodes/npc96_region_Project1.shp', quiet = TRUE)
belgium_shape_sf <- st_transform(belgium_shape_sf, CRS("+proj=longlat +datum=WGS84"))
belgium_shape_sf %>% as_tibble() %>% slice(1:3) 

# Now we will plot the relative exposure per area unit
post_expo <- DB %>% group_by(codposs) %>% summarize(num = n(), total_expo = sum(expo)) 
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


# Some further analysis
# Let us count the proportion of 0 in chargtot, lnexpo and nbrtotc
100*sum(DB$chargtot == 0)/nrow(DB)  #88%
g4 <- ggplot(DB, aes(chargtot)) + geom_histogram(bins=100) + scale_y_log10()
g4
100*sum(DB$lnexpo == 0)/nrow(DB)  #77%
100*sum(DB$nbrtotc == 0)/nrow(DB)  #88%

# From here we can see that the number of 0 is important, so we have to think about switching from a 
# Poisson GLM to a Zero-Inflated Poisson GLM


### Now, given the high proportion of 0, we will fit the best Cross-Validated 
### (using glmulti and Validation approach with {caret}, splitting the data into training (80%) and test (20%) set) 
### Poisson GLM model

# Let's define the training and test data that will be used from now on
set.seed(100)
training.samples <- DB$nbrtotc %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- DB[training.samples, ]
test.data <- DB[-training.samples, ]

g5 <- ggplot(train.data, aes(x = nbrtotc)) + theme_bw() + geom_density(trim = TRUE) +
  geom_density(data = test.data, trim = TRUE, col = "red") + 
  theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank(), legend.position="bottom") +
  ggtitle("Caret splitting")
g5

## 1 - Classical Poisson Model (without commune, INS, codeposs and chargtot because the latter is deterministic wrt nbrtotc)

# Now, we will use the LASSO regression for model selection: Prior to lasso, the most widely used method for choosing which covariates 
# to include was stepwise selection, which only improves prediction accuracy in certain cases, such as when only a few covariates have 
# a strong relationship with the outcome. However, in other cases, it can make prediction error worse. 
# Also, at the time, ridge regression was the most popular technique for improving prediction accuracy. 
# Ridge regression improves prediction error by shrinking large regression coefficients in order to reduce overfitting, 
# but it does not perform covariate selection and therefore does not help to make the model more interpretable.
# Lasso is able to achieve both of these goals by forcing the sum of the absolute value of the regression coefficients 
# to be less than a fixed value, which forces certain coefficients to be set to zero, effectively choosing a simpler model that does 
# not include those coefficients. This idea is similar to ridge regression, in which the sum of the squares of the coefficients is forced 
# to be less than a fixed value, though in the case of ridge regression, this only shrinks the size of the coefficients, it does not set any 
# of them to zero.

# To implement that reasoning, we will use the glmnet package (dropping commune, INS, codposs, expo, nbrtotan, chargtot) --> https://web.stanford.edu/~hastie/glmnet/glmnet_alpha.html 
xmatrix <- model.matrix(nbrtotc ~ agephagecar+usec+sexp+fuelc+split+fleetc+sportc+powerc+coverp+lat*long, data=train.data)[,-1]

set.seed(100)
# we have to find the best value of lambda (the one which minimizes the TMSE), so we will use Cross Validation
lasso_GLM_freq_CV <- cv.glmnet(y=train.data$nbrtotc, xmatrix, family='poisson', offset=train.data$lnexpo, type.measure="mse", standardize=TRUE) #10F CV
# plot(lasso_GLM_freq_CV)
# lasso_GLM_freq_CV$lambda.min  # the minimum value of lambda
# coef(lasso_GLM_freq_CV, s = "lambda.min") # the corresponding coefficients

lasso_GLM_freq <- glmnet(y=train.data$nbrtotc, xmatrix, family='poisson', offset=train.data$lnexpo, type.measure="mse", standardize=TRUE, s=lasso_GLM_freq_CV$lambda.min)
coef(lasso_GLM_freq, s=lasso_GLM_freq_CV$lambda.min)
plot_glmnet(lasso_GLM_freq, label=5, xvar="norm")  # label the 5 biggest final coefs
 

# the last plot say us the most relevant covariates for the dependent variable, if their path is above 0, they have a 
# positive relation with nbrtotc and vice versa --> https://stats.stackexchange.com/questions/154825/what-to-conclude-from-this-lasso-plot-glmnet
# the most important thing is the above axis: the dof are the variable that are not zero for a particular level of lambda, 
# that is the coefficient which penalized the betas in our model

#To end, we cannot take a summary table of our regression --> https://stackoverflow.com/questions/12937331/why-is-it-inadvisable-to-get-statistical-summary-information-for-regression-coef 

# Graphs representing the empirical distribution of the 5 most important variables in LASSO
test.data %>% summarize(emp_freq = sum(nbrtotc) / sum(expo)) 
freq_by_ageph <- test.data %>% group_by(ageph) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))
freq_by_powerc <- test.data %>% group_by(powerc) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))
freq_by_agecar <- test.data %>% group_by(agecar) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))
freq_by_fuel <- test.data %>% group_by(fuelc) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))
freq_by_split <- test.data %>% group_by(split) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))

test.data$split <- factor(test.data$split, ordered = TRUE, levels = c("Once", "Twice", "Thrice", "Monthly"))
freq_by_agecar$agecar <- factor(freq_by_agecar$agecar, ordered = TRUE, levels = c("0-1", "2-5", "6-10", ">10"))
freq_by_powerc$powerc <- factor(freq_by_powerc$powerc, ordered = TRUE, levels = c("<66", "66-110", ">110"))

g4 <- ggplot(freq_by_ageph, aes(x = ageph, y = emp_freq)) + theme_bw() + 
  geom_bar(stat = "identity", color = "red",
           fill = "orange", alpha = .5) + 
  ggtitle("Empirical claim freq per age of ph")

# It is worth mentioning that it is impossible to find the SE in LASSO prediction --> https://stats.stackexchange.com/questions/91462/standard-errors-for-lasso-prediction-using-r 

g5 <- ggplot(freq_by_powerc, aes(x = powerc, y = emp_freq)) + theme_bw() +
  geom_bar(stat = "identity", color = "red", 
           fill = "orange", alpha = .5) + 
  ggtitle("Empirical claim freq per power of the car")

g6 <- ggplot(freq_by_agecar, aes(x = agecar, y = emp_freq)) + theme_bw() +
  geom_bar(stat = "identity", color = "red", 
           fill = "orange", alpha = .5) + 
  ggtitle("Empirical claim freq per age of the car")

g7 <- ggplot(freq_by_fuel, aes(x = fuelc, y = emp_freq)) + theme_bw() +
  geom_bar(stat = "identity", color = "red", 
           fill = "orange", alpha = .5) + 
  ggtitle("Empirical claim freq per fuel")

g8 <- ggplot(freq_by_split, aes(x = split, y = emp_freq)) + theme_bw() +
  geom_bar(stat = "identity", color = "red", 
           fill = "orange", alpha = .5) + 
  ggtitle("Empirical claim freq per payment split")

g9 <- grid.arrange(g4,g5,g6,g7,g8)
g9

# Predictions
xnewmatrix <- model.matrix( ~ nbrtotc+ageph+agecar+usec+sexp+fuelc+split+fleetc+sportc+powerc+coverp+lat*long, data=test.data)[,-1]
lasso_GLM_freq_pred=predict(lasso_GLM_freq, newx=xnewmatrix[,2:19], s=lasso_GLM_freq_CV$lambda.min, type='response', newoffset=test.data$lnexpo)
xnewmatrix2 <- as_tibble(xnewmatrix)
lasso_GLM_freq_pred=as_tibble(lasso_GLM_freq_pred)
test.data_LASSO_GLM_freq=data.frame(test.data,lasso_GLM_freq_pred$"1")
test.data_LASSO_GLM_freq <- rename(test.data_LASSO_GLM_freq, lasso_GLM_freq_pred = lasso_GLM_freq_pred..1.)

# Graphs representing the predicted distribution of the 5 most important variables in LASSO
freq_by_ageph_LASSO <- test.data_LASSO_GLM_freq %>% group_by(ageph) %>% summarize(pred.freq=sum(lasso_GLM_freq_pred)/sum(expo))
g10 <- ggplot(freq_by_ageph_LASSO, aes(x = ageph, y = pred.freq)) + theme_bw() + 
  geom_bar(stat="identity",color = "red", fill = "orange", alpha = .5) + 
  ggtitle("Predicted claim freq per age of ph")
g10

g11 <- ggplot(test.data_LASSO_GLM_freq, aes(as.factor(powerc), lasso_GLM_freq_pred)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=0.5) + 
  xlab("power of the car") + ylab("Fitted Values")
g11

g12 <- ggplot(test.data_LASSO_GLM_freq, aes(as.factor(agecar), lasso_GLM_freq_pred)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=0.5) + 
  xlab("age of the car") + ylab("Fitted Values")
g12

g13 <- ggplot(test.data_LASSO_GLM_freq, aes(as.factor(fuelc), lasso_GLM_freq_pred)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=0.5) + 
  xlab("fuel of the car") + ylab("Fitted Values")
g13

g14 <- ggplot(test.data_LASSO_GLM_freq, aes(as.factor(split), lasso_GLM_freq_pred)) + 
  geom_boxplot(outlier.colour="red", outlier.shape=8, outlier.size=0.5) + 
  xlab("payment split") + ylab("Fitted Values")
g14

# Let's calculate the Test MSE which will be used to compare the GLM with the Machine Learning Method
test_MSE_lasso_GLM <- mean((xnewmatrix2$nbrtotc - lasso_GLM_freq_pred) ^ 2) 
#actually Mean Squared Prediction Error

# Check for over/underdispersion in the model
mean(lasso_GLM_freq_pred)
var(lasso_GLM_freq_pred)
# The fact that the variance is greater than the mean in our dependent variable confirm the assumption of overd.
# We can actually try to reduce it by using mixed models as the Zero Inflated Model
# but actually we can conclude that the GLM is not so efficient to fit the data, so we will use machine learning
# techniques to improve the fit

# Let's predict the annual expected claim frequency for some profiles extracted from test.data
PredOnFreqLassoGLM <- predict(lasso_GLM_freq, newx=xnewmatrix[23:25,2:20], s=lasso_GLM_freq_CV$lambda.min, type='response', newoffset=test.data$lnexpo)
Pred_freq_final <- PredOnFreqLassoGLM*test.data$expo[23:25]

# Let's represent the GLM results by Spatial Data
post_dt <- st_centroid(belgium_shape_sf)
post_dt$long <- do.call(rbind, post_dt$geometry)[,1]
post_dt$lat <- do.call(rbind, post_dt$geometry)[,2]
post_dt$ageph <- test.data$ageph[1]
post_dt$agecar <- test.data$agecar[1]
post_dt$usec <- test.data$usec[1]
post_dt$sexp <- test.data$sexp[1]
post_dt$fuelc <- test.data$fuelc[1]
post_dt$split <- test.data$split[1]
post_dt$fleetc <- test.data$fleetc[1]
post_dt$sportc <- test.data$sportc[1]
post_dt$powerc <- test.data$powerc[1]
post_dt$coverp <- test.data$coverp[1]

post_dt <- as_tibble(post_dt)
xnewmatrix3 <- model.matrix( ~ ageph+lat+long+agecar+usec+sexp+fuelc+split+fleetc+sportc+powerc+coverp, data=post_dt)[,-1]

pred <- predict(lasso_GLM_freq, newx=xnewmatrix3, type = "response", s=lasso_GLM_freq_CV$lambda.min, newoffset=test.data$lnexpo)
dt_pred <- tibble(pc = post_dt$POSTCODE, long = post_dt$long, lat = post_dt$lat, pred)
names(dt_pred)[4] <- "fit_spatial" 

dt_pred <- dplyr::arrange(dt_pred, post_dt$POSTCODE)

post_freq <- dt_pred %>% group_by(pc) %>% summarize(num = n(), total_freq = sum(fit_spatial)) 
post_freq %>% slice(1:5)

belgium_shape_sf <- left_join(belgium_shape_sf, post_freq, by = c("POSTCODE" = "pc"))
belgium_shape_sf$freq <- belgium_shape_sf$total_freq/belgium_shape_sf$Shape_Area

belgium_shape_sf$freq_class <- cut(belgium_shape_sf$freq, breaks = quantile(belgium_shape_sf$freq, c(0,0.2,0.8,1), na.rm = TRUE),
                                   right = FALSE, include.lowest = TRUE, labels = c("low", "average", "high"))
ggplot(belgium_shape_sf) +
  geom_sf(aes(fill = freq_class), colour = "black", size = 0.1) +
  ggtitle("DB claim frequency data") + labs(fill = "Relative\nfreq") +
  scale_fill_brewer(palette = "Blues", na.value = "white") + 
  theme_bw()

belgium_shape_sf <- st_simplify(belgium_shape_sf, dTolerance = 0.00001)
tm_shape(belgium_shape_sf) + tm_borders(col = "black") + 
  tm_fill(col = "freq_class", style = "cont", palette = "Blues", colorNA = "white")
tmap_leaflet(tmap_last())

# Alternativa
# num_bins <- 8
# classint_fisher <- classIntervals(dt_pred$fit_spatial, num_bins, style = "fisher")
# classint_fisher$brks
# min(dt_pred$fit_spatial)
# max(dt_pred$fit_spatial)

# belgium_shape_sf <- left_join(belgium_shape_sf, dt_pred,  by = c("POSTCODE" = "pc"))
# belgium_shape_sf$class_fisher <- cut(belgium_shape_sf$fit_spatial, breaks = classint_fisher$brks, right = FALSE, 
#                                      include.lowest = TRUE, dig.lab = 3)

# ggplot(belgium_shape_sf) + theme_bw() + labs(fill = "Fisher") +
#   geom_sf(aes(fill = class_fisher), colour = NA) +
#   ggtitle("DB claim frequency data") +
#  scale_fill_brewer(palette = "Blues", na.value = "white") +
#  theme_bw()

#---------------------------- 3.2 Gradient Boosting ------------------------------

tgrid <- expand.grid('depth' = c(1,3,5), 'ntrees' = NA, 'oob_err' = NA)

for(i in seq_len(nrow(tgrid))){
  set.seed(100) 
  # Fit a GBM
  GB_GLM <- gbm(nbrtotc ~ ageph+agecar+usec+sexp+fuelc+split+fleetc+sportc+powerc+coverp+lat*long+offset(lnexpo),
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
GB_GLM <- gbm(nbrtotc ~ ageph+agecar+usec+sexp+fuelc+split+fleetc+sportc+powerc+coverp+lat*long+offset(lnexpo),
              data = train.data, distribution = 'poisson', var.monotone = NULL,
              n.trees = tgrid$ntrees[1], interaction.depth = tgrid$depth[1], n.minobsinnode = 1000, shrinkage = 0.1,
              bag.fraction = 0.75, cv.folds = 0)

summary(GB_GLM)
print(GB_GLM)

# Partial Dependence Plot (PDP) for ageph
g15=plot(GB_GLM, i.var = 1, lwd = 2, col = "blue", main = "", type="response")
g15

# Let's check the correlation coef
cor(train.data$ageph,train.data$nbrtotc) #negative correlation coeff --> proven

# Partial Dependence Plot (PDP) for split
g16=plot(GB_GLM, i.var = 6, lwd = 1, col = "blue", main = "", type="response")
g16

# Partial Dependence Plot (PDP) for fuel
g17=plot(GB_GLM, i.var = 5, lwd = 1, col = "blue", main = "", type="response")
g17

# Let's predict the annual expected claim frequency for some profiles extracted from test.data
pred_GB_GLM=predict(GB_GLM, newdata = test.data[23:25,], type = "response", n.trees = 93) 
Pred_freq_final_GB <- pred_GB_GLM*test.data$expo[23:25]

# compute the test error as a function of number of trees
n.trees = seq(from=1 ,to=93, by=1) #no of trees-a vector of 93 values 
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

# Let's represent the GBM results by Spatial Data
pred2 <- predict(GB_GLM, newdata=post_dt, type = "response", n.trees = 93)
dt_pred2 <- tibble(pc = post_dt$POSTCODE, long = post_dt$long, lat = post_dt$lat, pred2)
names(dt_pred2)[4] <- "fit_spatial2" 

dt_pred2 <- dplyr::arrange(dt_pred2, post_dt$POSTCODE)

post_freq2 <- dt_pred2 %>% group_by(pc) %>% summarize(num = n(), total_freq2 = sum(fit_spatial2)) 
post_freq2 %>% slice(1:5)

belgium_shape_sf <- left_join(belgium_shape_sf, post_freq2, by = c("POSTCODE" = "pc"))
belgium_shape_sf$freq2 <- belgium_shape_sf$total_freq2/belgium_shape_sf$Shape_Area

belgium_shape_sf$freq_class <- cut(belgium_shape_sf$freq2, breaks = quantile(belgium_shape_sf$freq2, c(0,0.2,0.8,1), na.rm = TRUE),
                                   right = FALSE, include.lowest = TRUE, labels = c("low", "average", "high"))
ggplot(belgium_shape_sf) +
  geom_sf(aes(fill = freq_class), colour = "black", size = 0.1) +
  ggtitle("DB claim frequency data") + labs(fill = "Relative\nfreq") +
  scale_fill_brewer(palette = "Blues", na.value = "white") + 
  theme_bw()

belgium_shape_sf <- st_simplify(belgium_shape_sf, dTolerance = 0.00001)
tm_shape(belgium_shape_sf) + tm_borders(col = "black") + 
  tm_fill(col = "freq_class", style = "cont", palette = "Blues", colorNA = "white")
tmap_leaflet(tmap_last())

#---------------------------- 4. Severity Modelling ------------------------------
#---------------------------- 4.1 GLM ------------------------------

# Claim severity is highly skewed (to the right), so to model it we can use the Gamma distribution or the 
# log-normal one. We will use the latter since (for the sake of reproducibility) the function glmnet doesn't 
# support the gamma distribution 

# to model the severity we define a new variable, AvClAm = chargtot/nbrtotc that is the average cost of a claim
train.data_sev=train.data %>% filter(chargtot > 0 & chargtot<81000)
train.data %>% filter(chargtot > 0 & chargtot>81000) # 18 observations not included in the train.set because exceed the threshold
test.data_sev=test.data %>% filter(chargtot > 0 & chargtot<81000)
test.data %>% filter(chargtot > 0 & chargtot>81000) # 3 observations not included in the train.set because exceed the threshold
train.data_sev$log_AvClAm = log(train.data_sev$chargtot/train.data_sev$nbrtotc)
test.data_sev$log_AvClAm = log(test.data_sev$chargtot/test.data_sev$nbrtotc)

# we want to prove that the AvClAm variable is lognormally distributed, so we transform the variable and we make a QQPlot of that
qqnorm(train.data_sev$log_AvClAm)
qqline(train.data_sev$log_AvClAm, col="red")

# as we can see the observed values are close to the theoretical line, so we can conclude that it is log-normally distributed

# let's start the Lasso
xmatrix <- model.matrix(log_AvClAm ~ lat+long+ageph+agecar+usec+sexp+fuelc+split+fleetc+sportc+powerc+coverp, data=train.data_sev)[,-1]

set.seed(100)
# we have to find the best value of lambda (the one which minimizes the TMSE), so we will use Cross Validation
lasso_GLM_sev_CV <- cv.glmnet(y=train.data_sev$log_AvClAm, xmatrix, family='gaussian', type.measure="mse", standardize=TRUE) #10F CV
plot(lasso_GLM_sev_CV)
lasso_GLM_sev_CV$lambda.min  # the minimum value of lambda
coef(lasso_GLM_sev_CV, s = "lambda.min") # the corresponding coefficients

lasso_GLM_sev <- glmnet(y=train.data_sev$log_AvClAm, xmatrix, family='gaussian', type.measure="mse", standardize=TRUE, s=lasso_GLM_sev_CV$lambda.min)
coef(lasso_GLM_sev, s=lasso_GLM_sev_CV$lambda.min)
plot_glmnet(lasso_GLM_sev, label=5, xvar="norm")  # label the 5 biggest final coefs

# representation of fitted values (predictions) for the severity
xnewmatrix <- model.matrix( ~ log_AvClAm+lat+long+ageph+agecar+usec+sexp+fuelc+split+fleetc+sportc+powerc+coverp, data=test.data_sev)[,-1]
lasso_GLM_sev_pred=predict(lasso_GLM_sev, newx=xnewmatrix[,2:19], s=lasso_GLM_sev_CV$lambda.min, type='response')




#---------------------------- 4.2 Gradient Boosting ------------------------------