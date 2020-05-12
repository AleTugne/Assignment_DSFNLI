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
library(rpart)
library(rpart.plot)
library(pdp)
library(partykit)
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

# colnames(DB2)[1] <- gsub('^...','',colnames(DB2)[1]) # if in DB2 column 'INS' = 'i..INS'

# Merging the two DB by Postal Code
DB <- left_join(DB1, DB2, by = c("CODPOSS" = "CODPOSS"))

# Rename the columns in lowercase and replace spaces with underscores
DB <- DB %>% rename_all(function(.name) {
  .name %>% tolower 
})
DB <- rename(DB, expo = duree)
DB %>% slice(1:3) 

# Rearrange the columns to reflect informations about: 1)city, 2)ph, 3)car, 4)policy
col_order <- c("codposs", "commune", "lat", "long", "ins", "ageph", "sexp", "agecar", "fuelc", "split", "usec", 
               "fleetc", "sportc", "powerc", "coverp", "expo", "lnexpo", "nbrtotc", "nbrtotan", "chargtot")
DB <- DB[, col_order]

# Rearrange the levels for split, agecar and powerc
DB$split <- factor(DB$split, levels = c("Once", "Twice", "Thrice", "Monthly"))
DB$agecar <- factor(DB$agecar, levels = c("0-1", "2-5", "6-10", ">10"))
DB$powerc <- factor(DB$powerc, levels = c("<66", "66-110", ">110"))

# First some univariate analysis --> See how the variables are distributed on the total number of observations
# 1 - Barcharts / Histograms
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

hist.ageph <- ggplot.hist(DB, DB$ageph, "ageph", 1) + scale_x_continuous(breaks = scales::pretty_breaks(n = 10)) + 
              xlab("P/h age")
bar.sex <- ggplot.bar(DB, DB$sexp, "sex") + xlab("P/h gender")
bar.fuel <- ggplot.bar(DB, DB$fuelc, "fuel") + xlab("Type of fuel")
bar.use <- ggplot.bar(DB, DB$usec, "use") + xlab("Type of use")
bar.power <- ggplot.bar(DB, DB$powerc, "power") + xlab("Power of car")
bar.agecar <- ggplot.bar(DB, DB$agecar, "agec") + xlab("Car age")
bar.sport <- ggplot.bar(DB, DB$sportc, "sport") + xlab("Sport car")
bar.cover <- ggplot.bar(DB, DB$coverp, "cover") + xlab("Type of coverage")
bar.split <- ggplot.bar(DB, DB$split, "split") + xlab("Payment split")
bar.fleet <- ggplot.bar(DB, DB$fleetc, "fleet") + xlab("Part of a Fleet")


#Chargtot*ageph
DB_chargtot <- DB %>% filter(chargtot > 0 & chargtot < 81000)
density.chargtot_ageph <- ggplot(DB_chargtot, aes(x=ageph)) + geom_density(adjust = 5, 
                           col = KULbg, fill = KULbg, alpha = 0.5) + xlim(5,95)+xlab("Chargtot per age") + 
                            scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

#Chargtot*carPower
box.chargtot_powcar <- ggplot(DB_chargtot,aes(x = powerc, y = chargtot)) + geom_boxplot(col = KULbg, 
                        fill = KULbg, alpha = 0.5) + ylab("Claim severity")

g1 <- grid.arrange(hist.ageph, bar.sex, bar.fuel, bar.use, bar.power, bar.agecar, bar.sport, bar.fleet, bar.cover, bar.split)
g1

# 2 - Piecharts
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

pie.sex <- pie.chart.2lvs(DB, DB$sexp, "P/h gender", c("red2", KULbg))
pie.fuel <- pie.chart.2lvs(DB, DB$fuelc, "Type of fuel", c("red2", KULbg))
pie.use <- pie.chart.2lvs(DB, DB$usec, "Type of use", c(KULbg, "red1"))
pie.fleet <- pie.chart.2lvs(DB, DB$fleetc, "Type of insurance", c(KULbg, "red1"))
pie.sport <- pie.chart.2lvs(DB, DB$sportc, "Sport car", c(KULbg, "red1"))
pie.cover <- pie.chart.3lvs(DB, DB$coverp, "Type of coverage")
pie.power <- pie.chart.3lvs(DB, DB$powerc, "Power of car")
pie.split <- pie.chart.4lvs(DB, DB$split, "Payment split")
pie.agecar <- pie.chart.4lvs(DB, DB$agecar, "Car age")

g2 <- grid.arrange(hist.ageph, pie.sex, pie.fuel, pie.use, pie.fleet, pie.sport, pie.cover, pie.power, 
                   pie.split, pie.agecar, ncol = 4)
g2

# Plots of the relevant variables for our analysis 
# 1 - Number of claims (as Frequency) --> Section 3.1 - 3.2
bar.nclaims <- ggplot.bar(DB, variable = DB$nbrtotc, "nclaims") + xlab("Number of claims") + ylab("%")

# 2 - Total cost of claims (as Severity) --> Section 4.1 - 4.2
density.chargtot <- DB_chargtot %>% ggplot(aes(chargtot)) + geom_density(adjust = 3, col = KULbg, 
                                     fill = KULbg, alpha = 0.5) + xlim(0, 1e4) + ylab("%") + 
                                     xlab("Total claim amount") + theme_bw()

# 3 - Exposure --> Section 2
hist.expo <- ggplot.hist(DB, DB$expo, "expo", 0.05) + xlab("Exposure to risk") + ylab("%")

g3 <- grid.arrange(bar.nclaims, density.chargtot, hist.expo)
g3

# Graphs representing the empirical claim frequency of some important variables in LASSO
test.data_freq %>% summarize(emp_freq = sum(nbrtotc) / sum(expo)) 
freq_by_ageph <- test.data_freq %>% group_by(ageph,level) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))
freq_by_coverp <- test.data_freq %>% group_by(coverp) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))
freq_by_agecar <- test.data_freq %>% group_by(agecar) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))
freq_by_fuel <- test.data_freq %>% group_by(fuelc) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))
freq_by_split <- test.data_freq %>% group_by(split) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))

ggplot.bar2 <- function(DT, variable, xlab){
  ggplot(data = DT, aes(x=variable, y=emp_freq)) + theme_bw() + 
    geom_bar(stat = "identity", col = KULbg, fill = KULbg, alpha = .5) + labs(x = xlab, y = "")
}

bar.freq.ageph <- ggplot.bar2(freq_by_ageph, freq_by_ageph$ageph,level, "ageph") +
  ggtitle("Empirical claim freq per age of P/h")

bar.freq.coverp <- ggplot.bar2(freq_by_coverp, freq_by_coverp$coverp, "coverp") +
  ggtitle("Empirical claim freq per type of coverage")

bar.freq.agecar <- ggplot.bar2(freq_by_agecar, freq_by_agecar$agecar, "agecar") +
  ggtitle("Empirical claim freq per age of the car")

bar.freq.fuel <- ggplot.bar2(freq_by_fuel, freq_by_fuel$fuelc, "fuel") +
  ggtitle("Empirical claim freq per fuel")

bar.freq.split <- ggplot.bar2(freq_by_split, freq_by_split$split, "split") +
  ggtitle("Empirical claim freq per payment split")

g4 <- grid.arrange(bar.freq.ageph, bar.freq.coverp, bar.freq.agecar, bar.freq.fuel, bar.freq.split)
g4

#---------------------------- 2. Spatial Data ------------------------------

# We initialize the map of Belgium using SF for further analysis
belgium_shape_sf <- st_read('./shape file Belgie postcodes/npc96_region_Project1.shp', quiet = TRUE)
belgium_shape_sf <- st_transform(belgium_shape_sf, CRS("+proj=longlat +datum=WGS84"))
belgium_shape_sf %>% as_tibble() %>% slice(1:3) 

# As an example, we can plot the relative exposure per area unit
post_expo <- DB %>% group_by(codposs) %>% summarize(num = n(), total_expo = sum(expo)) 
post_expo %>% slice(1:5)

belgium_shape_sf <- left_join(belgium_shape_sf, post_expo, by = c("POSTCODE" = "codposs"))
belgium_shape_sf$freq <- belgium_shape_sf$total_expo/belgium_shape_sf$Shape_Area

belgium_shape_sf$freq_class <- cut(belgium_shape_sf$freq, breaks = quantile(belgium_shape_sf$freq, 
                                   c(0,0.2,0.8,1), na.rm = TRUE), right = FALSE, include.lowest = TRUE, 
                                   labels = c("low", "average", "high"))
ggplot(belgium_shape_sf) +
  geom_sf(aes(fill = freq_class), colour = "black", size = 0.1) +
  ggtitle("Relative Frequency per Area") + labs(fill = "Frequency") +
  scale_fill_brewer(palette = "Blues", na.value = "white") + 
  theme_bw()

#---------------------------- 3. Frequency Modelling ------------------------------
#---------------------------- 3.1 GLM ------------------------------

# Let's recap the main characteristics of the number of claims
bar.nclaims

mean(DB$nbrtotc)
mean_relfreq <- sum(DB$nbrtotc)/sum(DB$expo)
mean_relfreq
variance_relfreq <- sum((DB$nbrtotc - mean_relfreq * DB$expo)^2)/sum(DB$expo)
variance_relfreq

# Some further analysis
# Let us count the proportion of 0 in nbrtotc
100*sum(DB$nbrtotc == 0)/nrow(DB)  #88%

# From here (and from bar.nclaims) we can see that the number of 0 is > 75%, so we have to think about 
# switching from a Poisson GLM to a Zero-Inflated Poisson GLM

# Let's do some buckets for ageph
set.seed(100)
temp.cv <- rpart(cbind(expo,nbrtotc) ~ ageph, data=DB, method="poisson", parms=list(shrink=10), control=rpart.control(minsplit=2, minbucket=1, cp=0, xval=5))
plotcp(temp.cv)
cpt <- as_tibble(temp.cv$cptable)
print(cpt[1:20,], digits = 6)
min_xerr <- min(cpt[,'xerror'])
optimal_CP <- cpt %>% filter(xerror==min(xerror))
temp <- rpart(cbind(expo,nbrtotc) ~ ageph, data=DB, parms=list(shrink=10), method="poisson", control=rpart.control(minsplit=2, minbucket=1, cp=optimal_CP$CP))
temp 
temp <- as.party(temp)
plot(temp)

# Divide ageph based on Rpart
level <- c(15,23.5,26.5,29.5,31.5,37.5,46.5,57.5,77.5,97)

# Let's define the training (80%) and test (20%) sets that will be used from now on
set.seed(100)
training.samples <- DB$nbrtotc %>% createDataPartition(p = 0.8, list = FALSE)
train.data_freq  <- DB[training.samples, ]
test.data_freq <- DB[-training.samples, ]

test.data_freq$split <- factor(test.data_freq$split, levels = c("Once", "Twice", "Thrice", "Monthly"))
test.data_freq$agecar <- factor(test.data_freq$agecar, levels = c("0-1", "2-5", "6-10", ">10"))
test.data_freq$powerc <- factor(test.data_freq$powerc, levels = c("<66", "66-110", ">110"))

# Poisson GLM using LASSO regression from glmnet pack (without commune, INS, codeposs, expo, lnexpo, nbrtotan & chargtot)
xmatrix <- model.matrix(nbrtotc ~ lat+long+cut(ageph,level)+agecar+usec+sexp+fuelc+split+fleetc+sportc+powerc+coverp,
                        data=train.data_freq)[,-1]

# Let's use 10f CV to find the best value of lambda (the one which minimizes the TMSE)
set.seed(100)
lasso_GLM_freq_CV <- cv.glmnet(y=train.data_freq$nbrtotc, xmatrix, family='poisson', offset=train.data_freq$lnexpo,
                               type.measure="deviance", standardize=TRUE)
 plot(lasso_GLM_freq_CV)
# lasso_GLM_freq_CV$lambda.1se # the minimum value of lambda
# coef(lasso_GLM_freq_CV, s = "lambda.1se") # the corresponding coefficients

lasso_GLM_freq <- glmnet(y=train.data_freq$nbrtotc, xmatrix, family='poisson', offset=train.data_freq$lnexpo, 
                         type.measure="deviance", standardize=TRUE, s=lasso_GLM_freq_CV$lambda.1se)

coef(lasso_GLM_freq, s=lasso_GLM_freq_CV$lambda.1se)
plot_glmnet(lasso_GLM_freq, label=10, xvar="norm")  # label the 5 biggest final coefs
 
# Fit a GLM on the most important variables selected by LASSO
GLM_freq <- glm(nbrtotc~cut(ageph,level)+agecar+fuelc+split+coverp+powerc, data=train.data_freq, family=poisson(link="log"), offset=lnexpo)

#GLM results
summary(GLM_freq)
#plot(GLM_freq)
BIC(GLM_freq)
anova(GLM_freq, test="Chisq")

# Let's predict the annual expected claim frequency for the test.data
freq_prediction_GLM <- (predict(GLM_freq, test.data_freq, type='response'))

# Partial dependence plots of the variables in GLM_freq
# ageph
a <- min(test.data_freq$ageph):max(test.data_freq$ageph)
freq_pred_ageph <- predict(GLM_freq, newdata = data.frame(ageph=a, expo=1, lnexpo=0, agecar=test.data_freq$agecar[1], coverp=test.data_freq$coverp[1], fuelc=test.data_freq$fuelc[1], split=test.data_freq$split[1], powerc=test.data_freq$powerc[1]), type = "terms",se.fit = TRUE)
b_pred_age <- freq_pred_ageph$fit
l_pred_age <- freq_pred_ageph$fit - qnorm(0.975)*freq_pred_ageph$se.fit
u_pred_age <- freq_pred_ageph$fit + qnorm(0.975)*freq_pred_ageph$se.fit
df <- data.frame(a, b_pred_age, l_pred_age, u_pred_age)
p_pred_age_freq <- ggplot(df, aes(x = a))
p_pred_age_freq <- p_pred_age_freq + geom_line(aes(a, b_pred_age[,1]), size = 1, col = KULbg)   
p_pred_age_freq <- p_pred_age_freq + geom_line(aes(a, u_pred_age[,1]), size = 0.5, linetype = 2, col = KULbg) + geom_line(aes(a, l_pred_age[,1]), size = 0.5, linetype = 2, col = KULbg)
p_pred_age_freq <- p_pred_age_freq + xlab("ageph") + ylab("fit") + theme_bw()

partial <- pdp::partial
partial(GLM_freq, pred.var = c("ageph"), plot = TRUE)
p_pred_agecar_freq <- partial(GLM_freq, pred.var = c("agecar"), plot = TRUE, col = KULbg)
p_pred_fuelc_freq <- partial(GLM_freq, pred.var = c("fuelc"), plot = TRUE, col = KULbg)
p_pred_split_freq <- partial(GLM_freq, pred.var = c("split"), plot = TRUE, col = KULbg)
p_pred_coverp_freq <- partial(GLM_freq, pred.var = c("coverp"), plot = TRUE, col = KULbg)
p_pred_powerc_freq <- partial(GLM_freq, pred.var = c("powerc"), plot = TRUE, col = KULbg)

g5 <- grid.arrange(p_pred_age_freq, p_pred_fuelc_freq, p_pred_split_freq, p_pred_coverp_freq, p_pred_powerc_freq)
g5

# Let's compute the Test MSE to compare GLM and GBM
test_MSE_GLM_freq <- mean((test.data_freq$nbrtotc - freq_prediction_GLM) ^ 2) 
#actually Mean Squared Prediction Error

#---------------------------- 3.2 Gradient Boosting ------------------------------

# Let's find the optimal number of trees by OOB
GB_freq <- gbm(nbrtotc ~ lat+long+ageph+agecar+usec+sexp+fuelc+split+fleetc+sportc+powerc+coverp+offset(lnexpo),
               data = train.data_freq, distribution = 'poisson', var.monotone = rep(0,12),
               n.trees = 200, interaction.depth = 1, n.minobsinnode = 100, shrinkage = 0.1,
               bag.fraction = 0.75, train.fraction = 1, cv.folds = 5, verbose=TRUE)

best.iter.oob_freq <- gbm.perf(GB_freq, method = "OOB")
print(best.iter.oob_freq)

# Fit the optimal GBM
summary(GB_freq, n.trees = best.iter.oob_freq)
print(GB_freq, n.trees = best.iter.oob_freq)

# Partial Dependence Plot (PDP) for ageph
PDP_ageph <- plot(GB_freq, i.var = 3, lwd = 1, col = KULbg, main = "", type="response")
PDP_ageph

# Partial Dependence Plot (PDP) for split
PDP_split <- plot(GB_freq, i.var = 8, lwd = 1, col = KULbg, main = "", type="response")
PDP_split

# Partial Dependence Plot (PDP) for fuel
PDP_fuel <- plot(GB_freq, i.var = 7, lwd = 1, col = KULbg, main = "", type="response")
PDP_fuel

# Let's predict the annual expected claim frequency for the test.data
freq_prediction_GB <- (predict(GB_freq, newdata = test.data_freq, type = "response", 
                        n.trees = best.iter.oob_freq))

# Compute the test error as a function of number of trees
n.trees <- seq(from = 1, to = best.iter.oob_freq, by = 1) 
predmatrix <- predict(GB_freq, test.data_freq, n.trees = n.trees, type = "response")

# Calculating The Mean Squared Test Error
test_MSE_GB_freq <- with(test.data_freq, apply((predmatrix-nbrtotc)^2, 2, mean))
Min_test_MSE_GB_freq <- min(test_MSE_GB_freq)

# Plotting the test error vs number of trees
plot(n.trees, test_MSE_GB_freq, pch=19, col=KULbg, xlab="Number of Trees", ylab="Test Error", 
     main = "Perfomance of Boosting on Test Set")
abline(h = min(test_MSE_GB_freq), col="red")
legend("topright", c("Min. MSTE"), col="red", lty=1, lwd=1)

# Let's represent the GBM results by Spatial Data
# Blegium shape file
post_dt <- st_centroid(belgium_shape_sf)
post_dt$long <- do.call(rbind, post_dt$geometry)[,1]
post_dt$lat <- do.call(rbind, post_dt$geometry)[,2]

# gbm with only spatial effects considered and derived optimal parameters
GB_freq_1 <- gbm(nbrtotc ~ lat+long+offset(lnexpo),
                 data = train.data_freq, distribution = 'poisson', var.monotone = rep(0,2),
                 n.trees = 200, interaction.depth = 2, n.minobsinnode = 100, shrinkage = 0.1,
                 bag.fraction = 0.75, train.fraction = 1, cv.folds = 5, verbose=TRUE)

best.iter.oob_1 <- gbm.perf(GB_freq_1, method = "OOB")

pred <- predict(GB_freq_1, newdata = post_dt, n.trees = best.iter.oob_1, type = "response", ir.var = c(1,2))

dt_pred <- data.frame(pc = post_dt$POSTCODE,
                      long = post_dt$long,
                      lat = post_dt$lat, pred)
names(dt_pred)[4] <- "fit_spatial"

belgium_shape_sf <- left_join(belgium_shape_sf,
                              dt_pred,
                              by = c("POSTCODE" =
                                       "pc"))
#version 1
ggplot(belgium_shape_sf) +
  geom_sf(aes(fill = fit_spatial), colour = NA) +
  ggtitle("Gradient Boosting machine - frequency") +
  scale_fill_gradient(low = "#99CCFF",
                      high = "#003366") +
  theme_bw()

#version 2 
tm_shape(belgium_shape_sf) +
  tm_borders(col = 'white', lwd = .1) +
  tm_fill("fit_spatial", style = "cont",
          palette = "RdBu", legend.reverse = TRUE,
          auto.palette.mapping = TRUE) +
  tm_layout(legend.title.size = 1.0,
            legend.text.size = 1.0)

#---------------------------- 4. Severity Modelling ------------------------------
#---------------------------- 4.1 GLM ------------------------------

train.data_sev <- train.data_freq %>% filter(chargtot > 0 & chargtot < 81000)
train.data_freq %>% filter(chargtot > 0 & chargtot > 81000) # 18 observations exceeding the threshold
test.data_sev <- test.data_freq %>% filter(chargtot > 0 & chargtot < 81000)
test.data_freq %>% filter(chargtot > 0 & chargtot > 81000) # 3 observations exceeding the threshold
train.data_sev$log_AvClAm = log(train.data_sev$chargtot/train.data_sev$nbrtotc)
test.data_sev$log_AvClAm = log(test.data_sev$chargtot/test.data_sev$nbrtotc)

# AvClAm is lognormally distributed
qqnorm(train.data_sev$log_AvClAm, col = KULbg)
qqline(train.data_sev$log_AvClAm, col = "red")

# let's start the LASSO
xmatrix2 <- model.matrix(log_AvClAm ~ lat+long+ageph+agecar+usec+sexp+fuelc+split+fleetc+sportc+powerc+coverp, 
                         data=train.data_sev)[,-1]

set.seed(100)
lasso_GLM_sev_CV <- cv.glmnet(y=train.data_sev$log_AvClAm, xmatrix2, family='gaussian', type.measure="mse", standardize=TRUE) #10F CV
# plot(lasso_GLM_sev_CV)
# lasso_GLM_sev_CV$lambda.1se  # the minimum value of lambda
# coef(lasso_GLM_sev_CV, s = "lambda.min") # the corresponding coefficients

lasso_GLM_sev <- glmnet(y=train.data_sev$log_AvClAm, xmatrix2, family='gaussian', type.measure="mse", 
                        standardize=TRUE, s=lasso_GLM_sev_CV$lambda.1se)

coef(lasso_GLM_sev, s=lasso_GLM_sev_CV$lambda.1se)
plot_glmnet(lasso_GLM_sev, label=5, xvar="norm")  # label the 5 biggest final coefs

# Fit a GLM on the most important variables selected by LASSO
GLM_sev <- glm(log_AvClAm~split+coverp, data=train.data_sev, family=gaussian)

#GLM results
summary(GLM_sev)
#plot(GLM_freq)
BIC(GLM_sev)
anova(GLM_sev, test="F")

# Let's predict the annual expected claim severity for the test.data
sev_prediction_GLM <- (predict(GLM_sev, test.data_sev, type='response'))

# Partial dependence plots of the variables in GLM_freq
p_pred_split_sev <- partial(GLM_sev, pred.var = c("split"), plot = TRUE)
p_pred_coverp_sev <- partial(GLM_sev, pred.var = c("coverp"), plot = TRUE)

g6 <- grid.arrange(p_pred_split_sev, p_pred_coverp_sev)

# Let's compute the Test MSE to compare GLM and GBM
test_MSE_GLM_sev <- mean((test.data_sev$log_AvClAm - sev_prediction_GLM) ^ 2) 
#actually Mean Squared Prediction Error

#---------------------------- 4.2 Gradient Boosting ------------------------------

# Let's find the optimal number of trees by OOB
GB_sev <- gbm(log_AvClAm ~ lat+long+ageph+agecar+usec+sexp+fuelc+split+fleetc+sportc+powerc+coverp,
               data = train.data_sev, distribution = 'gaussian', var.monotone = rep(0,12),
               n.trees = 200, interaction.depth = 1, n.minobsinnode = 100, shrinkage = 0.1,
               bag.fraction = 0.75, train.fraction = 1, cv.folds = 5, verbose=TRUE)

best.iter.oob_sev <- gbm.perf(GB_sev, method = "OOB")
print(best.iter.oob_sev)

# Fit the optimal GBM
summary(GB_sev, n.trees = best.iter.oob_sev)
print(GB_sev, n.trees = best.iter.oob_sev)

# Partial Dependence Plot (PDP) for ageph
PDP_ageph <- plot(GB_sev, i.var = 3, lwd = 2, col = KULbg, main = "")
PDP_ageph

# Partial Dependence Plot (PDP) for split
PDP_split <- plot(GB_sev, i.var = 8, lwd = 1, col = KULbg, main = "")
PDP_split

# Partial Dependence Plot (PDP) for cover
PDP_cover = plot(GB_sev, i.var = 12, lwd = 1, col = KULbg, main = "")
PDP_cover

# Let's predict the annual expected claim severity for the test.data
sev_prediction_GB <- predict(GB_sev, newdata = test.data_sev, type = "response", n.trees = best.iter.oob_sev) 

# Compute the test error as a function of number of trees
n.trees <- seq(from = 1, to = best.iter.oob_sev, by = 1) 
predmatrix <- predict(GB_sev, test.data_sev, n.trees = n.trees, type = "response")

# Calculating The Mean Squared Test Error
test_MSE_GB_sev <- with(test.data_sev, apply((predmatrix-log_AvClAm)^2, 2, mean))
Min_test_MSE_GB_sev <- min(test_MSE_GB_sev)

# Plotting the test error vs number of trees
plot(n.trees, test_MSE_GB_sev, pch=19, col=KULbg, xlab="Number of Trees", ylab="Test Error", 
     main = "Perfomance of Boosting on Test Set")
abline(h = min(test_MSE_GB_sev), col="red")
legend("topright", c("Min. MSTE"), col="red", lty=1, lwd=1)

