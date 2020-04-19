# --------------------------- 0. Setup --------------------------------------

# Importing libraries
library(glmulti)
library(tidyverse)
library(rgdal)
library(mapview)
library(sf)
library(tmap)
library(caret)
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

# Some analysis
mean(DB$nbrtotc)

mean = sum(DB$nbrtotc)/sum(DB$expo)
mean
variance = sum((DB$nbrtotc - mean * DB$expo)^2)/sum(DB$expo)
variance

DB %>% summarize(emp_freq = sum(nbrtotc) / sum(expo)) 
freq_by_sex = DB %>% group_by(sexp) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))
freq_by_age = DB %>% group_by(ageph) %>% summarize(emp_freq = sum(nbrtotc) / sum(expo))

# Some graphs
g1 = ggplot(freq_by_age, aes(x = ageph, y = emp_freq)) + theme_bw() + 
  geom_bar(stat = "identity", color = "red",
           fill = "orange", alpha = .5) + 
  ggtitle("MTPL - empirical claim freq per age policyholder")
g1

g2 = ggplot(freq_by_sex, aes(x = sexp, y = emp_freq)) + theme_bw() +
  geom_bar(stat = "identity", color = "red", 
           fill = "orange", alpha = .5) + 
  ggtitle("MTPL - empirical claim freq per sex policyholder")
g2

col = "red"
fill = "orange"
ylab = "Relative frequency"
ggplot.bar = function(DT, variable, xlab){
  ggplot(data = DT, aes(as.factor(variable))) + theme_bw() + 
    geom_bar(aes(y = (..count..)/sum(..count..)), col = col, fill = fill, alpha = 0.5) + labs(x = xlab, y = ylab)
}

ggplot.hist = function(DT, variable, xlab, binwidth){
  ggplot(data = DT, aes(variable)) + theme_bw() + 
    geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = binwidth, col = col, fill = fill, alpha = 0.5) + 
    labs(x = xlab, y = ylab)
}

#Frequency
plot.eda.nclaims = ggplot.bar(DB, variable = DB$nbrtotc, "nclaims")
#Exposure
plot.eda.exp = ggplot.hist(DB, DB$expo, "expo", 0.05)
#Severity
DB.sev = DB %>% filter(chargtot > 0 & nbrtotan <= 81000)
plot.eda.amount = ggplot(data = DB.sev, aes(chargtot)) + 
  geom_density(adjust = 3, col = col, fill = fill, alpha = 0.5) + 
  xlim(0, 1e4) + ylab(ylab) + xlab("severity") + theme_bw()

g3 = grid.arrange(plot.eda.nclaims, plot.eda.exp, plot.eda.amount)
g3

plot.eda.fuel = ggplot.bar(DB, DB$fuelc, "fuel")
plot.eda.sex = ggplot.bar(DB, DB$sexp, "sex")
plot.eda.use = ggplot.bar(DB, DB$usec, "use")
DB$split = factor(DB$split, ordered = TRUE, levels = c("Once", "Twice", "Thrice", "Monthly"))
plot.eda.split = ggplot.bar(DB, DB$split, "split")
DB$agecar = factor(DB$agecar, ordered = TRUE, levels = c("0-1", "2-5", "6-10", ">10"))
plot.eda.agecar = ggplot.bar(DB, DB$agecar, "agec")
plot.eda.ageph = ggplot.hist(DB, DB$ageph, "ageph", 1)

g4 = grid.arrange(plot.eda.fuel, plot.eda.sex, plot.eda.use, plot.eda.split, plot.eda.ageph, plot.eda.agecar)
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

# Let's calculate the R2, RMSE and MAE to compare the different models
predictions1 <- F_GLM %>% predict(DB)
data.frame( R2 = R2(predictions1, DB$nbrtotc),
            RMSE = RMSE(predictions1, DB$nbrtotc),
            MAE = MAE(predictions1, DB$nbrtotc))

### Cross validation approaches to GLM using {caret}
## 1 - Validation Set Approach
# Split the data into training (80%) and test (20%) set 
set.seed(123)
training.samples <- DB$nbrtotc %>% createDataPartition(p = 0.8, list = FALSE)
train.data  <- DB[training.samples, ]
test.data <- DB[-training.samples, ]

g5 <- ggplot(train.data, aes(x = nbrtotc)) + theme_bw() + geom_density(trim = TRUE) +
        geom_density(data = test.data, trim = TRUE, col = "red") + 
        theme(axis.title.y = element_blank(), axis.ticks.y = element_blank(), axis.text.y = element_blank()) +
        ggtitle("Caret splitting") 
g5

F_GLM2 <- glm(nbrtotc~agecar+fuelc+split+lat+ageph+offset(lnexpo), data=train.data, fam = poisson(link = log))

# Make predictions and compute the R2, RMSE and MAE
predictions2 <- F_GLM2 %>% predict(test.data)
data.frame( R2 = R2(predictions2, test.data$nbrtotc),
            RMSE = RMSE(predictions2, test.data$nbrtotc),
            MAE = MAE(predictions2, test.data$nbrtotc))

# So, in this case the F_GLM remain the best (RMSE --> 2.436933 vs. 2.446796)

## 2 - 5-Fold Cross Validation
set.seed(123) 
train.control <- trainControl(method = "cv", number = 5, returnResamp = "all", selectionFunction = "best")
hyper_grid <- expand.grid(k = seq(2, 2000, by = 2))
# Train the model
F_GLM3 <- train(nbrtotc~agecar+fuelc+split+lat+ageph+offset(lnexpo), data = DB, method = "knn", family = poisson(link = log),
               trControl = train.control, tuneGrid = hyper_grid, metric = "Accuracy")
# Summarize the results
print(F_GLM3)


#ggplot() + theme_bw() +
#  geom_line(data = knn_fit$results, aes(k, RMSE)) +
#  geom_point(data = knn_fit$results, aes(k, RMSE)) +
#  geom_point(data = filter(knn_fit$results, k == as.numeric(knn_fit$bestTune)),
#             aes(k, RMSE),
#             shape = 21,
#             fill = "yellow",
#             color = "black",
#             stroke = 1,
#             size = 3) +
#  scale_y_continuous("Error (RMSE)")
# Define training control

