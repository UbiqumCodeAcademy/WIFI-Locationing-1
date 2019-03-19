#### 1- Setting up the R environement ####

library(readr)

pacman:: p_load(pacman,
                readr,
                tidyr,
                reshape2, 
                dplyr, 
                ggplot2,
                ggmap, 
                plotly, 
                randomForest, 
                caret, 
                viridis, 
                cowplot,
                bindrcpp, 
                doMC, 
                e1071,
                class, 
                kknn,
                gbts,
                Metrics,
                dummies
)
####Parallel processing####
# getDoParWorkers()
# makeCluster(3)
registerDoMC(cores=4)

#### 2- Load Data Sets ####

td <- read_csv("~/Desktop/Ubiqum/Data Analysis/RStudio/Wifi Location/UJIndoorLoc/trainingData.csv") # you need to fix this path
vd <- read_csv("~/Desktop/Ubiqum/Data Analysis/RStudio/Wifi Location/UJIndoorLoc/validationData.csv") # you need to fix this path


#### 2.1- Deleting WAPS that are useless (=100) ####

#Train
notworking_waps <- which(apply(td [,1:520], 2, function(x) mean(x)) == 100)
notworking_row <- which(apply(td [,1:520], 1, function(x) mean(x)) == 100)
tdwaps <- td [-c(notworking_row), -c(notworking_waps)]

#Validation
validation_notworking_waps <- which(apply(vd [,1:520], 2, function(x) mean(x)) == 100)
vdwaps <- vd [, -c(validation_notworking_waps)]

#### 3- Convert Variables #### 

#Test # try to create a loop or apply to fix this!
tdwaps$FLOOR <- as.factor(tdwaps$FLOOR)
tdwaps$BUILDINGID <- as.factor(tdwaps$BUILDINGID)
tdwaps$SPACEID <- as.factor(tdwaps$SPACEID)
tdwaps$RELATIVEPOSITION <- as.factor(tdwaps$RELATIVEPOSITION)
tdwaps$USERID <- as.factor(tdwaps$USERID)
tdwaps$PHONEID <- as.factor(tdwaps$PHONEID)
tdwaps$TIMESTAMP <- as.POSIXct(as.numeric(tdwaps$TIMESTAMP), origin = '1970-01-01')

#Validation
vdwaps$FLOOR <- as.factor(vdwaps$FLOOR)
vdwaps$BUILDINGID <- as.factor(vdwaps$BUILDINGID)
vdwaps$SPACEID <- as.factor(vdwaps$SPACEID)
vdwaps$RELATIVEPOSITION <- as.factor(vdwaps$RELATIVEPOSITION)
vdwaps$USERID <- as.factor(vdwaps$USERID)
vdwaps$PHONEID <- as.factor(vdwaps$PHONEID)
vdwaps$TIMESTAMP <- as.POSIXct(as.numeric(vdwaps$TIMESTAMP), origin = '1970-01-01')

#### 4- Creating a new column w/ strongest WAP on the row ####

#Test
tdwaps[, 1:465] <- as.data.frame(apply(
  tdwaps[, 1:465], 2, function(x) {ifelse(x==100, -105,x)}))

tdwaps <- filter(tdwaps, USERID != 6)

# zerovariancewaps <- as.data.frame(apply(tdwaps [WAP], 1, 
#                           nearZeroVar, saveMetrics = TRUE))
# 
# wifidt <- tdwaps[, -zerovariancewaps] 
# I am pretty sure NearZeroVariance doesnt go this way

#Validation
vdwaps[,1:367] <- as.data.frame(apply(vdwaps[, 1:367], 2, function(x) {ifelse(x==100, -105,x)}))

names(tdwaps)
## Intersect
trainwaps <- tdwaps[, 1:465]
validationwaps <- vdwaps[, 1:367]
traininfo <- tdwaps[, 466:474]
validationinfo <- vdwaps[, 368:376]

trainwaps <- names(trainwaps)
validationwaps <- names(validationwaps)

similarWAPS <- intersect(trainwaps, validationwaps)

usefulwapsintd <- tdwaps[,c(similarWAPS)]
usefulwapsinvd <- vdwaps[,c(similarWAPS)]

trainWAP <- cbind(usefulwapsintd, traininfo)
validationWAP <- cbind(usefulwapsinvd, validationinfo)
trainWAP <- trainWAP [, -(317:321)]
validationWAP <- validationWAP[, -(317:321)]
names(trainWAP)
names(validationWAP)

WAP <- grep("WAP", names(trainWAP), value = T)
WAPvd <- grep("WAP", names(validationWAP), value = T)

trainWAP$maxWAP <- apply(trainWAP[WAP], 1, which.max)
validationWAP$maxWAP <- apply(validationWAP[WAPvd], 1, which.max)


#### 5- Convert from wide to long for visualization ####

#Test
lttd <- melt(trainWAP, id.vars = c(466:475))
traincompletelt <-melt(td, id.vars = c(521:529))
lttd <- filter(lttd, value != 100)
lttd <- filter(lttd, USERID != 6)
maxwapgrouped <- dplyr::group_by(tdwaps, maxWAP)

#Validation
ltvd <- melt(vdwaps, id.vars = c(368:377))
ltvd <- filter(ltvd, value != 100)
testcompletelt <- melt(vd, id.vars = c(521:529))


### 5.1 So, where are the actual routers? ###
closeWAPS <- filter(train, value > -67) 
closeWAPS <- filter(closeWAPS, USERID != 6)
plot(closeWAPS$LONGITUDE, closeWAPS$LATITUDE)
plot(train$USERID)

WAPSoutliers <- filter(closeWAPS, value > -30)
plot( WAPSoutliers$LATITUDE, WAPSoutliers$LONGITUDE)
list(WAPSoutliers$USERID)
plot(WAPSoutliers$USERID)

#### 5.2 Separate Strong Signal WorkingWAPs data by building ####
#BL 0
closewaps_bld0 <- filter(closeWAPS, BUILDINGID == 0)

#BL 1
closewaps_bld1 <- filter(working_waps, BUILDINGID == 1)

#BL2
closewaps_bld2 <- filter(working_waps, BUILDINGID == 2)

##PLOTS

plot(closewaps_bld0$LATITUDE, closewaps_bld0$LONGITUDE)
plot(closewaps_bld1$LATITUDE, closewaps_bld1$LONGITUDE)
plot(closewaps_bld2$LATITUDE, closewaps_bld2$LONGITUDE)

#### 5.3- Break it dowm per floor (0,1,2,3,4) ####

#Floor 0 #do a loop to save these data frames! The easiest way is to store it in a list. 
closewaps_bld0_fl0 <- filter(closewaps_bld0, FLOOR == 0)
plot(closewaps_bld0_fl0$LATITUDE, closewaps_bld0_fl0$LONGITUDE)

#Floor 1
closewaps_bld0_fl1 <- filter(closewaps_bld0, FLOOR == 1)
plot(closewaps_bld0_fl1$LATITUDE, closewaps_bld0_fl1$LONGITUDE)

#Floor 2
closewaps_bld0_fl2 <- filter(closewaps_bld0, FLOOR == 2)
plot(closewaps_bld0_fl2$LATITUDE, closewaps_bld0_fl2$LONGITUDE)

#Floor 3
closewaps_bld0_fl3 <- filter(closewaps_bld0, FLOOR == 3)
plot(closewaps_bld0_fl3$LATITUDE, closewaps_bld0_fl3$LONGITUDE)

#Floor 4
closewaps_bld0_fl4 <- filter(closewaps_bld0, FLOOR == 4)
plot(closewaps_bld0_fl4$LATITUDE, closewaps_bld0_fl4$LONGITUDE) 
bld0complete <- filter(traincomplete, BUILDINGID==0)
wapsfl4_bld0 <- filter(bld0complete , FLOOR==4)

#Aqui me sale entonces que no hay WAPs, pero hago un list y me dice que si...


#### 6- Plots and visualization ####

#Test
ggplot(lttd, aes(x=USERID, y= abs(value)))+
  geom_bar(stat = "identity", colour = "green")

ggplot(closeWAPS, aes(x=RELATIVEPOSITION, y= abs(value)))+
  geom_bar(stat = "identity", colour = "red")

ggplot(data = ltvd) +
  aes(x = value, fill = FLOOR) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of WAPS per Building",
       subtitle = "On Train") +
  theme_minimal() +
  facet_wrap(vars(BUILDINGID))

ggplot(data = tdwaps) +
  aes(x = maxWAP, fill = FLOOR) +
  geom_histogram(bins = 30) +
  labs(title = "Distribution of WAPS per Building",
       subtitle = "On Train") +
  theme_minimal() +
  facet_wrap(vars(BUILDINGID))

plot1 <- plot_ly(lttd, x= ~LATITUDE, 
                 y = ~LONGITUDE, 
                 z = ~FLOOR, 
                 marker = list(color = ~ FLOOR, 
                               colourscale = "earth", 
                               showscale = FALSE)) %>%
  add_markers()%>%
  layout(scene = list(xaxis = list(title = 'Latitude'),
                      yaxis = list(title = 'Longitude'),
                      zaxis = list(title = 'Floor')))

plot1

plot2 <- plot_ly(maxwaps, x= ~LATITUDE, 
                 y = ~LONGITUDE, 
                 z = ~FLOOR, 
                 marker = list(color = ~ value, 
                               colourscale = "earth", 
                               showscale = TRUE)) %>%
  add_markers()%>%
  layout(scene = list(xaxis = list(title = 'Latitude'),
                      yaxis = list(title = 'Longitude'),
                      zaxis = list(title = 'Floor')),
         annotations = list(
           x = 1.13,
           y = 1.05,
           text = 'WAPS Strength ',
           xref = 'paper',
           yref = 'paper',
           showarrow = FALSE
         ))
plot2

plot(LATITUDE ~ LONGITUDE, data = lttd, pch = 20, col = "cyan")

#### 7- Sampling ####

#Test
# Sampling n=60
sample60 <- trainWAP %>% group_by(BUILDINGID, FLOOR) %>% sample_n(60)
# Visualization
table(sample60$FLOOR)
table(sample60$BUILDINGID)

# Filter by building 
sample60_b0 <- filter(sample60, BUILDINGID == 0)
sample60_b1 <- dplyr::filter(sample60, BUILDINGID == 1)
sample60_b2 <- dplyr::filter(sample60, BUILDINGID == 2)

## Ploting

a <- htmltools::tagList()

for (i in unique(sample60$BUILDINGID)) {
  a[[i]] <- sample60 %>% dplyr::filter(BUILDINGID == i) %>% 
    plot_ly( x = ~ LONGITUDE, 
             y = ~ LATITUDE, 
             z = ~ FLOOR, 
             type = "scatter3d", 
             mode = "markers")
}

plotsample60 <- plot_ly(sample60, x= ~LONGITUDE, 
                        y = ~LATITUDE, 
                        z = ~FLOOR, 
                        type = "scatter3d", 
                        mode = "markers", 
                        color = ~BUILDINGID)
plotsample60

plotb0 <- plot_ly(b0, 
                  x= ~LONGITUDE, 
                  y = ~LATITUDE, 
                  z = ~FLOOR, 
                  type = "scatter3d", 
                  mode = "markers", 
                  color = ~FLOOR)
plotb0



#Validation


#### 8- Modeling ####

set.seed(123)

# ### 8.1 BUILDING####
# Get the best mtry / PREDICTING BUILDING
# Random Forest
# Accuracy     Kappa
# 0.9990999 0.9985770
# Best mtry =
besmtry_rf <- tuneRF(trainWAP[WAP],
                     trainWAP$BUILDINGID,
                     stepFactor = 2,
                     improve = TRUE,
                     trace = TRUE,
                     plot = T)

# Train a random forest mtry = 9
#142 seconds
system.time(rf_building <- randomForest(x = trainWAP[WAP],
                                        y = trainWAP$BUILDINGID,
                                        importance = TRUE,
                                        do.trace = TRUE,
                                        ntree = 200,
                                        mtry = 9))


# RF - Building Prediction Model:
RFbuildingpred <- predict(rf_building, validationWAP)

#Performance
perfRFbuildingpred <- postResample(RFbuildingpred, validationWAP$BUILDINGID)

#Confusion Matrix
confusionMatrix(RFbuildingpred, validationWAP$BUILDINGID)

#### KNN
# Accuracy     Kappa
#0.9693969 0.9520621
# 208 seconds

#STANDARIZE parameters from the dataset
preprocessKNNBuiLding <- preProcess(trainWAP[WAP],method = c("center", "scale"))
preprocessKNNBuildingValidation <- preProcess(validationWAP[WAPS], method = c("center", "scale"))

standarizedKNNbldgWAPStd <- predict(preprocessKNNBuiLding, trainWAP[WAP])
standarizedKNNbldgWAPSvd <- predict(preprocessKNNBuildingValidation, validationWAP[WAP])

standKNNbldgTD <- cbind(standarizedKNNbldgWAPStd, BUILDINGID =trainWAP$BUILDINGID)
standKNNbldgVD <- cbind(standarizedKNNbldgWAPSvd, BUILDINGID =validationWAP$BUILDINGID)

#Train KNN
system.time(knn_building <- train.kknn(BUILDINGID~.,
                                       data = standKNNbldgTD,
                                       kernel = "optimal",
                                       kmax = 9,
                                       scale = FALSE))

# KNN- Building Prediction Model:
KNNbuildingpred <- predict(knn_building, standKNNbldgVD)

#Performance
perfKNNbuildingpred <- postResample(KNNbuildingpred, standKNNbldgVD$BUILDINGID)

#Confusion Matrix
confusionMatrix(KNNbuildingpred, standKNNbldgVD$BUILDINGID)

##SVM
# Accuracy     Kappa
# 0.9486949 0.9197740
# 65 seconds
system.time(svm_building <- train(BUILDINGID~.,
                                  data = standKNNbldgTD,
                                  method = "svmLinear",
                                  trControl = trainControl(verboseIter = TRUE)))

# SVM - Building Prediction Model:
SVMbuildingpred <- predict(svm_building, standKNNbldgVD)

#Performance
perfSVMbuildingpred <- postResample(SVMbuildingpred, standKNNbldgVD$BUILDINGID)

#Confusion Matrix
confusionMatrix(SVMbuildingpred, standKNNbldgVD$BUILDINGID)

#SAVE MODEL - RANDOM FOREST SELECTED
save(RFbuildingpred, file = "RF_PREDICTION_BLDG.rda")

#### LOAD MODEL ####
load("RF_PREDICTION_BLDG.rda")

#### 8.1.2 CREATE NEW VALIDATION DATA SET WITH PREDICTED BUILDING ####

RFbuildingpred
validationWAP2 <- validationWAP
validationWAP2$BUILDINGID <-RFbuildingpred
validationWAP2 <- validationWAP2[, -c(313, 314)]
names(validationWAP2)

### Create new TRAIN DATA FRAME for predicting FLOOR that included BUILDING and FLOOR
trainWAPFLOOR <- trainWAP[, -c(313, 314)]
names(trainWAPFLOOR)

WAPSFLOOR <- grep("WAP", names(trainWAPFLOOR), value = T)
set.seed(123)
names(trainWAPFLOOR)
#### 8.2 FLOOR ####
# Get the best mtry / PREDICTING BUILDING
#Random Forest
# Accuracy     Kappa
# 0.9000900 0.8600066
#Best mtry = 34
besmtry_rf <- tuneRF(x = trainWAPFLOOR[,-c(313)],
                     y = trainWAPFLOOR$FLOOR,
                     stepFactor = 2,
                     improve = TRUE,
                     trace = TRUE,
                     plot = T)

# Train a random forest mtry = 34
#463 seconds
system.time(rf_floor <- randomForest(x = trainWAPFLOOR[,-c(313)],
                                     y = trainWAPFLOOR$FLOOR,
                                     importance = TRUE,
                                     do.trace = TRUE,
                                     ntree = 100,
                                     mtry = 34))


# RF - FLOOR Prediction Model:
RFfloorpred <- predict(rf_floor, validationWAP2)

#Performance
perfRFfloorpred <- postResample(RFfloorpred, validationWAP2$FLOOR)

#Confusion Matrix
confusionMatrix(RFfloorpred, validationWAP2$FLOOR)

#### KNN
#197 seconds
# Accuracy     Kappa
# 0.7902790 0.7146276

#STANDARIZE parameters from the dataset
preprocessKNNfloor <- preProcess(trainWAPFLOOR[WAPSFLOOR],method = c("center", "scale"))
preprocessKNNfloorValidation <- preProcess(validationWAP2[WAPSFLOOR], method = c("center", "scale"))

standarizedfloorWAPStd <- predict(preprocessKNNfloor, trainWAPFLOOR[WAPSFLOOR])
standarizedfloorWAPSvd <- predict(preprocessKNNfloorValidation, validationWAP2[WAPSFLOOR])

standFLOORtd <- cbind(standarizedfloorWAPStd , BUILDINGID = trainWAPFLOOR$BUILDINGID, FLOOR = trainWAPFLOOR$FLOOR)
standFLOORvd <- cbind(standarizedfloorWAPSvd, BUILDINGID = validationWAP2$BUILDINGID, FLOOR = validationWAP2$FLOOR)

#Train KNN
system.time(knn_floor <- train.kknn(FLOOR~.,
                                    data = standFLOORtd,
                                    kernel = "optimal",
                                    kmax = 9,
                                    scale = FALSE))

# KNN- Building Prediction Model:
KNNfloorpred <- predict(knn_floor, standFLOORvd)

#Performance
perfKNNfloorpred <- postResample(KNNfloorpred, standFLOORvd$FLOOR)

#Confusion Matrix
confusionMatrix(KNNfloorpred, standFLOORvd$FLOOR)

##SVM
#208 seconds
# Accuracy     Kappa
# 0.8127813 0.7457957
system.time(svm_floor<- train(FLOOR~.,
                              data = standFLOORtd,
                              method = "svmLinear",
                              trControl = trainControl(verboseIter = TRUE)))

# SVM - Building Prediction Model:
SVMfloorpred <- predict(svm_floor, standFLOORvd)

#Performance
perfSVMfloorpred <- postResample(SVMfloorpred, standFLOORvd$FLOOR)

#Confusion Matrix
confusionMatrix(SVMfloorpred, standFLOORvd$FLOOR)

#SAVE MODEL - RANDOM FOREST SELECTED
save(RFfloorpred, file = "RF_PREDICTION_FLOOR.rda")

#### LOAD MODEL ####
load("RF_PREDICTION_FLOOR.rda")

#### 8.2.2 CREATE NEW VALIDATION DATA SET WITH PREDICTED FLOOR ####

RFfloorpred 
validationWAP3 <- validationWAP2
validationWAP3$LATITUDE <- validationWAP$LATITUDE
validationWAP3$FLOOR <- RFfloorpred
names(validationWAP3)
validationWAP3 <- validationWAP3[, c(1:312, 316, 313, 314, 315)]

### Create new TRAIN DATA FRAME for predicting FLOOR that included BUILDING and FLOOR
trainWAPLATITUDE <- trainWAP[,c(-313)]
names(trainWAPLATITUDE)

#Dummify Building and Floor variables
#Train
dummyBuild <- dummy(trainWAPLATITUDE$BUILDINGID, sep = "_")
trainWAPLATITUDE <- cbind(trainWAPLATITUDE, dummyBuild)
dummyFloor <- dummy(trainWAPLATITUDE$FLOOR, sep = "_")
trainWAPLATITUDE <- cbind(trainWAPLATITUDE, dummyFloor)
trainWAPLATITUDERF <- trainWAPLATITUDE[, c(1:316)]
trainWAPLATITUDEdummified <- trainWAPLATITUDE[, c(1:313, 316:324)]
trainWAPLATITUDEdummified$maxWAP <- as.numeric(trainWAPLATITUDEdummified$maxWAP)
names(trainWAPLATITUDERF)
names(trainWAPLATITUDEdummified)
#Validation

# dummyBuildvalidation <- NULL
# dummyFloorvalidation<- NULL
# validationWAP3dummy <- NULL
# validationwap3RF <- NULL
# validationWAP3dummified <- NULL
# 
dummyBuildvalidation <- dummy(validationWAP3$BUILDINGID, sep = "_")
validationWAP3 <- cbind(validationWAP3, dummyBuildvalidation)
dummyFloorvalidation <- dummy(validationWAP3$FLOOR, sep = "_")
validationWAP3 <- cbind(validationWAP3, dummyFloorvalidation)
names(validationWAP3)
validationwap3RF <- validationWAP3[, c(1:316)]
names(validationwap3RF)
validationWAP3dummified <- validationWAP3[, c(1:313, 316:324)]


#### 8.3 - LATITUDE ####

#Random Forest
names(trainWAPLATITUDERF)

#Best mtry = 105
besmtry_rf <- tuneRF(x = trainWAPLATITUDERF[, -c(313, 314)],
                     y = trainWAPLATITUDERF$LATITUDE,
                     stepFactor = 2, 
                     improve = TRUE,
                     trace = TRUE, 
                     plot = T)

# Train a random forest mtry = 104
#463 seconds
system.time(rf_latitude <- randomForest(x = trainWAPLATITUDERF[, -c(313, 314)],
                                        y = trainWAPLATITUDERF$LATITUDE,
                                        importance = TRUE, 
                                        do.trace = TRUE, 
                                        ntree = 150, 
                                        mtry = 104))


# RF - FLOOR Prediction Model:
RFlatitutepred <- predict(rf_latitude, validationwap3RF)

#Performance
perfRFlatitudepred <- postResample(RFlatitutepred, validationwap3RF$LATITUDE)

#### KNN

#STANDARIZE parameters from the dataset

preprocessLatitude <- preProcess(trainWAPLATITUDE[WAPS], method = "range")

standarizedlatitudeWAPStd <- predict(preprocessLatitude, trainWAPLATITUDE[WAPS])
standarizedlatitudeWAPSvd <- predict(preprocessLatitude, validationWAP3[WAPS])

standLATITUDEtd <- cbind(standarizedlatitudeWAPStd,
                         BUILDINGID00 = trainWAPLATITUDEdummified$BUILDINGID_0,
                         BUILDINGID01 = trainWAPLATITUDEdummified$BUILDINGID_1,
                         BUILDINGID02 = trainWAPLATITUDEdummified$BUILDINGID_2,
                         LATITUDE = trainWAPLATITUDEdummified$LATITUDE)

names(standLATITUDEvd)

standLATITUDEvd <- cbind(standarizedlatitudeWAPSvd, 
                         BUILDINGID00 = validationWAP3dummified$BUILDINGID_0,
                         BUILDINGID01 = validationWAP3dummified$BUILDINGID_1,
                         BUILDINGID02 = validationWAP3dummified$BUILDINGID_2,
                         LATITUDE = validationWAP3dummified$LATITUDE)

#Train KNN
system.time(knn_latitude <- train.kknn(LATITUDE~.,
                                       data = standLATITUDEtd,
                                       kernel = "optimal",
                                       kmax = 10,
                                       scale = FALSE))



# KNN- Building Prediction Model:
KNNlatitudepred <- predict(knn_latitude, standLATITUDEvd)

#Performance 
perfKNNlatitudepred <- postResample(KNNlatitudepred, standLATITUDEvd$LATITUDE)

#SAVE MODEL - RANDOM FOREST SELECTED
save(RFlatitutepred, file = "RF_PREDICTION_LATITUDE.rda")

#### LOAD MODEL ####
load("RF_PREDICTION_LATITUDE.rda")

#### 8.3.2 CREATE NEW VALIDATION DATA SET WITH PREDICTED LATITUDE ####

validationWAPLONGITUDE <- validationWAP3
validationWAPLONGITUDE$LONGITUDE <- validationWAP$LONGITUDE
validationWAPLONGITUDE$LATITUDE <- RFlatitutepred
names(validationWAPLONGITUDE)

### Create new TRAIN DATA FRAME for predicting LONGITUDE that includeS BUILDING, FLOOR AND LATITUDE

#Train

names(trainWAP)
trainWAPLONGITUDERF <- trainWAP
trainWAPLONGITUDEdummified <- trainWAPLATITUDEdummified
trainWAPLONGITUDEdummified$LONGITUDE <- trainWAP$LONGITUDE
names(trainWAPLONGITUDEdummified_noFloor)
trainWAPLONGITUDEdummified_noFloor <- trainWAPLONGITUDEdummified[, -c(318:322)]

#Validation

validationwapLONGITUDERF <- validationWAPLONGITUDE[, c(1:316, 325)]
names(validationWAPLONGITUDEdummified_nofloor)
validationWAPLONGITUDEdummified <- validationWAPLONGITUDE [, -c(314, 315)]
validationWAPLONGITUDEdummified_nofloor <- validationWAPLONGITUDEdummified[, -c(318:322)]

#### 8.4 - LONGITUDE ####

#Random Forest

#Best mtry = 105
besmtry_rf <- tuneRF(trainWAPLONGITUDERF[, -c(313, 315), 
                     trainWAPLONGITUDERF$LONGITUDE,
                     stepFactor = 2, 
                     improve = TRUE,
                     trace = TRUE, 
                     plot = T)

# Train a random forest mtry = 
names(trainWAPLONGITUDERF)
system.time(rf_longitude <- randomForest(x = trainWAPLONGITUDERF[, -c(313, 315)],
                                        y = trainWAPLONGITUDERF$LONGITUDE,
                                        importance = TRUE, 
                                        do.trace = TRUE, 
                                        ntree = 100, 
                                        mtry = 53))


# RF - LONGITUDE Prediction Model:
RFlongitudepred <- predict(rf_longitude, validationwapLONGITUDERF)

#Performance
perfRFlongitudepred <- postResample(RFlongitudepred, validationwapLONGITUDERF$LONGITUDE)
perfRFlongitudepred

#### KNN

#STANDARIZE parameters from the dataset


WAPSLONG <- grep("WAP", names(trainWAPLONGITUDEdummified), value = T)

preprocessLongitude <- preProcess(trainWAPLONGITUDEdummified[WAPSLONG], method = "range")

standarizedlongitudeWAPStd <- predict(preprocessLongitude, trainWAPLONGITUDEdummified[WAPSLONG])
standarizedlongitudeWAPSvd <- predict(preprocessLongitude, validationWAPLONGITUDEdummified[WAPSLONG])

standLONGITUDEtd <- cbind(standarizedlongitudeWAPStd,
                         BUILDINGID00 = trainWAPLONGITUDEdummified$BUILDINGID_0,
                         BUILDINGID01 = trainWAPLONGITUDEdummified$BUILDINGID_1,
                         BUILDINGID02 = trainWAPLONGITUDEdummified$BUILDINGID_2, 
                         LATITUDE = trainWAPLONGITUDEdummified$LATITUDE, 
                         LONGITUDE = trainWAPLONGITUDEdummified$LONGITUDE)

names(standLONGITUDEvd)

standLONGITUDEvd <- cbind(standarizedlongitudeWAPSvd, 
                         BUILDINGID00 = validationWAPLONGITUDEdummified$BUILDINGID_0,
                         BUILDINGID01 = validationWAPLONGITUDEdummified$BUILDINGID_1,
                         BUILDINGID02 = validationWAPLONGITUDEdummified$BUILDINGID_2,
                         LATITUDE = validationWAPLONGITUDEdummified$LATITUDE, 
                         LONGITUDE = validationWAPLONGITUDEdummified$LONGITUDE)

#Train KNN
system.time(knn_longitude <- train.kknn(LONGITUDE~.,
                                       data = standLONGITUDEtd,
                                       kernel = "optimal",
                                       kmax = 10,
                                       scale = FALSE))



# KNN- Building Prediction Model:
KNNlongitudepred <- predict(knn_longitude, standLONGITUDEvd)

#Performance 
perfKNNlongitudepred <- postResample(KNNlongitudepred, standLONGITUDEvd$LONGITUDE)
perfKNNlongitudepred


