#### 1- Setting up the R environement ####

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
                kknn
)
####Parallel processing####

registerDoMC(cores=4)

#### 2- Load Data Sets ####

td <- read_csv("~/Desktop/Ubiqum/Data Analysis/RStudio/Wifi Location/UJIndoorLoc/trainingData.csv")
vd <- read_csv("~/Desktop/Ubiqum/Data Analysis/RStudio/Wifi Location/UJIndoorLoc/validationData.csv")


#### 2.1- Deleting WAPS that are useless (=100) ####

#Test
notworking_waps <- which(apply(td [,1:520], 2, function(x) mean(x)) == 100)
notworking_row <- which(apply(td [,1:520], 1, function(x) mean(x)) == 100)
tdwaps <- td [-c(notworking_row), -c(notworking_waps)]

#Validation
validation_notworking_waps <- which(apply(vd [,1:520], 2, function(x) mean(x)) == 100)
vdwaps <- vd [, -c(validation_notworking_waps)]

#### 3- Convert Variables #### 

#Test
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
  tdwaps[, 1:465], 2, function(x) {ifelse(x==100, -100,x)}))

tdwaps <- filter(tdwaps, USERID != 6)

WAP <- grep("WAP", names(tdwaps), value = T)

tdwaps$maxWAP <- apply(tdwaps[WAP], 1, which.max)

# zerovariancewaps <- as.data.frame(apply(tdwaps [WAP], 1, 
#                           nearZeroVar, saveMetrics = TRUE))
# 
# wifidt <- tdwaps[, -zerovariancewaps] 
# I am pretty sure NearZeroVariance doesnt go this way

#Validation
vdwaps[,1:367] <- as.data.frame(apply(vdwaps[, 1:367], 2, function(x) {ifelse(x==100, -100,x)}))
WAPvd <- grep("WAP", names(vdwaps), value = T)
vdwaps$maxWAP <- apply(vdwaps[WAPvd], 1, which.max)


## Intercept
trainwaps <- tdwaps[, 1:465]
validationwaps <- vdwaps[, 1:367]
traininfo <- tdwaps[, 466:475]
validationinfo <- vdwaps[, 368:377]

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
#### 5- Convert from wide to long for visualization ####

#Test
lttd <- melt(tdwaps, id.vars = c(466:475))
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

#Floor 0
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
sample60 <- tdwaps %>% group_by(BUILDINGID, FLOOR) %>% sample_n(60)
# Visualization
table(sample60$FLOOR)
table(sample60$BUILDINGID)
 
# Filter by building 
sample60_b0 <- filter(sample60, BUILDINGID == 0)
sample60_b1 <- dplyr::filter(sample60, BUILDINGID == 1)
sample60_b2 <- dplyr::filter(sample60, BUILDINGID == 2)

## Ploting

a <- htmltools::tagList()

for (i in unique(sample1$BUILDINGID)) {
  a[[i]] <- sample1 %>% dplyr::filter(BUILDINGID == i)
  %>% plot_ly(sample1, 
              x = ~ LONGITUDE, 
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

WAPS <- grep("WAP", names(trainWAP), value = T)
set.seed(123)

#### 8.1 BUILDING####
# Get the best mtry / PREDICTING BUILDING
#Random Forest
#Accuracy    Kappa 
#1        1
#Best mtry = 34
besmtry_rf <- tuneRF(trainWAP[WAPS], 
                     trainWAP$BUILDINGID,
                     stepFactor = 2, 
                     improve = TRUE,
                     trace = TRUE, 
                     plot = T)

# Train a random forest mtry = 34 
#142 seconds
system.time(rf_building <- randomForest(BUILDINGID~., 
                                data = trainWAP, 
                                importance = TRUE, 
                                do.trace = TRUE, 
                                ntree = 100, 
                                mtry = 34))


# # Train a random forest using caret package - TOO LONG
# system.time(rf_caret_building <- train(y=trainWAP$BUILDINGID, 
#                                        x=trainWAP[WAPS], 
#                                        method ="rf", 
#                                        ntree = 100, 
#                                        tuneGrid = expand.grid (.mtry = 34)))


# RF - Building Prediction Model:
RFbuildingpred <- predict(rf_building, validationWAP)

#Performance
perfRFbuildingpred <- postResample(RFbuildingpred, validationWAP$BUILDINGID)
#Confusion Matrix
confusionMatrix(RFbuildingpred, validationWAP$BUILDINGID)

#### KNN 
# Accuracy     Kappa 
#0.9945995 0.9914785 
#calculate the pre-process parameters from the dataset
preprocessParam <- preProcess(trainWAP[WAPS], 
                              method = c("center", "scale"))

#Train KNN 
system.time(knn_building <- train.kknn(BUILDINGID~., 
                                       data = trainWAP,
                                       kernel = "optimal",
                                       kmax = 10,
                                       scale = FALSE))

# RF - Building Prediction Model:
KNNbuildingpred <- predict(knn_building, validationWAP)

#Performance
perfKNNbuildingpred <- postResample(KNNbuildingpred, validationWAP$BUILDINGID)

#Confusion Matrix
confusionMatrix(KNNbuildingpred, validationWAP$BUILDINGID)
#Dice que tiene que ser ambos del mismo tamanño

##SVM
# system.time(svm_building <- svm())





