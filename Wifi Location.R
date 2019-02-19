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
                doMC
)
####Parallel processing####
#did it work?

getDoParWorkers()
makeCluster(1)
registerDoMC(1)


#### 2- Load Data Sets ####

td <- read_csv("~/Desktop/Ubiqum/Data Analysis/RStudio/Wifi Location/UJIndoorLoc/trainingData.csv")
vd <- read_csv("~/Desktop/Ubiqum/Data Analysis/RStudio/Wifi Location/UJIndoorLoc/validationData.csv")


#### 2.1- Deleting WAPS that are useless (=100) ####

notworking_waps <- which(apply(td [,1:520], 2, function(x) mean(x)) == 100)
tdwaps <- td [,-c(notworking_waps)]


#### 3- Convert Variables #### 

tdwaps$FLOOR <- as.factor(td$FLOOR)
tdwaps$BUILDINGID <- as.factor(td$BUILDINGID)
tdwaps$SPACEID <- as.factor(td$SPACEID)
tdwaps$RELATIVEPOSITION <- as.factor(td$RELATIVEPOSITION)
tdwaps$USERID <- as.factor(td$USERID)
tdwaps$PHONEID <- as.factor(td$PHONEID)
tdwaps$TIMESTAMP <- as.POSIXct(as.numeric(td$TIMESTAMP), origin = '1970-01-01')


#### 4- Creating a new column w/ strongest WAP on the row ####

tdwaps[, 1:465] <- as.data.frame(apply(
  tdwaps[, 1:465], 2, function(x) {ifelse(x==100, -100,x)}))

tdwaps <- filter(tdwaps, USERID != 6)

WAP <- grep("WAP", names(tdwaps), value = T)

tdwaps$maxWAP <- apply(tdwaps[WAP], 1, which.max)
names(tdwaps)
# zerovariancewaps <- as.data.frame(apply(tdwaps [WAP], 1, 
#                           nearZeroVar, saveMetrics = TRUE))
# 
# wifidt <- tdwaps[, -zerovariancewaps] 
# I am pretty sure NearZeroVariance doesnt go this way

#### 5- Convert from wide to long for visualization ####

lttd <- melt(tdwaps, id.vars = c(466:475))
traincomplete <-melt(td, id.vars = c(521:529))
lttd <- filter(lttd, value != 100)
lttd <- filter(lttd, USERID != 6)
maxwapgrouped <- dplyr::group_by(tdwaps, maxWAP)

test <- melt(vd, id.vars = c(521:529))
test <- filter(test, value != 100)

#### 6- Plots and visualization ####

ggplot(lttd, aes(x=USERID, y= abs(value)))+
  geom_bar(stat = "identity", colour = "green")

ggplot(closeWAPS, aes(x=RELATIVEPOSITION, y= abs(value)))+
  geom_bar(stat = "identity", colour = "red")

ggplot(data = train) +
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

# Sampling n=30
sample1 <- tdwaps %>% group_by(BUILDINGID, FLOOR) %>% sample_n(30)

table(sample1$FLOOR)
table(sample1$BUILDINGID)

# Filter by building 
b0 <- filter(sample1, BUILDINGID == 0)
b1 <- dplyr::filter(sample1, BUILDINGID == 1)
b2 <- dplyr::filter(sample1, BUILDINGID == 2)

## Plot

sample1$BUILDINGID <- as.character(sample1$BUILDINGID)

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

plotsample1 <- plot_ly(sample1, x= ~LONGITUDE, 
                 y = ~LATITUDE, 
                 z = ~FLOOR, 
                 type = "scatter3d", 
                 mode = "markers", 
                 color = ~BUILDINGID)
plotsample1

plotb0 <- plot_ly(b0, 
                  x= ~LONGITUDE, 
                  y = ~LATITUDE, 
                  z = ~FLOOR, 
                  type = "scatter3d", 
                  mode = "markers", 
                  color = ~FLOOR)
plotb0



#### 8- Modeling ####

waps<- grep("WAP", names(sample1), value = T)

#### 8.1 FLOOR ####
# Get the best mtry / PREDICTING FLOOR
#Random Forest
besmtry_rf <- tuneRF(sample1[waps], 
                     sample1$FLOOR, 
                     ntreeTry = 100, 
                     stepFactor = 2, 
                     improve = 0.05,
                     trace = TRUE, 
                     plot = T)
#mtry = 42 	OOB error = 11.79% 
#0 0.05 

# Train a random forest using that mtry
system.time(rf1 <- randomForest(y=sample1$FLOOR, 
                                x=sample1[waps], 
                                importance = T, 
                                method ="rf", 
                                ntree = 100, 
                                mtry = 42))
#user  system elapsed 
#1.560   0.010   1.578 

# Train a random forest using caret package
system.time(rf1_caret <- train(y=sample1$LONGITUDE, 
                                      x= sample1[waps], 
                                      data = sample1, 
                                      method = "rf", 
                                      ntree = 100, 
                                      tuneGrid = expand.grid(.mtry = 42)))
#  user  system elapsed 
#9.698   0.283  10.041 

#KNN



#### 8.2 BUILDING ####
# Get the best mtry / PREDICTING Building 
besmtry_rf <- tuneRF(sample1[waps], 
                     sample1$BUILDINGID, 
                     ntreeTry = 100, 
                     stepFactor = 2, 
                     improve = 0.05,
                     trace = TRUE, 
                     plot = T)

#mtry = 42 	OOB error = 11.79% 
#0 0.05 

# Train a random forest using that mtry
system.time(rf1 <- randomForest(y=sample1$FLOOR, 
                                x=sample1[waps], 
                                importance = T, 
                                method ="rf", 
                                ntree = 100, 
                                mtry = 42))
#user  system elapsed 
#1.560   0.010   1.578 

# Train a random forest using caret package
system.time(rf1_caret <- train(y=sample1$LONGITUDE, 
                               x= sample1[waps], 
                               data = sample1, 
                               method = "rf", 
                               ntree = 100, 
                               tuneGrid = expand.grid(.mtry = 42)))
#  user  system elapsed 
#9.698   0.283  10.041 

