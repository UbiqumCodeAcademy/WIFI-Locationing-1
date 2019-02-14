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
                cowplot
)
#### 2- Load Data Sets ####

td <- read_csv("~/Desktop/Ubiqum/Data Analysis/RStudio/Wifi Location/UJIndoorLoc/trainingData.csv")
vd <- read_csv("~/Desktop/Ubiqum/Data Analysis/RStudio/Wifi Location/UJIndoorLoc/validationData.csv")


#### 2.1- Deleting WAPS that are useless (=100) ####

notworking_waps <- which(apply(td [,1:520], 2, function(x) mean(x)) == 100)
tdwaps <- td [,-c(notworking_waps)]

tdwaps[, 1:465] <- as.data.frame(apply(tdwaps[, 1:465], 2, function(x) {ifelse(x==100, -104,x)}))

WAP <- grep("WAP", names(tdwaps), value = T)

tdwaps$maxWAP <- apply(tdwaps[WAP], 1, which.max)

list(tdwaps$maxWAP)

summary(tdwaps$maxWAP)  


#### 3- Convert Variables #### 

td$FLOOR <- as.factor(td$FLOOR)
td$BUILDINGID <- as.factor(td$BUILDINGID)
td$SPACEID <- as.factor(td$SPACEID)
td$RELATIVEPOSITION <- as.factor(td$RELATIVEPOSITION)
td$USERID <- as.factor(td$USERID)
td$PHONEID <- as.factor(td$PHONEID)
td$TIMESTAMP <- as.POSIXct(as.numeric(td$TIMESTAMP), origin = '1970-01-01')

#### 4- Convert from wide to long ####

train <- melt(td, id.vars = c(521:529))
traincomplete <-melt(td, id.vars = c(521:529))
train <- train[,c(10, 11, 1, 2, 3, 4, 5, 6, 7, 8, 9)]
train <- filter(train, value != 100)
summary(train)

test <- melt(vd, id.vars = c(521:529))
test <-  test[,c(10, 11, 1, 2, 3, 4, 5, 6, 7, 8, 9)]
test <- filter(test, value != 100)


#### 6- Plots ####

ggplot(td, aes(x=USERID, y= (apply(td [,1:520], 2, function(x) sum(x)))))+
  geom_bar(stat = "identity", colour= "blue")

ggplot(closeWAPS, aes(x=PHONEID, y= abs(value)))+
  geom_bar(stat = "identity", colour = "green")

ggplot(closeWAPS, aes(x=RELATIVEPOSITION, y= abs(value)))+
  geom_bar(stat = "identity", colour = "red")


plot1 <- plot_ly(train, x= ~LATITUDE, 
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

plot1

plot2 <- plot_ly(closeWAPS, x= ~LATITUDE, 
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


plot(LATITUDE ~ LONGITUDE, data = closeWAPS, pch = 20, col = "cyan")

#### 8- Sampling ####

sample1 <- td %>% group_by(BUILDINGID, FLOOR) %>% sample_n(30)

table(sample1$FLOOR)
table(sample1$BUILDINGID)

b0 <- filter(sample1, BUILDINGID == 0)

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

wap1s <- grep("WAP", names(sample1), value = T)

# Get the best mtry
besmtry_rf <- tuneRF(sample1[wap1s], 
                     sample1$LONGITUDE, 
                     ntreeTry = 100, 
                     stepFactor = 2, 
                     improve = 0.05,
                     trace = TRUE, 
                     plot = T)

# Train a random forest using that mtry
system.time(rf1 <- randomForest(y=sample1$LONGITUDE, 
                                x=sample1[wap1s], 
                                importance = T, 
                                method ="rf", 
                                ntree = 100, 
                                mtry = 44))

# Train a random forest using caret package
system.time(rf1_caret <- train(y=sample1$LONGITUDE, 
                                      x= sample1[wap1s], 
                                      data = sample1, 
                                      method = "rf", 
                                      ntree = 100, 
                                      tuneGrid = expand.grid(.mtry = 44)))


