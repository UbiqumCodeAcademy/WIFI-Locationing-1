#### 1- Setting up the R environement ####

pacman:: p_load(pacman,
                readr,
                tidyr,
                reshape2, 
                dplyr, 
                ggplot2,
                ggmap
                )
#### 2- Load Data Sets ####

td <- read_csv("~/Desktop/Ubiqum/Data Analysis/RStudio/Wifi Location/UJIndoorLoc/trainingData.csv")
vd <- read_csv("~/Desktop/Ubiqum/Data Analysis/RStudio/Wifi Location/UJIndoorLoc/validationData.csv")

plot(td$LATITUDE, td$LONGITUDE)
plot(vd$LATITUDE, vd$LONGITUDE)

#### 2.1- Deleting WAPS that are useless (=100) ####

notworking_waps <- which(apply(td [,1:520], 2, function(x) mean(x)) == 100)
working_waps <- td [,-c(notworking_waps)]

apply(working_waps [, 1:465], 2, FUN = mean)


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
train <- train[,c(10, 11, 1, 2, 3, 4, 5, 6, 7, 8, 9)]
train <- filter(train, value != 100)
summary(train)

test <- melt(vd, id.vars = c(521:529))
test <-  test[,c(10, 11, 1, 2, 3, 4, 5, 6, 7, 8, 9)]
test <- filter(test, value != 100)

### So, where are the actual routers? ###
closeWAPS <- filter(train, value > -67)
plot(closeWAPS$LONGITUDE, closeWAPS$LATITUDE)

#### 5- Separate Strong Signal WorkingWAPs data by building ####
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

#### 5.1- Break it dowm per floor (0,1,2,3,4) ####

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
#Aqui me sale entonces que no hay WAPs, pero hago un list y me dice que si...


#### 5- Separate WorkingWAPs data by building ####
#BL 0
bld0 <- filter(working_waps, BUILDINGID == 0)

#BL 1
bld1 <- filter(working_waps, BUILDINGID == 1)

#BL2
bld2 <- filter(working_waps, BUILDINGID == 2)

##PLOTS

plot(bld0$LATITUDE, bld0$LONGITUDE)
plot(bld1$LATITUDE, bld1$LONGITUDE)
plot(bld2$LATITUDE, bld2$LONGITUDE)

#### 5.1- Break it dowm per floor (0,1,2,3,4) ####

#Floor 0
bld0_fl0 <- filter(bld0, FLOOR == 0)

#Floor 1
bld0_fl1 <- filter(bld0, FLOOR == 1)
plot(bld0_fl1$LATITUDE, bld0_fl1$LONGITUDE)

#Floor 2
bld0_fl2 <- filter(bld0, FLOOR == 2)
plot(bld0_fl2$LATITUDE, bld0_fl2$LONGITUDE)

#Floor 3
bld0_fl3 <- filter(bld0, FLOOR == 3)
plot(bld0_fl3$LATITUDE, bld0_fl3$LONGITUDE)

#Floor 4
bld0_fl4 <- filter(bld0, FLOOR == 4)
plot(bld0_fl4$LATITUDE, bld0_fl4$LONGITUDE) 
#Aqui me sale entonces que no hay WAPs, pero hago un list y me dice que si...


#### 6- Create a data frame for each feature PER building (WORKING WAPS) ####

#Building 0
bld0_lat <- data.frame(bld0$LATITUDE, bld0[,1:465])
bld0_lon <- data.frame(bld0$LONGITUDE, bld0[, 1:465])
bld0_floor <- data.frame(bld0$FLOOR, bld0[, 1:465])

#Building 1
bld1_lat <- data.frame(bld1$LATITUDE, bld1[,1:465])
bld1_lon <- data.frame(bld1$LONGITUDE, bld1[, 1:465])
bld1_floor <- data.frame(bld1$FLOOR, bld1[, 1:465])

#Building 2
bld2_lat <- data.frame(bld2$LATITUDE, bld2[,1:465])
bld2_lon <- data.frame(bld2$LONGITUDE, bld2[, 1:465])
bld2_floor <- data.frame(bld2$FLOOR, bld2[, 1:465])


ggplot(closeWAPS, aes(x=USERID, y=abs(value)))+
  geom_bar(stat = "identity", colour= "blue")

ggplot(closeWAPS, aes(x=PHONEID, y= abs(value)))+
  geom_bar(stat = "identity", colour = "green")

ggplot(closeWAPS, aes(x=RELATIVEPOSITION, y= abs(value)))+
  geom_bar(stat = "identity", colour = "red")

closeWAPS_inside <- filter(closeWAPS, RELATIVEPOSITION==1)
plot(closeWAPS_inside$LATITUDE, closeWAPS_inside$LONGITUDE)

insidewaps <- filter(train, RELATIVEPOSITION==1)
plot(insidewaps$LATITUDE, insidewaps$LONGITUDE) 


ggplot(closewaps_bld0_fl0, aes(x=LATITUDE, y= LONGITUDE))+
  geom_point(colour= "red")

list(closeWAPS$PHONEID)

