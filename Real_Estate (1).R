require(readr)
library(dplyr)
library(ggplot2)

train <- read_csv("Property_Price_Train.csv")
test <- read_csv("Property_Price_Test.csv")

#check train dataset
glimpse(train)
str(train)
summary(train)
colSums(is.na(train))
sapply(train, n_distinct)
sum(duplicated(train[,-1]))

#check test dataset
glimpse(test)
str(test)
summary(test)
colSums(is.na(test))
sapply(test, n_distinct)
sum(duplicated(test[,-1]))


#lets combine the dataset
df <- bind_rows(train, test)

#check df dataset
glimpse(df)
summary(df)
colSums(is.na(df))
sum(is.na(df$Lane_Type)) / nrow(df) * 100
sum(is.na(df$Pool_Quality)) / nrow(df) * 100
sum(is.na(df$Fence_Quality)) / nrow(df) * 100
sum(is.na(df$Miscellaneous_Feature)) / nrow(df) * 100
sapply(df, n_distinct)
sum(duplicated(df[,-1]))

str(df)


#lets start with EDA
#also, will convert all numerical data types which are categorical but are in numeric format.

##Building Class
table(df$Building_Class)
str(df$Building_Class)
#the building class has categories divided, hence it is categorical and not numeric.
df$Building_Class <- as.character(df$Building_Class)
df[which(is.na(df$Building_Class)),]
#plotting
ggplot(df, aes(x = Building_Class)) + geom_bar(fill = 'red')

##Zoning Class
table(df$Zoning_Class)
str(df$Zoning_Class)
df[which(is.na(df$Zoning_Class)),] #4 NA values
df$Zoning_Class[is.na(df$Zoning_Class)] <- "RLD"
#plotting
ggplot(df, aes(x = Zoning_Class)) + geom_bar(fill = 'red')

##Lot_Extend
table(df$Lot_Extent)
#clearly Lot_extend do not seem to be categorical.
summary(df$Lot_Extent) #486 NA values
sum(is.na(df$Lot_Extent)) / nrow(df) * 100
#replacing by mean, as not much of difference between mean and median
df$Lot_Extent[is.na(df$Lot_Extent)] <- 68
#outliers and plotting
quantile(df$Lot_Extent, seq(0, 1, 0.01), na.rm = T)
plot(quantile(df$Lot_Extent, seq(0, 1, 0.01), na.rm = T)) #we can see there is a sudden rise
quantile(df$Lot_Extent, seq(0.99, 1, 0.001), na.rm = T)
df$Lot_Extent[which(df$Lot_Extent > 140.00)] <- 140.00

##Lot_Size
table(df$Lot_Size)
summary(df$Lot_Size) #0 NA values
boxplot(df$Lot_Size)
quantile(df$Lot_Size, seq(0, 1, 0.01), na.rm = T)
plot(quantile(df$Lot_Size, seq(0, 1, 0.01), na.rm = T)) #there is outliers
quantile(df$Lot_Size, seq(0, 0.01, 0.0001), na.rm = T)
df$Lot_Size[which(df$Lot_Size > 58310.04)] <- 58310.04
df$Lot_Size[which(df$Lot_Size < 41.03736)] <- 41.03736

##Road Type
table(df$Road_Type)
df[which(is.na(df$Road_Type)),] #0 NA value
#plotting
ggplot(df, aes(x = Road_Type)) + geom_bar(fill = 'red')

##Lane_Type
table(df$Lane_Type)
df[which(is.na(df$Lane_Type)),] #2720 NA value
df$Lane_Type[is.na(df$Lane_Type)] <- "NA"
#plotting
ggplot(df, aes(x = Lane_Type)) + geom_bar(fill = 'red')

##Property_Shape
table(df$Property_Shape)
df[which(is.na(df$Property_Shape)),] #0 NA value
#plotting
ggplot(df, aes(x = Property_Shape)) + geom_bar(fill = 'red')

##Land_Outline
table(df$Land_Outline)
df[which(is.na(df$Land_Outline)),] #0 NA value
#plotting
ggplot(df, aes(x = Land_Outline)) + geom_bar(fill = 'red')

##Utility_Type
table(df$Utility_Type)
df[which(is.na(df$Utility_Type)),] #2 NA value
df$Utility_Type[is.na(df$Utility_Type)] <- "AllPub"
#plotting
ggplot(df, aes(x = Utility_Type)) + geom_bar(fill = 'red')

##Lot_Configuration
table(df$Lot_Configuration)
df[which(is.na(df$Lot_Configuration)),] #0 NA value
#plotting
ggplot(df, aes(x = Lot_Configuration)) + geom_bar(fill = 'red')

##PropertySlope
table(df$Property_Slope)
df[which(is.na(df$Property_Slope)),] #0 NA value
#plotting
ggplot(df, aes(x = Property_Slope)) + geom_bar(fill = 'red')

##Neighborhood
table(df$Neighborhood)
df[which(is.na(df$Neighborhood)),] #0 NA value
#plotting
ggplot(df, aes(x = Neighborhood)) + geom_bar(fill = 'red')

##Condition1
table(df$Condition1)
df[which(is.na(df$Condition1)),] #0 NA value
#plotting
ggplot(df, aes(x = Condition1)) + geom_bar(fill = 'red')

##Condition2
table(df$Condition2)
df[which(is.na(df$Condition2)),] #0 NA value
#plotting
ggplot(df, aes(x = Condition2)) + geom_bar(fill = 'red')

##House_Type
table(df$House_Type)
df[which(is.na(df$House_Type)),] #0 NA value
#plotting
ggplot(df, aes(x = House_Type)) + geom_bar(fill = 'red')

##House_design
table(df$House_Design)
df[which(is.na(df$House_Design)),] #0 NA value
#plotting
ggplot(df, aes(x = House_Design)) + geom_bar(fill = 'red')

##Overall_Material
table(df$Overall_Material)
str(df$Overall_Material) #overall material is in numeric but should categorical
df$Overall_Material <- as.character(df$Overall_Material) # convert to character type
df[which(is.na(df$Overall_Material)),] #0 NA value
#plotting
ggplot(df, aes(x = House_Design)) + geom_bar(fill = 'red')

##House_Condition
table(df$House_Condition)
df[which(is.na(df$House_Condition)),] #0 NA value
str(df$House_Condition)
df$House_Condition <- as.character(df$House_Condition)
#plotting
ggplot(df, aes(x = House_Condition)) + geom_bar(fill = 'red')

##Construction year
table(df$Construction_Year)
df[which(is.na(df$Construction_Year)),] #0 NA value
str(df$Construction_Year) #should be character as it in year
df$Construction_Year <- as.character(df$Construction_Year)
#plotting
ggplot(df, aes(x = Construction_Year)) + geom_bar(fill = 'red')

##Remodeled year
table(df$Remodel_Year)
df[which(is.na(df$Remodel_Year)),] #0 NA value
str(df$Remodel_Year) #should be character as it in year
df$Remodel_Year <- as.character(df$Remodel_Year)
#plotting
ggplot(df, aes(x = Remodel_Year)) + geom_bar(fill = 'red')

##Roof Design
table(df$Roof_Design)
df[which(is.na(df$Roof_Design)),] #0 NA value
#plotting
ggplot(df, aes(x = Roof_Design)) + geom_bar(fill = 'red')

##Roof Quality
table(df$Roof_Quality)
df[which(is.na(df$Roof_Quality)),] #0 NA value
#plotting
ggplot(df, aes(x = Roof_Quality)) + geom_bar(fill = 'red')

##Exterior1st
table(df$Exterior1st)
df[which(is.na(df$Exterior1st)),] #1 NA value
df$Exterior1st[is.na(df$Exterior1st)] <- "VinylSd"
#plotting
ggplot(df, aes(x = Exterior1st)) + geom_bar(fill = 'red')

##Exterior2nd
table(df$Exterior2nd)
df[which(is.na(df$Exterior2nd)),] #1 NA value
df$Exterior2nd[is.na(df$Exterior2nd)] <- "VinylSd"
#plotting
ggplot(df, aes(x = Exterior2nd)) + geom_bar(fill = 'red')

##Brick_Veneer_Type
table(df$Brick_Veneer_Type)
df[which(is.na(df$Brick_Veneer_Type)),] #24 NA values
df$Brick_Veneer_Type[is.na(df$Brick_Veneer_Type)] <- "None"
#plotting
ggplot(df, aes(x = Brick_Veneer_Type)) + geom_bar(fill = 'red')

##Brick_Veneer_Area
table(df$Brick_Veneer_Area)
summary(df$Brick_Veneer_Area)
df$Brick_Veneer_Area[is.na(df$Brick_Veneer_Area)] <- 0 #replaced na with 0 as median is 0
boxplot(df$Brick_Veneer_Area)
quantile(df$Brick_Veneer_Area, seq(0, 1, 0.01), na.rm = T)
plot(quantile(df$Brick_Veneer_Area, seq(0, 1, 0.01), na.rm = T))
quantile(df$Brick_Veneer_Area, seq(0.99, 1, 0.001), na.rm = T)
df$Brick_Veneer_Area[which(df$Brick_Veneer_Area > 793.976)] <- 793.976

##Exterior_Material
table(df$Exterior_Material)
df[which(is.na(df$Exterior_Material)),] #0 NA values
#plotting
ggplot(df, aes(x = Exterior_Material)) + geom_bar(fill = 'red')

##Exterior_Condition
table(df$Exterior_Condition)
df[which(is.na(df$Exterior_Condition)),] #0 NA values
#plotting
ggplot(df, aes(x = Exterior_Condition)) + geom_bar(fill = 'red')

##Foundation_Type
table(df$Foundation_Type)
df[which(is.na(df$Foundation_Type)),] #0 NA values
#plotting
ggplot(df, aes(x = Foundation_Type)) + geom_bar(fill = 'red')

##BAsement_Height
table(df$Basement_Height)
df[which(is.na(df$Basement_Height)),] #81 NA values
df$Basement_Height[is.na(df$Basement_Height)] <- "NA"
#plotting
ggplot(df, aes(x = Basement_Height)) + geom_bar(fill = 'red')

##Basment_Condition
table(df$Basement_Condition)
df[which(is.na(df$Basement_Condition)),] #82 NA values
df$Basement_Condition[is.na(df$Basement_Condition) & 
                        df$Basement_Height == 'NA'] <- "NA" #still there are NA's
df$Basement_Condition[is.na(df$Basement_Condition)] <- "TA" #0 NA
#plotting
ggplot(df, aes(x = Basement_Condition)) + geom_bar(fill = 'red')

##Exposure_Level
table(df$Exposure_Level)
df[which(is.na(df$Exposure_Level)),] #79 NA values
df$Exposure_Level[is.na(df$Exposure_Level) & 
                        df$Basement_Height == 'NA'] <- "NA" #still there are NA's
df$Exposure_Level[is.na(df$Exposure_Level)] <- "No" #0 NA
#plotting
ggplot(df, aes(x = Exposure_Level)) + geom_bar(fill = 'red')

##BstFinType1
table(df$BsmtFinType1)
df[which(is.na(df$BsmtFinType1)),] #82 NA values
df$BsmtFinType1[is.na(df$BsmtFinType1) & 
                    df$Basement_Height == 'NA'] <- "NA" #0 NA value
#plotting
ggplot(df, aes(x = BsmtFinType1)) + geom_bar(fill = 'red')

##BsmtFinSF1
table(df$BsmtFinSF1)
summary(df$BsmtFinSF1)
df$BsmtFinSF1[is.na(df$BsmtFinSF1)] <- 368
boxplot(df$BsmtFinSF1)
quantile(df$BsmtFinSF1, seq(0, 1, 0.01), na.rm = T)
plot(quantile(df$BsmtFinSF1, seq(0, 1, 0.01), na.rm = T))
quantile(df$BsmtFinSF1, seq(0.99, 1, 0.001), na.rm = T)
df$BsmtFinSF1[which(df$BsmtFinSF1 > 1656.458)] <- 1656.458

##BsmtFinType2
table(df$BsmtFinType2)
df[which(is.na(df$BsmtFinType2)),] #80 NA values
df$BsmtFinType2[is.na(df$BsmtFinType2) & 
                  df$Basement_Height == 'NA'] <- "NA" #1 NA value left
df$BsmtFinType2[is.na(df$BsmtFinType2)] <- "NA"
#plotting
ggplot(df, aes(x = BsmtFinType2)) + geom_bar(fill = 'red')

##BsmtFinSF2
table(df$BsmtFinSF2)
summary(df$BsmtFinSF2)
df$BsmtFinSF2[is.na(df$BsmtFinSF2)] <- 0
boxplot(df$BsmtFinSF2)
quantile(df$BsmtFinSF2, seq(0, 1, 0.01), na.rm = T)
plot(quantile(df$BsmtFinSF2, seq(0, 1, 0.01), na.rm = T))
quantile(df$BsmtFinSF2, seq(0.99, 1, 0.001), na.rm = T)
df$BsmtFinSF2[which(df$BsmtFinSF2 > 1058.328)] <- 1058.328

##BsmtUnfSF
table(df$BsmtUnfSF)
summary(df$BsmtUnfSF)
df$BsmtUnfSF[is.na(df$BsmtUnfSF)] <- 467
boxplot(df$BsmtUnfSF)
quantile(df$BsmtUnfSF, seq(0, 1, 0.01), na.rm = T)
plot(quantile(df$BsmtUnfSF, seq(0, 1, 0.01), na.rm = T))
quantile(df$BsmtUnfSF, seq(0.9, 1, 0.01), na.rm = T)
df$BsmtUnfSF[which(df$BsmtUnfSF > 1776.49)] <- 1776.49

##Total_Basement_Area
table(df$Total_Basement_Area)
summary(df$Total_Basement_Area)
df$Total_Basement_Area[is.na(df$Total_Basement_Area)] <- 989
boxplot(df$Total_Basement_Area)
quantile(df$Total_Basement_Area, seq(0, 1, 0.01), na.rm = T)
plot(quantile(df$Total_Basement_Area, seq(0, 1, 0.01), na.rm = T))
quantile(df$Total_Basement_Area, seq(0.9, 1, 0.01), na.rm = T)
df$Total_Basement_Area[which(df$Total_Basement_Area > 2198.30)] <- 2198.30

##Heating Type
table(df$Heating_Type)
df[which(is.na(df$Heating_Type)),] #0 NA values
#plotting
ggplot(df, aes(x = Heating_Type)) + geom_bar(fill = 'red')

##Heating Quality
table(df$Heating_Quality)
df[which(is.na(df$Heating_Quality)),] #0 NA values
#plotting
ggplot(df, aes(x = Heating_Quality)) + geom_bar(fill = 'red')

##Air_Conditioning
table(df$Air_Conditioning)
df[which(is.na(df$Air_Conditioning)),] #0 NA values
#plotting
ggplot(df, aes(x = Air_Conditioning)) + geom_bar(fill = 'red')

##Electrical_System
table(df$Electrical_System)
df[which(is.na(df$Electrical_System)),] #1 NA values
df$Electrical_System[is.na(df$Electrical_System)] <- "SBrkr"
#plotting
ggplot(df, aes(x = Electrical_System)) + geom_bar(fill = 'red')

##First_Floor_Area
table(df$First_Floor_Area)
summary(df$First_Floor_Area) #o NA values
boxplot(df$First_Floor_Area)
quantile(df$First_Floor_Area, seq(0, 1, 0.01), na.rm = T)
plot(quantile(df$First_Floor_Area, seq(0, 1, 0.01), na.rm = T))
quantile(df$First_Floor_Area, seq(0.99, 1, 0.001), na.rm = T)
df$First_Floor_Area[which(df$First_Floor_Area > 2434.782)] <- 2434.782

##Second_Floor_Area
table(df$Second_Floor_Area)
summary(df$Second_Floor_Area) #o NA values
boxplot(df$Second_Floor_Area)
quantile(df$Second_Floor_Area, seq(0, 1, 0.01), na.rm = T)
plot(quantile(df$Second_Floor_Area, seq(0, 1, 0.01), na.rm = T))
quantile(df$Second_Floor_Area, seq(0.99, 1, 0.001), na.rm = T)
df$Second_Floor_Area[which(df$Second_Floor_Area > 1838.158)] <- 1838.158

##LowQualFinSf
table(df$LowQualFinSF)
summary(df$LowQualFinSF)
boxplot(df$LowQualFinSF)
quantile(df$LowQualFinSF, seq(0,1,0.01), na.rm = T)
plot(quantile(df$LowQualFinSF, seq(0,1,0.01), na.rm = T))
quantile(df$LowQualFinSF, seq(0.99,1,0.001), na.rm = T)
df$LowQualFinSF[which(df$LowQualFinSF > 513.166)] <- 513.166

##Grade_Living_Area
table(df$Grade_Living_Area)
summary(df$Grade_Living_Area)
boxplot(df$Grade_Living_Area)
quantile(df$Grade_Living_Area, seq(0,1,0.01), na.rm = T)
plot(quantile(df$Grade_Living_Area, seq(0,1,0.01), na.rm = T))
df$Grade_Living_Area[which(df$Grade_Living_Area > 2936.18)] <- 2936.18

##Underground full bathroom
table(df$Underground_Full_Bathroom)
str(df$Underground_Full_Bathroom) #should categorical not numeric
df$Underground_Full_Bathroom <- as.character(df$Underground_Full_Bathroom)
df[which(is.na(df$Underground_Full_Bathroom)),] #2 NA values
df$Underground_Full_Bathroom[is.na(df$Underground_Full_Bathroom)] <- "0"
#plotting
ggplot(df, aes(x = Underground_Full_Bathroom)) + geom_bar(fill = 'red')

##Underground_Half_Bathroom
table(df$Underground_Half_Bathroom)
str(df$Underground_Half_Bathroom) #should categorical not numeric
df$Underground_Half_Bathroom <- as.character(df$Underground_Half_Bathroom)
df[which(is.na(df$Underground_Half_Bathroom)),] #2 NA values
df$Underground_Half_Bathroom[is.na(df$Underground_Half_Bathroom)] <- "0"
#plotting
ggplot(df, aes(x = Underground_Half_Bathroom)) + geom_bar(fill = 'red')

##Full_Bathroom_Above_Grade
table(df$Full_Bathroom_Above_Grade)
str(df$Full_Bathroom_Above_Grade)
df$Full_Bathroom_Above_Grade <- as.character(df$Full_Bathroom_Above_Grade)
df[which(is.na(df$Full_Bathroom_Above_Grade)),] #0 NA value
#Plotting
ggplot(df, aes(x = Full_Bathroom_Above_Grade)) + geom_bar(fill = 'red')

##Half_Bathroom_Above_Grade
table(df$Half_Bathroom_Above_Grade)
str(df$Half_Bathroom_Above_Grade)
df$Half_Bathroom_Above_Grade <- as.character(df$Half_Bathroom_Above_Grade)
df[which(is.na(df$Half_Bathroom_Above_Grade)),] #0 NA value
#plotting
ggplot(df, aes(x = Half_Bathroom_Above_Grade)) + geom_bar(fill = 'red')

##Bedroom
table(df$Bedroom_Above_Grade)
str(df$Bedroom_Above_Grade)
df$Bedroom_Above_Grade <- as.character(df$Bedroom_Above_Grade)
df[which(is.na(df$Bedroom_Above_Grade)),] #0 NA value
#plotting
ggplot(df, aes(x = Bedroom_Above_Grade)) + geom_bar(fill = 'red')

##Kitchens_Above_Grade
table(df$Kitchen_Above_Grade)
str(df$Kitchen_Above_Grade)
df$Kitchen_Above_Grade <- as.character(df$Kitchen_Above_Grade)
df[which(is.na(df$Kitchen_Above_Grade)),] #0 NA value
#plotting
ggplot(df, aes(x = Kitchen_Above_Grade)) + geom_bar(fill = 'red')

##Kitchen_Quality
table(df$Kitchen_Quality)
df[which(is.na(df$Kitchen_Quality)),] #1 NA values
df$Kitchen_Quality[is.na(df$Kitchen_Quality)] <- "TA"
#plotting
ggplot(df, aes(x = Kitchen_Quality)) + geom_bar(fill = 'red')

##Rooms Above Grade
table(df$Rooms_Above_Grade)
str(df$Rooms_Above_Grade)
df$Rooms_Above_Grade <- as.character(df$Rooms_Above_Grade)
df[which(is.na(df$Rooms_Above_Grade)),] #0 NA value
#plotting
ggplot(df, aes(x = Rooms_Above_Grade)) + geom_bar(fill = 'red')

##Functional_Rates
table(df$Functional_Rate)
df[which(is.na(df$Functional_Rate)),] #2 NA values
df$Functional_Rate[is.na(df$Functional_Rate)] <- "TF"
#plotting
ggplot(df, aes(x = Functional_Rate)) + geom_bar(fill = 'red')

##Fireplaces
table(df$Fireplaces)
str(df$Fireplaces)
df$Fireplaces <- as.character(df$Fireplaces)
df[which(is.na(df$Fireplaces)),] #0 NA value
#plotting
ggplot(df, aes(x = Fireplaces)) + geom_bar(fill = 'red')

##Fireplace_Quality
table(df$Fireplace_Quality)
df[which(is.na(df$Fireplace_Quality)),] #1419 NA values
df$Fireplace_Quality[is.na(df$Fireplace_Quality)] <- "NA" #taking NA as No Firepplace
#plotting
ggplot(df, aes(x = Fireplace_Quality)) + geom_bar(fill = 'red')

##Garage
table(df$Garage)
df[which(is.na(df$Garage)),] #157 NA values
df$Garage[is.na(df$Garage)] <- "NA" #taking NA as No Garage
#plotting
ggplot(df, aes(x = Garage)) + geom_bar(fill = 'red')

##Garage Built Year
table(df$Garage_Built_Year)
df[which(is.na(df$Garage_Built_Year)),] #159 NA value
#since we have 157 NA values in Garage
df$Garage_Built_Year[is.na(df$Garage_Built_Year) & 
                       df$Garage == 'NA'] <- "NA" #2 NA value left
#lets take the other two NA as 1999 because the last remodeled date is 1999
df$Garage_Built_Year[is.na(df$Garage_Built_Year)] <- "1999"
#plotting
ggplot(df, aes(x = Garage_Built_Year)) + geom_bar(fill = 'red')

##Garage_Finish_Year
table(df$Garage_Finish_Year)
df[which(is.na(df$Garage_Finish_Year)),] #159 NA value
df$Garage_Finish_Year[is.na(df$Garage_Finish_Year) & 
                       df$Garage == 'NA'] <- "NA" #2 NA value
df$Garage_Finish_Year[is.na(df$Garage_Finish_Year)] <- "Unf"
#plotting
ggplot(df, aes(x = Garage_Finish_Year)) + geom_bar(fill = 'red')

##Garage_Size
table(df$Garage_Size)
df[which(is.na(df$Garage_Size)),] #1 NA value
df$Garage_Size[is.na(df$Garage_Size)] <- "2"
str(df$Garage_Size)
#plotting
ggplot(df, aes(x = Garage_Size)) + geom_bar(fill = 'red')

##Garage Area
table(df$Garage_Area)
summary(df$Garage_Area)
df$Garage_Area[is.na(df$Garage_Area)] <- 477
quantile(df$Garage_Area, seq(0,1,0.01), na.rm = T)
plot(quantile(df$Garage_Area, seq(0,1,0.01), na.rm = T))
quantile(df$Garage_Area, seq(0.99,1,0.001), na.rm = T)
quantile(df$Garage_Area, seq(0,0.1,0.001), na.rm = T)
df$Garage_Area[which(df$Garage_Area > 1024.4188)] <- 1024.4188
df$Garage_Area[which(df$Garage_Area < 0.000000)] <- 0.000000

##Garage_Quality
table(df$Garage_Quality)
df[which(is.na(df$Garage_Quality)),] #159 NA value
df$Garage_Quality[is.na(df$Garage_Quality) & 
                        df$Garage == 'NA'] <- "NA" #2 NA value
df$Garage_Quality[is.na(df$Garage_Quality)] <- "TA"
#plotting
ggplot(df, aes(x = Garage_Quality)) + geom_bar(fill = 'red')

##Garage_Condition
table(df$Garage_Condition)
df[which(is.na(df$Garage_Condition)),] #159 NA value
df$Garage_Condition[is.na(df$Garage_Condition) & 
                    df$Garage == 'NA'] <- "NA" #2 NA value
df$Garage_Condition[is.na(df$Garage_Condition)] <- "TA"
#plotting
ggplot(df, aes(x = Garage_Condition)) + geom_bar(fill = 'red')

##Pavedd_Drive
table(df$Pavedd_Drive)
df[which(is.na(df$Pavedd_Drive)),] #0 NA value
#plotting
ggplot(df, aes(x = Pavedd_Drive)) + geom_bar(fill = 'red')

##W_Deck_Area
table(df$W_Deck_Area)
summary(df$W_Deck_Area)
boxplot(df$W_Deck_Area)
quantile(df$W_Deck_Area, seq(0.99,1,0.001), na.rm = T)
quantile(df$W_Deck_Area, seq(0,0.15,0.001), na.rm = T)
plot(quantile(df$W_Deck_Area, seq(0,1,0.01), na.rm = T))
df$W_Deck_Area[which(df$W_Deck_Area > 458.7350)] <- 458.7350
df$W_Deck_Area[which(df$W_Deck_Area < 0.000000)] <- 0.000000

##Open_Lobby_Area
table(df$Open_Lobby_Area)
summary(df$Open_Lobby_Area)
quantile(df$Open_Lobby_Area, seq(0,1,0.01), na.rm = T)
plot(quantile(df$Open_Lobby_Area, seq(0,1,0.01), na.rm = T))
quantile(df$Open_Lobby_Area, seq(0.99,1,0.001), na.rm = T)
quantile(df$Open_Lobby_Area, seq(0,0.15,0.001), na.rm = T)
df$Open_Lobby_Area[which(df$Open_Lobby_Area > 276.8300)] <- 276.8300
df$Open_Lobby_Area[which(df$Open_Lobby_Area < 0.000000)] <- 0.000000

##Enclosed_Lobby_Area
table(df$Enclosed_Lobby_Area)
summary(df$Enclosed_Lobby_Area)
quantile(df$Enclosed_Lobby_Area, seq(0,1,0.01), na.rm = T)
plot(quantile(df$Enclosed_Lobby_Area, seq(0,1,0.01), na.rm = T))
quantile(df$Enclosed_Lobby_Area, seq(0.99,1,0.001), na.rm = T)
quantile(df$Open_Lobby_Area, seq(0,0.15,0.001), na.rm = T)
df$Enclosed_Lobby_Area[which(df$Enclosed_Lobby_Area > 264.415)] <- 264.415
df$Enclosed_Lobby_Area[which(df$Enclosed_Lobby_Area < 0.000000)] <- 0.000000

##Three_Season_Lobby_Area
table(df$Three_Season_Lobby_Area)
summary(df$Three_Season_Lobby_Area)
quantile(df$Three_Season_Lobby_Area, seq(0,1,0.01), na.rm = T)
plot(quantile(df$Three_Season_Lobby_Area, seq(0,1,0.01), na.rm = T))
quantile(df$Three_Season_Lobby_Area, seq(0.99,1,0.001), na.rm = T)
df$Three_Season_Lobby_Area[which(df$Three_Season_Lobby_Area > 326.071)] <- 326.071

##Screen_Lobby_Area
table(df$Screen_Lobby_Area)
summary(df$Screen_Lobby_Area)
quantile(df$Screen_Lobby_Area, seq(0,1,0.01), na.rm = T)
plot(quantile(df$Screen_Lobby_Area, seq(0,1,0.01), na.rm = T))
quantile(df$Screen_Lobby_Area, seq(0.99,1,0.001), na.rm = T)
df$Screen_Lobby_Area[which(df$Screen_Lobby_Area > 288.000)] <- 288.000

##Pool_Area
table(df$Pool_Area)
summary(df$Pool_Area)
quantile(df$Pool_Area, seq(0,1,0.01), na.rm = T)
plot(quantile(df$Pool_Area, seq(0,1,0.01), na.rm = T))
quantile(df$Pool_Area, seq(0.99,1,0.001), na.rm = T)

##Pool_Quality
table(df$Pool_Quality)
count(df[which(is.na(df$Pool_Quality)),]) # 2908 NA value
df$Pool_Quality[is.na(df$Pool_Quality)] <- "NA"
#plotting
ggplot(df, aes(x = Pool_Quality)) + geom_bar(fill = 'red')

##Fence_Quality
table(df$Fence_Quality)
df[which(is.na(df$Fence_Quality)),] #2347 NA values
df$Fence_Quality[is.na(df$Fence_Quality)] <- "NA" #taking NA as No Fence
#plotting
ggplot(df, aes(x = Fence_Quality)) + geom_bar(fill = 'red')

##Miscellaneous_Feature
table(df$Miscellaneous_Feature)
count(df[which(is.na(df$Miscellaneous_Feature)),]) #2813 NA value
df$Miscellaneous_Feature[is.na(df$Miscellaneous_Feature)] <- "None"
#plotting
ggplot(df, aes(x = Miscellaneous_Feature)) + geom_bar(fill = 'red')

##Miscellaneous_Value
table(df$Miscellaneous_Value)
summary(df$Miscellaneous_Value)
quantile(df$Miscellaneous_Value, seq(0,1,0.01), na.rm = T)
plot(quantile(df$Miscellaneous_Value, seq(0,1,0.01), na.rm = T))
quantile(df$Miscellaneous_Value, seq(0.99,1,0.001), na.rm = T)
df$Miscellaneous_Value[which(df$Miscellaneous_Value > 4500.0)] <- 4500.0

##Month_sold
table(df$Month_Sold)
df[which(is.na(df$Month_Sold)),] #0 NA value
str(df$Month_Sold)
df$Month_Sold <- as.character(df$Month_Sold)
#plotting
ggplot(df, aes(x = Month_Sold)) + geom_bar(fill = 'red')

##Year_Sold
table(df$Year_Sold)
df[which(is.na(df$Year_Sold)),] #0 NA value
str(df$Year_Sold)
df$Year_Sold <- as.character(df$Year_Sold)
#plotting
ggplot(df, aes(x = Year_Sold)) + geom_bar(fill = 'red')

##Sale_Type
table(df$Sale_Type)
df[which(is.na(df$Sale_Type)),] #1 NA value
df$Sale_Type[is.na(df$Sale_Type)] <- "WD"
#plotting
ggplot(df, aes(x = Sale_Type)) + geom_bar(fill = 'red')

##Sale_Condition
table(df$Sale_Condition)
df[which(is.na(df$Sale_Condition)),] #0 NA value
#plotting
ggplot(df, aes(x = Sale_Condition)) + geom_bar(fill = 'red')





###___BI-Variate Analysis___###

#let's split the data

train_1 <- df[1:1459,]
test_1 <- df[1460:2918,]

####plotting graphs w.r.t. price from train_1


#plot1_neighborhood
ggplot(train_1, aes(x = Neighborhood, y = Sale_Price, color = Building_Class)) + geom_point()

#plot2
ggplot(train_1, aes(x = Basement_Condition, y = Sale_Price, color = Building_Class)) + geom_point()

#plot3
ggplot(train_1, aes(x = Exterior_Condition, y = Sale_Price)) + geom_point()

#plot3
ggplot(train_1, aes(x = Lane_Type, y = Sale_Price)) + geom_point()

#plot4
ggplot(train_1, aes(x = Zoning_Class, y = Sale_Price)) + geom_point()

#plot5
ggplot(train_1, aes(x = Remodel_Year, y = Sale_Price, color = Neighborhood)) + geom_point()

#plot6
ggplot(train_1, aes(x = Kitchen_Quality, y = Sale_Price)) + geom_point()

#plot7
ggplot(train_1, aes(x = Road_Type, y = Sale_Price, color = Neighborhood)) + geom_point()

#plot8
ggplot(train_1, aes(x = Fence_Quality, y = Sale_Price, color = Neighborhood)) + geom_point()

#plot9
ggplot(train_1, aes(x = Sale_Type, y = Sale_Price, color = Neighborhood)) + geom_point()

#plot10
ggplot(train_1, aes(x = Sale_Condition, y = Sale_Price, color = Neighborhood)) + geom_point()

#plot11
ggplot(train_1, aes(x = Building_Class, y = Sale_Price, color = Neighborhood)) + geom_point()

#plot12
ggplot(train_1, aes(x = Brick_Veneer_Type, y = Sale_Price, color = House_Type)) + geom_point()

#plot13
ggplot(train_1, aes(x = Brick_Veneer_Area, y = Sale_Price, color = House_Type)) + geom_point()




###feature engineering on df dataset

df$Total_Lobby_area <- df$Open_Lobby_Area + 
                       df$Screen_Lobby_Area + 
                       df$Enclosed_Lobby_Area + 
                       df$Three_Season_Lobby_Area


df$Total_Rooms <- as.numeric(df$Rooms_Above_Grade) + 
                  as.numeric(df$Bedroom_Above_Grade)


df$Total_Bathrooms <- as.numeric(df$Full_Bathroom_Above_Grade) +
                      as.numeric(df$Half_Bathroom_Above_Grade) +
                      as.numeric(df$Underground_Full_Bathroom) +
                      as.numeric(df$Underground_Half_Bathroom)


df$Total_Floor_Area <- as.numeric(df$First_Floor_Area) +
                       as.numeric(df$Second_Floor_Area) +
                       as.numeric(df$Grade_Living_Area) +
                       as.numeric(df$Total_Basement_Area)


df$first_floor <- ifelse(df$First_Floor_Area > 0, 1, 0 )
table(df$first_floor)


df$second_floor <- ifelse(df$Second_Floor_Area > 0, 1, 0 )
table(df$second_floor)


df$ground_floor <- ifelse(df$Grade_Living_Area > 0, 1, 0 )
table(df$ground_floor)


df$basement <- ifelse(df$Total_Basement_Area > 0, 1, 0 )
table(df$basement)


df$Total_Floor <- df$first_floor + df$second_floor + df$ground_floor + df$basement
table(df$Total_Floor)


df$pool <- ifelse(df$Pool_Area > 0, 'Y', 'N' )
table(df$pool)


df$Garage_avail <- ifelse(df$Garage_Area > 0, 'Y', 'N')
table(df$Garage_avail)


df$Bsmnt_Avail <- ifelse(df$Total_Basement_Area > 0, 'Y', 'N')
table(df$Bsmnt_Avail)


#Splitting Basement into three condition
df$basement[which(df$Basement_Condition == 'TA')] <- 'Good'
df$basement[which(df$Basement_Condition == 'Gd')] <- 'Good'
df$basement[which(df$Basement_Condition == 'Po')] <- 'Bad'
df$basement[which(df$Basement_Condition == 'Fa')] <- 'Bad'
df$basement[which(df$Basement_Condition == 'NA')] <- 'No'

#Splitting Exterior into three condition
df$exterior[which(df$Exterior_Condition == 'TA')] <- 'Good'
df$exterior[which(df$Exterior_Condition == 'Ex')] <- 'Good'
df$exterior[which(df$Exterior_Condition == 'Gd')] <- 'Good'
df$exterior[which(df$Exterior_Condition == 'Fa')] <- 'Bad'
df$exterior[which(df$Exterior_Condition == 'Po')] <- 'Bad'

#Splitting Fence Quality
df$Fence_Quality[which(df$Fence_Quality == 'GdPrv')] <- 'Good'
df$Fence_Quality[which(df$Fence_Quality == 'MnPrv')] <- 'Good'
df$Fence_Quality[which(df$Fence_Quality == 'GdWo')] <- 'Bad'
df$Fence_Quality[which(df$Fence_Quality == 'MnWw')] <- 'Bad'
df$Fence_Quality[which(df$Fence_Quality == 'NA')] <- 'No'

df_1 <- as.data.frame(df)



######CREATING DUMMY and finding important columns

train_df <- df_1[1:1459,]
test_df <- df_1[1460:2918,]


# Columns created as dummies.
library(dummies)

data <- dummy.data.frame(train_df[,-1])

data <- bind_cols(Id = train_df$Id, data)

# Split avl. data into train and val. datasets
set.seed(999)
ind = sample(1:nrow(data), 0.75 * nrow(data), replace = F)

t <- data[ind,]
v <- data[-ind,]

#Default model (with all predictors included)
lrm_chk <- lm(Sale_Price ~ ., data = t[,-1]) # ----->>> Leaving 1st column. See why!

options(max.print=9999)
summary(lrm_chk)
#Multiple R-squared:  0.959,	Adjusted R-squared:  0.916


#let's make new model with important variables
model <- df %>% select(Id, Lot_Size, Property_Slope, 
                       Overall_Material, Remodel_Year, Roof_Quality,
                       Exterior_Material, Exposure_Level, Total_Basement_Area, Garage,
                       Garage_Built_Year, Screen_Lobby_Area, Sale_Price)  
str(model)
#View(model)
colSums(is.na(model))

model$Id <- as.character(model$Id)

Land_OutlineLow <- data$Land_OutlineLow
Lot_ConfigurationCulDSac <- data$Lot_ConfigurationCulDSac
Rooms_Above_Grade10 <- data$Rooms_Above_Grade10
Condition2PosN <- data$Condition2PosN
Construction_Year2001 <- data$Construction_Year2001
Full_Bathroom_Above_Grade2 <- data$Full_Bathroom_Above_Grade2


model <- cbind(model, Lot_ConfigurationCulDSac)
model <- cbind(model, Rooms_Above_Grade10)
model <- cbind(model, Condition2PosN)
model <- cbind(model, Land_OutlineLow)
model <- cbind(model, Construction_Year2001)
model <- cbind(model, Full_Bathroom_Above_Grade2)

#lets split model dataset in for train and test
train_mod <- model[1:1459,]
test_mod <- model[1460:2918,]

colSums(is.na(train_mod))
colSums(is.na(test_mod))

glimpse(train_mod)
glimpse(test_mod)

train_mod$Sale_Price <- as.character(train_mod$Sale_Price)

##SCALE
train_mod_scale <- mutate_if(train_mod, is.numeric, scale)

glimpse(train_mod_scale)

train_mod_scale$Sale_Price <- as.numeric(train_mod_scale$Sale_Price)

train_mod_scale <- as.data.frame(train_mod_scale)

summary(train_mod_scale$Sale_Price)
str(train_mod_scale)


# Columns created as dummies.
library(dummies)

final_data <- dummy.data.frame(train_mod_scale[,-1])

final_data <- bind_cols(Id = train_mod_scale$Id, final_data)

# Split avl. data into train and val. datasets
set.seed(999)
index = sample(1:nrow(final_data), 0.75 * nrow(final_data), replace = F)

trn <- final_data[index,]
val <- final_data[-index,]


##################------LINEAR REGRESSION---------##############

#Default model (with all predictors included)
lrm.1 <- lm(Sale_Price ~ ., data = trn[,-1]) # ----->>> Leaving 1st column. See why!

summary(lrm.1)
#Multiple R-squared:  0.8681,	Adjusted R-squared:  0.8398

# Step-wise reduction
lrm.2 <- step(lrm.1, direction = 'both')

summary(lrm.2)
#Multiple R-squared:  0.8622,	Adjusted R-squared:  0.8471

require(car)
sort(vif(lrm.2))

#Try Removing Remodel_Year1950
lrm.3 <- lm(formula = Sale_Price ~ Lot_Size + Property_SlopeGS + Property_SlopeMS + 
              Overall_Material1 + Overall_Material10 + Overall_Material2 + 
              Overall_Material3 + Overall_Material4 + Overall_Material5 + 
              Overall_Material6 + Overall_Material7 + Overall_Material8 + 
              Remodel_Year1951 + Remodel_Year1952 + 
              Remodel_Year1953 + Remodel_Year1954 + Remodel_Year1955 + 
              Remodel_Year1956 + Remodel_Year1957 + Remodel_Year1958 + 
              Remodel_Year1959 + Remodel_Year1960 + Remodel_Year1961 + 
              Remodel_Year1962 + Remodel_Year1963 + Remodel_Year1964 + 
              Remodel_Year1965 + Remodel_Year1966 + Remodel_Year1967 + 
              Remodel_Year1968 + Remodel_Year1969 + Remodel_Year1970 + 
              Remodel_Year1971 + Remodel_Year1972 + Remodel_Year1973 + 
              Remodel_Year1974 + Remodel_Year1975 + Remodel_Year1976 + 
              Remodel_Year1977 + Remodel_Year1978 + Remodel_Year1979 + 
              Remodel_Year1980 + Remodel_Year1981 + Remodel_Year1982 + 
              Remodel_Year1983 + Remodel_Year1984 + Remodel_Year1985 + 
              Remodel_Year1986 + Remodel_Year1987 + Remodel_Year1988 + 
              Remodel_Year1989 + Remodel_Year1990 + Remodel_Year1991 + 
              Remodel_Year1992 + Remodel_Year1993 + Remodel_Year1994 + 
              Remodel_Year1995 + Remodel_Year1996 + Remodel_Year1997 + 
              Remodel_Year1998 + Remodel_Year1999 + Remodel_Year2000 + 
              Remodel_Year2001 + Remodel_Year2002 + Remodel_Year2003 + 
              Remodel_Year2004 + Remodel_Year2005 + Remodel_Year2006 + 
              Remodel_Year2007 + Remodel_Year2008 + Remodel_Year2009 + 
              Roof_QualityTG + Exterior_MaterialEx + Exterior_MaterialGd + 
              Exposure_LevelGd + Exposure_LevelMn + Exposure_LevelNA + 
              Total_Basement_Area + GarageCarPort + GarageDetchd + Garage_Built_Year1922 + 
              Garage_Built_Year1929 + Garage_Built_Year1932 + Garage_Built_Year1939 + 
              Garage_Built_Year1968 + Garage_Built_Year1974 + Garage_Built_Year1992 + 
              Garage_Built_Year1994 + Garage_Built_Year1995 + Garage_Built_Year1997 + 
              Screen_Lobby_Area + Lot_ConfigurationCulDSac + Rooms_Above_Grade10 + 
              Condition2PosN + Land_OutlineLow + Full_Bathroom_Above_Grade2 + 
              GarageBuiltIn + Garage_Built_Year1918 + Garage_Built_Year2010 + 
              GarageNA + Garage_Built_Year1952 + Garage_Built_Year1925 + 
              Garage_Built_Year2002 + Roof_QualityWSh + Garage_Built_Year1958 + 
              Garage_Built_Year2007 + Garage_Built_Year2001, data = trn[, 
                                                                        -1])

summary(lrm.3)
#Multiple R-squared:  0.8576,	Adjusted R-squared:  0.8422
sort(vif(lrm.3))

#Removing Overall Material5 from lrm.2
lrm.4 <- lm(formula = Sale_Price ~ Lot_Size + Property_SlopeGS + Property_SlopeMS + 
              Overall_Material1 + Overall_Material10 + Overall_Material2 + 
              Overall_Material3 + Overall_Material4 +  
              Overall_Material6 + Overall_Material7 + Overall_Material8 + 
              Remodel_Year1950 + Remodel_Year1951 + Remodel_Year1952 + 
              Remodel_Year1953 + Remodel_Year1954 + Remodel_Year1955 + 
              Remodel_Year1956 + Remodel_Year1957 + Remodel_Year1958 + 
              Remodel_Year1959 + Remodel_Year1960 + Remodel_Year1961 + 
              Remodel_Year1962 + Remodel_Year1963 + Remodel_Year1964 + 
              Remodel_Year1965 + Remodel_Year1966 + Remodel_Year1967 + 
              Remodel_Year1968 + Remodel_Year1969 + Remodel_Year1970 + 
              Remodel_Year1971 + Remodel_Year1972 + Remodel_Year1973 + 
              Remodel_Year1974 + Remodel_Year1975 + Remodel_Year1976 + 
              Remodel_Year1977 + Remodel_Year1978 + Remodel_Year1979 + 
              Remodel_Year1980 + Remodel_Year1981 + Remodel_Year1982 + 
              Remodel_Year1983 + Remodel_Year1984 + Remodel_Year1985 + 
              Remodel_Year1986 + Remodel_Year1987 + Remodel_Year1988 + 
              Remodel_Year1989 + Remodel_Year1990 + Remodel_Year1991 + 
              Remodel_Year1992 + Remodel_Year1993 + Remodel_Year1994 + 
              Remodel_Year1995 + Remodel_Year1996 + Remodel_Year1997 + 
              Remodel_Year1998 + Remodel_Year1999 + Remodel_Year2000 + 
              Remodel_Year2001 + Remodel_Year2002 + Remodel_Year2003 + 
              Remodel_Year2004 + Remodel_Year2005 + Remodel_Year2006 + 
              Remodel_Year2007 + Remodel_Year2008 + Remodel_Year2009 + 
              Roof_QualityTG + Exterior_MaterialEx + Exterior_MaterialGd + 
              Exposure_LevelGd + Exposure_LevelMn + Exposure_LevelNA + 
              Total_Basement_Area + GarageCarPort + GarageDetchd + Garage_Built_Year1922 + 
              Garage_Built_Year1929 + Garage_Built_Year1932 + Garage_Built_Year1939 + 
              Garage_Built_Year1968 + Garage_Built_Year1974 + Garage_Built_Year1992 + 
              Garage_Built_Year1994 + Garage_Built_Year1995 + Garage_Built_Year1997 + 
              Screen_Lobby_Area + Lot_ConfigurationCulDSac + Rooms_Above_Grade10 + 
              Condition2PosN + Land_OutlineLow + Full_Bathroom_Above_Grade2 + 
              GarageBuiltIn + Garage_Built_Year1918 + Garage_Built_Year2010 + 
              GarageNA + Garage_Built_Year1952 + Garage_Built_Year1925 + 
              Garage_Built_Year2002 + Roof_QualityWSh + Garage_Built_Year1958 + 
              Garage_Built_Year2007 + Garage_Built_Year2001, data = trn[, 
                                                                        -1])

summary(lrm.4)
#Multiple R-squared:  0.8358,	Adjusted R-squared:  0.8179 
#A huge difference in adjusted r-square, so we will proceed with lrm.3
sort(vif(lrm.4))

#Removing OverallMaterial5 from lrm.3
lrm.5 <- lm(formula = Sale_Price ~ Lot_Size + Property_SlopeGS + Property_SlopeMS + 
              Overall_Material1 + Overall_Material10 + Overall_Material2 + 
              Overall_Material3 + Overall_Material4 +  
              Overall_Material6 + Overall_Material7 + Overall_Material8 + 
              Remodel_Year1951 + Remodel_Year1952 + Remodel_Year1953 + 
              Remodel_Year1954 + Remodel_Year1955 + Remodel_Year1956 + 
              Remodel_Year1957 + Remodel_Year1958 + Remodel_Year1959 + 
              Remodel_Year1960 + Remodel_Year1961 + Remodel_Year1962 + 
              Remodel_Year1963 + Remodel_Year1964 + Remodel_Year1965 + 
              Remodel_Year1966 + Remodel_Year1967 + Remodel_Year1968 + 
              Remodel_Year1969 + Remodel_Year1970 + Remodel_Year1971 + 
              Remodel_Year1972 + Remodel_Year1973 + Remodel_Year1974 + 
              Remodel_Year1975 + Remodel_Year1976 + Remodel_Year1977 + 
              Remodel_Year1978 + Remodel_Year1979 + Remodel_Year1980 + 
              Remodel_Year1981 + Remodel_Year1982 + Remodel_Year1983 + 
              Remodel_Year1984 + Remodel_Year1985 + Remodel_Year1986 + 
              Remodel_Year1987 + Remodel_Year1988 + Remodel_Year1989 + 
              Remodel_Year1990 + Remodel_Year1991 + Remodel_Year1992 + 
              Remodel_Year1993 + Remodel_Year1994 + Remodel_Year1995 + 
              Remodel_Year1996 + Remodel_Year1997 + Remodel_Year1998 + 
              Remodel_Year1999 + Remodel_Year2000 + Remodel_Year2001 + 
              Remodel_Year2002 + Remodel_Year2003 + Remodel_Year2004 + 
              Remodel_Year2005 + Remodel_Year2006 + Remodel_Year2007 + 
              Remodel_Year2008 + Remodel_Year2009 + Roof_QualityTG + Exterior_MaterialEx + 
              Exterior_MaterialGd + Exposure_LevelGd + Exposure_LevelMn + 
              Exposure_LevelNA + Total_Basement_Area + GarageCarPort + 
              GarageDetchd + Garage_Built_Year1922 + Garage_Built_Year1929 + 
              Garage_Built_Year1932 + Garage_Built_Year1939 + Garage_Built_Year1968 + 
              Garage_Built_Year1974 + Garage_Built_Year1992 + Garage_Built_Year1994 + 
              Garage_Built_Year1995 + Garage_Built_Year1997 + Screen_Lobby_Area + 
              Lot_ConfigurationCulDSac + Rooms_Above_Grade10 + Condition2PosN + 
              Land_OutlineLow + Full_Bathroom_Above_Grade2 + GarageBuiltIn + 
              Garage_Built_Year1918 + Garage_Built_Year2010 + GarageNA + 
              Garage_Built_Year1952 + Garage_Built_Year1925 + Garage_Built_Year2002 + 
              Roof_QualityWSh + Garage_Built_Year1958 + Garage_Built_Year2007 + 
              Garage_Built_Year2001, data = trn[, -1])
summary(lrm.5)
#Multiple R-squared:  0.8297,	Adjusted R-squared:  0.8114
sort(vif(lrm.5))

#Removing propertyslopegs 
lrm.6 <- lm(formula = Sale_Price ~ Lot_Size +  Property_SlopeMS + 
              Overall_Material1 + Overall_Material10 + Overall_Material2 + 
              Overall_Material3 + Overall_Material4 + Overall_Material6 + 
              Overall_Material7 + Overall_Material8 + Remodel_Year1951 + 
              Remodel_Year1952 + Remodel_Year1953 + Remodel_Year1954 + 
              Remodel_Year1955 + Remodel_Year1956 + Remodel_Year1957 + 
              Remodel_Year1958 + Remodel_Year1959 + Remodel_Year1960 + 
              Remodel_Year1961 + Remodel_Year1962 + Remodel_Year1963 + 
              Remodel_Year1964 + Remodel_Year1965 + Remodel_Year1966 + 
              Remodel_Year1967 + Remodel_Year1968 + Remodel_Year1969 + 
              Remodel_Year1970 + Remodel_Year1971 + Remodel_Year1972 + 
              Remodel_Year1973 + Remodel_Year1974 + Remodel_Year1975 + 
              Remodel_Year1976 + Remodel_Year1977 + Remodel_Year1978 + 
              Remodel_Year1979 + Remodel_Year1980 + Remodel_Year1981 + 
              Remodel_Year1982 + Remodel_Year1983 + Remodel_Year1984 + 
              Remodel_Year1985 + Remodel_Year1986 + Remodel_Year1987 + 
              Remodel_Year1988 + Remodel_Year1989 + Remodel_Year1990 + 
              Remodel_Year1991 + Remodel_Year1992 + Remodel_Year1993 + 
              Remodel_Year1994 + Remodel_Year1995 + Remodel_Year1996 + 
              Remodel_Year1997 + Remodel_Year1998 + Remodel_Year1999 + 
              Remodel_Year2000 + Remodel_Year2001 + Remodel_Year2002 + 
              Remodel_Year2003 + Remodel_Year2004 + Remodel_Year2005 + 
              Remodel_Year2006 + Remodel_Year2007 + Remodel_Year2008 + 
              Remodel_Year2009 + Roof_QualityTG + Exterior_MaterialEx + 
              Exterior_MaterialGd + Exposure_LevelGd + Exposure_LevelMn + 
              Exposure_LevelNA + Total_Basement_Area + GarageCarPort + 
              GarageDetchd + Garage_Built_Year1922 + Garage_Built_Year1929 + 
              Garage_Built_Year1932 + Garage_Built_Year1939 + Garage_Built_Year1968 + 
              Garage_Built_Year1974 + Garage_Built_Year1992 + Garage_Built_Year1994 + 
              Garage_Built_Year1995 + Garage_Built_Year1997 + Screen_Lobby_Area + 
              Lot_ConfigurationCulDSac + Rooms_Above_Grade10 + Condition2PosN + 
              Land_OutlineLow + Full_Bathroom_Above_Grade2 + GarageBuiltIn + 
              Garage_Built_Year1918 + Garage_Built_Year2010 + GarageNA + 
              Garage_Built_Year1952 + Garage_Built_Year1925 + Garage_Built_Year2002 + 
              Roof_QualityWSh + Garage_Built_Year1958 + Garage_Built_Year2007 + 
              Garage_Built_Year2001, data = trn[, -1])

summary(lrm.6)
#Multiple R-squared:  0.8285,	Adjusted R-squared:  0.8102
sort(vif(lrm.6))

#Removing Remodel_Year1952 
lrm.7 <- lm(formula = Sale_Price ~ Lot_Size + Property_SlopeMS + Overall_Material1 + 
              Overall_Material10 + Overall_Material2 + Overall_Material3 + 
              Overall_Material4 + Overall_Material6 + Overall_Material7 + 
              Overall_Material8 + Remodel_Year1951 + 
              Remodel_Year1953 + Remodel_Year1954 + Remodel_Year1955 + 
              Remodel_Year1956 + Remodel_Year1957 + Remodel_Year1958 + 
              Remodel_Year1959 + Remodel_Year1960 + Remodel_Year1961 + 
              Remodel_Year1962 + Remodel_Year1963 + Remodel_Year1964 + 
              Remodel_Year1965 + Remodel_Year1966 + Remodel_Year1967 + 
              Remodel_Year1968 + Remodel_Year1969 + Remodel_Year1970 + 
              Remodel_Year1971 + Remodel_Year1972 + Remodel_Year1973 + 
              Remodel_Year1974 + Remodel_Year1975 + Remodel_Year1976 + 
              Remodel_Year1977 + Remodel_Year1978 + Remodel_Year1979 + 
              Remodel_Year1980 + Remodel_Year1981 + Remodel_Year1982 + 
              Remodel_Year1983 + Remodel_Year1984 + Remodel_Year1985 + 
              Remodel_Year1986 + Remodel_Year1987 + Remodel_Year1988 + 
              Remodel_Year1989 + Remodel_Year1990 + Remodel_Year1991 + 
              Remodel_Year1992 + Remodel_Year1993 + Remodel_Year1994 + 
              Remodel_Year1995 + Remodel_Year1996 + Remodel_Year1997 + 
              Remodel_Year1998 + Remodel_Year1999 + Remodel_Year2000 + 
              Remodel_Year2001 + Remodel_Year2002 + Remodel_Year2003 + 
              Remodel_Year2004 + Remodel_Year2005 + Remodel_Year2006 + 
              Remodel_Year2007 + Remodel_Year2008 + Remodel_Year2009 + 
              Roof_QualityTG + Exterior_MaterialEx + Exterior_MaterialGd + 
              Exposure_LevelGd + Exposure_LevelMn + Exposure_LevelNA + 
              Total_Basement_Area + GarageCarPort + GarageDetchd + Garage_Built_Year1922 + 
              Garage_Built_Year1929 + Garage_Built_Year1932 + Garage_Built_Year1939 + 
              Garage_Built_Year1968 + Garage_Built_Year1974 + Garage_Built_Year1992 + 
              Garage_Built_Year1994 + Garage_Built_Year1995 + Garage_Built_Year1997 + 
              Screen_Lobby_Area + Lot_ConfigurationCulDSac + Rooms_Above_Grade10 + 
              Condition2PosN + Land_OutlineLow + Full_Bathroom_Above_Grade2 + 
              GarageBuiltIn + Garage_Built_Year1918 + Garage_Built_Year2010 + 
              GarageNA + Garage_Built_Year1952 + Garage_Built_Year1925 + 
              Garage_Built_Year2002 + Roof_QualityWSh + Garage_Built_Year1958 + 
              Garage_Built_Year2007 + Garage_Built_Year2001, data = trn[, 
                                                                        -1])

summary(lrm.7)
#Multiple R-squared:  0.8283,	Adjusted R-squared:  0.8103
sort(vif(lrm.7))

#Remove Garage_Built_Year1968
lrm.8 <- lm(formula = Sale_Price ~ Lot_Size + Property_SlopeMS + Overall_Material1 + 
              Overall_Material10 + Overall_Material2 + Overall_Material3 + 
              Overall_Material4 + Overall_Material6 + Overall_Material7 + 
              Overall_Material8 + Remodel_Year1951 + Remodel_Year1953 + 
              Remodel_Year1954 + Remodel_Year1955 + Remodel_Year1956 + 
              Remodel_Year1957 + Remodel_Year1958 + Remodel_Year1959 + 
              Remodel_Year1960 + Remodel_Year1961 + Remodel_Year1962 + 
              Remodel_Year1963 + Remodel_Year1964 + Remodel_Year1965 + 
              Remodel_Year1966 + Remodel_Year1967 + Remodel_Year1968 + 
              Remodel_Year1969 + Remodel_Year1970 + Remodel_Year1971 + 
              Remodel_Year1972 + Remodel_Year1973 + Remodel_Year1974 + 
              Remodel_Year1975 + Remodel_Year1976 + Remodel_Year1977 + 
              Remodel_Year1978 + Remodel_Year1979 + Remodel_Year1980 + 
              Remodel_Year1981 + Remodel_Year1982 + Remodel_Year1983 + 
              Remodel_Year1984 + Remodel_Year1985 + Remodel_Year1986 + 
              Remodel_Year1987 + Remodel_Year1988 + Remodel_Year1989 + 
              Remodel_Year1990 + Remodel_Year1991 + Remodel_Year1992 + 
              Remodel_Year1993 + Remodel_Year1994 + Remodel_Year1995 + 
              Remodel_Year1996 + Remodel_Year1997 + Remodel_Year1998 + 
              Remodel_Year1999 + Remodel_Year2000 + Remodel_Year2001 + 
              Remodel_Year2002 + Remodel_Year2003 + Remodel_Year2004 + 
              Remodel_Year2005 + Remodel_Year2006 + Remodel_Year2007 + 
              Remodel_Year2008 + Remodel_Year2009 + Roof_QualityTG + Exterior_MaterialEx + 
              Exterior_MaterialGd + Exposure_LevelGd + Exposure_LevelMn + 
              Exposure_LevelNA + Total_Basement_Area + GarageCarPort + 
              GarageDetchd + Garage_Built_Year1922 + Garage_Built_Year1929 + 
              Garage_Built_Year1932 + Garage_Built_Year1939 + 
              Garage_Built_Year1974 + Garage_Built_Year1992 + Garage_Built_Year1994 + 
              Garage_Built_Year1995 + Garage_Built_Year1997 + Screen_Lobby_Area + 
              Lot_ConfigurationCulDSac + Rooms_Above_Grade10 + Condition2PosN + 
              Land_OutlineLow + Full_Bathroom_Above_Grade2 + GarageBuiltIn + 
              Garage_Built_Year1918 + Garage_Built_Year2010 + GarageNA + 
              Garage_Built_Year1952 + Garage_Built_Year1925 + Garage_Built_Year2002 + 
              Roof_QualityWSh + Garage_Built_Year1958 + Garage_Built_Year2007 + 
              Garage_Built_Year2001, data = trn[, -1])
summary(lrm.8)
#Multiple R-squared:  0.828,	Adjusted R-squared:  0.8101 
sort(vif(lrm.8))

#there doesn't seem much much difference in vif, 
#so let's start removing variables based on significance

#remove values based on significance
lrm.9 <- lm(formula = Sale_Price ~ Lot_Size + Property_SlopeMS +  
              Overall_Material10 +  Overall_Material3 + 
              Overall_Material4 +  Overall_Material7 + 
              Overall_Material8 +  
              Remodel_Year1954 + Remodel_Year1964 + Remodel_Year1965 + 
              Remodel_Year1990 + Remodel_Year1994 + Remodel_Year1995 + 
              Remodel_Year2002 + Remodel_Year2007 + 
              Remodel_Year2008 + Remodel_Year2009 + Roof_QualityTG + Exterior_MaterialEx + 
              Exterior_MaterialGd + Exposure_LevelGd + Exposure_LevelMn + 
              Exposure_LevelNA + Total_Basement_Area + GarageCarPort + 
              GarageDetchd + Garage_Built_Year1939 + Garage_Built_Year1974 + 
              Garage_Built_Year1994 + Garage_Built_Year1995 + 
              Screen_Lobby_Area + Lot_ConfigurationCulDSac + 
              Rooms_Above_Grade10 + Condition2PosN + Land_OutlineLow + 
              Full_Bathroom_Above_Grade2 + GarageBuiltIn +  
              Garage_Built_Year2010 + GarageNA + Garage_Built_Year1952 + 
              Garage_Built_Year2002 + Roof_QualityWSh + 
              Garage_Built_Year2007 , 
            data = trn[, -1])
summary(lrm.9)
#Multiple R-squared:  0.8148,	Adjusted R-squared:  0.8073 
sort(vif(lrm.9))

#Removing more columns
lrm.10 <- lm(formula = Sale_Price ~ Lot_Size + Property_SlopeMS + Overall_Material10 + 
               Overall_Material3 + Overall_Material4 + Overall_Material7 + 
               Overall_Material8 + Remodel_Year1954 + Remodel_Year1964 + 
               Remodel_Year1965 + Remodel_Year1990 + Remodel_Year1994 + 
               Remodel_Year1995 + Remodel_Year2007 + 
               Remodel_Year2008 + Remodel_Year2009 + Roof_QualityTG + Exterior_MaterialEx + 
               Exterior_MaterialGd + Exposure_LevelGd + Exposure_LevelMn + 
               Exposure_LevelNA + Total_Basement_Area + GarageCarPort + 
               GarageDetchd + Garage_Built_Year1939 + Garage_Built_Year1995 + Screen_Lobby_Area + 
               Lot_ConfigurationCulDSac + Rooms_Above_Grade10 + Condition2PosN + 
               Land_OutlineLow + Full_Bathroom_Above_Grade2 + GarageBuiltIn + 
               GarageNA + Garage_Built_Year1952 + 
               Garage_Built_Year2002 + Roof_QualityWSh + Garage_Built_Year2007, 
             data = trn[, -1])
summary(lrm.10)
#Multiple R-squared:  0.8132,	Adjusted R-squared:  0.8063
sort(vif(lrm.10))

#Remove PropertySlopeMS
lrm.11 <- lm(formula = Sale_Price ~ Lot_Size + Property_SlopeMS + Overall_Material10 + 
               Overall_Material3 + Overall_Material4 + Overall_Material7 + 
               Overall_Material8 + Remodel_Year1954 + Remodel_Year1964 + 
               Remodel_Year1965 + Remodel_Year1994 + 
               Remodel_Year1995 + Remodel_Year2007 + Remodel_Year2008 + 
               Remodel_Year2009 + Roof_QualityTG + Exterior_MaterialEx + 
               Exterior_MaterialGd + Exposure_LevelGd + 
               Exposure_LevelNA + Total_Basement_Area + GarageCarPort + 
               GarageDetchd + Garage_Built_Year1939 + Garage_Built_Year1995 + 
               Screen_Lobby_Area + Lot_ConfigurationCulDSac + Rooms_Above_Grade10 + 
               Condition2PosN + Land_OutlineLow + Full_Bathroom_Above_Grade2 + 
               GarageBuiltIn + GarageNA + Garage_Built_Year1952 +  
               Roof_QualityWSh + Garage_Built_Year2007, data = trn[, -1])
summary(lrm.11)
#Multiple R-squared:  0.812,	Adjusted R-squared:  0.8056
sort(vif(lrm.11))

#Removing all insignificant variables
lrm.12 <- lm(formula = Sale_Price ~ Lot_Size + Property_SlopeMS + Overall_Material10 + 
               Overall_Material3 + Overall_Material4 + Overall_Material7 + 
               Overall_Material8 + Remodel_Year1954 + Remodel_Year1964 + 
               Remodel_Year1965 + Remodel_Year1994 + Remodel_Year1995 + 
               Remodel_Year2008 + Remodel_Year2009 + 
               Roof_QualityTG + Exterior_MaterialEx + Exterior_MaterialGd + 
               Exposure_LevelGd + Exposure_LevelNA + Total_Basement_Area + 
               GarageCarPort + GarageDetchd + Garage_Built_Year1939 + Garage_Built_Year1995 + 
               Screen_Lobby_Area + Lot_ConfigurationCulDSac + Rooms_Above_Grade10 + 
               Condition2PosN + Land_OutlineLow + Full_Bathroom_Above_Grade2 + 
               GarageBuiltIn + GarageNA + Garage_Built_Year1952 + Roof_QualityWSh + 
               Garage_Built_Year2007, data = trn[, -1])
summary(lrm.12)
#Multiple R-squared:  0.8116,	Adjusted R-squared:  0.8054
sort(vif(lrm.12))


# Let's predict
pred_Sale_price <- predict(lrm.12, newdata = val)

summary(pred_Sale_price)

R = cor(pred_Sale_price, val$Sale_Price)

R

R^2
#r-squared is 0.6037474


#####################################################################

##Test Dataset
test_mod_scale <- mutate_if(test_mod, is.numeric, scale)

test_mod_scale <- as.data.frame(test_mod_scale)

summary(test_mod_scale)
str(test_mod_scale)


# Columns created as dummies.
library(dummies)

test_data <- dummy.data.frame(test_mod_scale[,-1])

test_data <- bind_cols(Id = test_mod_scale$Id, test_data)

#Test Sale Price
test_Sale_price <- predict(lrm.12, newdata = test_data)

summary(test_Sale_price)
View(test_Sale_price)



###############--------Random Forest--------############

require(randomForest)

rf_mod <- train_mod_scale

set.seed(100)
rf_ind = sample(1:nrow(rf_mod), 0.75 * nrow(rf_mod), replace = F)

rf_trn <- rf_mod[rf_ind,]
rf_val <- rf_mod[-rf_ind,]

rf_model <- randomForest(Sale_Price~., rf_trn, ntree = 400, do.trace = 20)

pred_rf <- predict(rf_model, rf_val)

#View(pred_rf)

str(pred_rf)
str(rf_val$Sale_Price)

R_rf = cor(pred_rf, rf_val$Sale_Price)

R_rf^2
#0.733612

#####################################################
#Test Sale Price Random Forest
test_Sale_price_rf <- predict(rf_model, newdata = test_mod_scale)

summary(test_Sale_price_rf)
View(test_Sale_price_rf)


###############--------SVM--------############
require(e1071)
svm_model <- svm(Sale_Price ~ ., data = trn[,-1], kernel = 'radial')

svm_pred <- predict(svm_model, newdata = val)

summary(svm_pred)

R_svm = cor(svm_pred, val$Sale_Price)

R_svm^2
#0.8299532 linear
#0.8469867 radial
#0.7886182 polynomial
#0.6290249 sigmoid

#####################################
test_Sale_price_svm<- predict(svm_model, newdata = test_data)

summary(test_Sale_price)
View(test_Sale_price)




#######################################################
#########-------------KNN--------############
library(class)

knn_label <- trn$Sale_Price
knn_test <- val$Sale_Price

knn_model <- knn(train = trn[,-1],
                 test = val[,-1],
                 cl = knn_label,
                 k = 35)

#count(train_mod)
#sqrt(1220)

summary(knn_model)

View(knn_model)

confusionMatrix(as.factor(knn_model), as.factor(train_mod_scale$Sale_Price))

knn_chek <- train_mod_scale$Sale_Price

knn_chek <- as.data.frame(knn_chek)

knn_chek <- knn_chek[-1,]

str(knn_model)

R_knn <- cor(as.numeric(knn_model), knn_chek)

R_knn^2
#0.8456833





#################################



###### R-Square of different models

#Linear Regression is 0.6037474
#Random Forrest is 0.733612
#SVM(linear) is 0.8299532
#SVM(radial) is 0.8469867
#SVM(polynomial) is 0.7886182
#SVM(sigmoid) is 0.6290249
#KNN is 0.8456833z
