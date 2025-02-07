
library("ggplot2")
library("readr")

## read dataset
NY_House_Dataset <- read_csv("C:/D_Drive/Arya/NTU/exchange/study/DA stuff/NY-House-Dataset.csv")

dataset <- NY_House_Dataset

## filter and clean data
dataset <- dataset[dataset$PRICE<195000000,]#only having prices less than $195000000
dataset <- dataset[dataset$PROPERTYSQFT!=2184.207862,]#removing rows with property sqft=2184.207862
dataset <- dataset[dataset$BATH %% 1 == 0, ] #removing baths that are in decimal values
dataset <- dataset[(dataset$BEDS + dataset$BATH) <= 55, ] #removing datasets with beds+bath sum more than 55


## column names
names(dataset)

## fitting linear model 1: predicting price by property square feet
lmod1 <- lm(PRICE~PROPERTYSQFT, data = dataset) #this is the original linear model
plot(PRICE~PROPERTYSQFT, data = dataset)
abline(lmod1)

ggplot(dataset, aes(x = PROPERTYSQFT, y = PRICE)) +
  geom_point() +
  stat_smooth(method = "lm", col="red")


lmod1 <- lm(log10(PRICE)~log10(PROPERTYSQFT), data = dataset) #this is the log transformed model
plot(log10(PRICE)~log10(PROPERTYSQFT), data = dataset)
abline(lmod1)


ggplot(dataset, aes(x = log10(PROPERTYSQFT), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="blue")

summary(lmod1)

## fitting linear model 2: predicting price by sum of beds and baths
lmod2 <- lm(PRICE~BEDS+BATH, data = dataset) #this is the original linear model 
lmod2 <- lm(log10(PRICE)~BEDS+BATH, data = dataset)#this is the log transformed lmod
plot(log10(PRICE)~BEDS+BATH, data = dataset)
abline(lmod2)

ggplot(dataset, aes(x = BEDS+BATH, y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="green")

summary(lmod2)

## fitting linear model 3: predicting price by ratio of bed and bath to square feet
dataset$BB_RATIO <- (dataset$BEDS + dataset$BATH) / dataset$PROPERTYSQFT 
#BBratio is the ratio of sum of beds and bath to the total area of the house
#it tells us whether people prefer more area of the house dedicated to beds and baths or other spaces like living/kitchen/etc

lmod3 = lm(log10(PRICE) ~ log10(BB_RATIO), data = dataset)
plot(log10(PRICE)~log10(BB_RATIO), data = dataset)
abline(lmod3)

ggplot(dataset, aes(x = log10(BB_RATIO), y = log10(PRICE))) +
  geom_point() +
  stat_smooth(method = "lm", col="pink")
#negative gradient shows us that people prefer more space for living/kitchen/etc rather than bigger bedrooms and bathrooms

summary(lmod3)

# I believe that the third one is the most useful one!
























