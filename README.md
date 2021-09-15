# Bike-Sharing-Demand
## Running Linear Regression Model on Bike Sharing Demand Dataset from Kaggle


# Loading Libraries


library(tidyr)
library(dplyr)
library(caTools)
library(ggplot2)

# Reading data from CSV file

bike <- read.csv(file.choose())


head(bike)

### Create a scatter plot of count vs temp. 

ggplot(bike,aes(temp,count)) + geom_point(alpha=0.2, aes(color=temp)) + theme_bw()

### Convert the datetime column into POSIXct before plotting.

bike$datetime <- as.POSIXct(bike$datetime)



ggplot(bike,aes(datetime,count)) + geom_point(aes(color=temp),alpha=0.5)  + scale_color_continuous(low='#55D8CE',high='#FF6E2E') +theme_bw()


### What is the correlation between temp and count?



cor(bike[,c('temp','count')])

### Let's explore the season data. Create a boxplot, with the y axis indicating count and the x axis begin a box for each season

ggplot(bike,aes(factor(season),count)) + 
  geom_boxplot(aes(color=factor(season))) +theme_bw()


### Create an "hour" column that takes the hour from the datetime column

bike$hour <- sapply(bike$datetime,function(x){format(x,"%H")})

### create a scatterplot of count versus hour, with color scale based on temp. Only use bike data where workingday==1

pl <- ggplot(filter(bike,workingday==1),aes(hour,count)) 
pl <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.5)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_bw()

### Now create the same plot for non working days



pl <- ggplot(filter(bike,workingday==0),aes(hour,count)) 
pl <- pl + geom_point(position=position_jitter(w=1, h=0),aes(color=temp),alpha=0.8)
pl <- pl + scale_color_gradientn(colours = c('dark blue','blue','light blue','light green','yellow','orange','red'))
pl + theme_bw()

# Building the Model


### Use lm() to build a model that predicts count based solely on the temp feature, name it temp.model



temp.model <- lm(count~temp,bike)


summary(temp.model)

### How many bike rentals would we predict if the temperature was 25 degrees Celsius?

### one way is to use linear equation to predict the value

6.0462 + 9.17*25

# method 2 is to predict using test data where temp is 25

temp.test <- data.frame(temp=c(25))
predict(temp.model,temp.test)

### Both values are almost identical.

### Using sapply() and as.numeric to change the hour column to a column of numeric values



bike$hour <- sapply(bike$hour,as.numeric)



# Building Model to predict the values



model <- lm(count ~ . -casual - registered -datetime -atemp,bike )

summary(model)


### Did the model perform well on the training data?

##### A linear model like the one we chose which uses OLS won't be able to take into account seasonality of our data, and will get thrown off by the growth in our dataset, accidentally attributing it towards the winter season, instead of realizing its just overall demand growing! Later on, we'll see if other models may be a better fit for this sort of data.




### lets see how well you can predict for future data points by creating a train/test split.


sample <- sample.split(bike$count, SplitRatio = 0.6)

previous <- subset(bike, sample == TRUE)
future <- subset(bike, sample == FALSE)


model1 <- lm(count~., bike)
summary(model1)


plot(model1)


count.predict <- predict(model1, future)

results <- cbind(count.predict, future$count)

colnames(results) <- c("predicted", "actual")

results <- as.data.frame(results)

results
