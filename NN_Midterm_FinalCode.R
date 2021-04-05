#Reading the data

spotify.data <- read.csv("Spotify Dataset.csv")
head(spotify.data)
#Looking at the head of the data we can see that the dataset includes 15 columns and majority of them are numerical.
#There are also some negative values which we might need to scale but we'll worry about that at a later step.
#Let's continue to explore the data to see if what else we can find.

#Summary of the data
summary(spotify.data)


#Structure of data
str(spotify.data)
#Here we can confirm that there are 12 integer variables and 3 character variables.


#Next let's take a look we take a look how much data is missing. This is a crucial step before working beginning
# to construct any model. The amount of missing data and how it is approach could significantly impact how a model performs
# and skew the results drastically. It is important to consider various approaches when dealing with missing data such as
# imputation techniques or dropping entries with missing information all together. The amount of missingness and type of data is also a factor
# that should be considered when deciding on imputation techniques.

require("visdat")
vis_dat(spotify.data)
vis_miss(spotify.data)

require("naniar")
gg_miss_var(spotify.data)

#Fortunately for us, this dataset has no missing values! This scenario is not typically when conduting a data science experiment
# but since we have confirmed that our dataset is healthy, we can move on to data exploration to identify potential questions we can answer.



#Most popular genre? (in terms of frequently occurring)


require("plyr")
counts <- table(spotify.data$top.genre)
freq.df <- as.data.frame(counts)
orderfreq <- freq.df[order(-freq.df$Freq),]
orderfreq
pie(counts, main="Most Frequent Genre",xlab="Genre")

#Here we wanted to see how which genre appears the most in our dataset in terms of frequency. 
#Interestingly, it seems that our dataset is mainly composed of the "pop" genre.
#However, it is different variations of pop.
#It looks like the most popular genre is dance pop with 327 songs falling in this category. Dancepop is overwhelmingly the most
#frequent with over 300 occurences.
#We only have 603 observations in this dataset so this means that 54.2% of the songs are dance pop.
#Another thing to note here is dance pop is an overhelming #1 in terms of genre frequency with pop coming 
# in second with a frequency of 60. The diferrence between top two frequent genres is 267.That is is huge drop 
# from first to second.



#Now that we know that dance pop is the most popular in terms of frequency, let's see if that trend occurs again
# when we look at genre in terms of popularity score. If our assumption is correct, we should see the dance pop have a high
# popularity score. To verify this we can take a look at the average popularity score of the top genres.

#Top genre in terms of average popularity score
library(dplyr)
pop.genre1<- aggregate(pop~top.genre, data=spotify.data, weighted.mean)

pop.genre2<- pop.genre1 %>% mutate_if(is.numeric, round)

pop.genre.desc<- pop.genre2 %>% arrange(desc(pop))

top_10<- pop.genre.desc %>% top_n(10)
#Here we are aggregrating the genres in terms of popularity. We are also taking the weighted mean for the popularity score
# for each genre. We then arrange the dataframe in terms of highest average popularity score to least.


library(ggplot2)
library(dplyr)


ggplot(top_10, aes(x=top.genre, y=pop)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label=pop), vjust=-0.3, hjust = -0.5, size=3.5) +
  theme_minimal() +
  coord_flip() 


#After seeing which genre appeared most frequently in our dataset, we wanted to see which
#had the highest (weighted) average popularity score. This could give us an insight into what 
#genres are considered popular, and a rough idea if it plays into popularity at all.

#We have a bar plot portraying the top 10 average scores of genres.
#As we can see in the graph, escape room seems to be the most popular genre with a score of 90. 
#These "escape room" genres include artists such as Lizzo and Charli XCX. It is a combination of
#trap music mixed with r&b and soul.
#Interestingly enough, our assumption was wrong. Dance pop doesn't even make the top 10 in terms of average
# popularity score. Considering dance pop was overlhemingly topped the other genres in terms of frequency, this is 
# definitely a peculiar reveal and something worth digging into.


#Bottom genre in terms of average popularity score

pop.genre1_bot<- aggregate(pop~top.genre, data=spotify.data, weighted.mean)

pop.genre2_bot<- pop.genre1_bot %>% mutate_if(is.numeric, round)

pop.genre.desc_bot<- pop.genre2_bot %>% arrange(desc(pop))

bot_10<- pop.genre.desc_bot %>% top_n(-10)

ggplot(bot_10, aes(x=top.genre, y=pop)) + 
  geom_bar(stat = "identity") +
  geom_text(aes(label=pop), vjust=-0.3, hjust = -0.5, size=3.5) +
  theme_minimal() +
  coord_flip() 

#Consequently, we wanted to see the bottom scoring genres in terms of (weighted) average popularity.
#From the graph, we can see that canadian latin is the least popular with an average popularity score of only
#18. Interestingly, we can see that teh average popularity scores for these "worst" performing songs
#are all in the 50s range. 



#To begin our analysis of dance pop we'll start but subsetting the data. 
#The data will be subsetted into two sets: dance pop and other songs.

#dance pop vs other song subset
dance.pop <- spotify.data %>% filter(`top.genre` == 'dance pop')
other.songs <- spotify.data %>% filter(`top.genre` != 'dance pop')


# How many artists?? 
# Down by about half from the original data set
unique(dance.pop$artist)
unique(other.songs$artist)


# A few artists take a considerable share, which may be 
# informative to our model
par(mfrow=c(2,1), mar=c(2,2,2,2))
barplot(table(dance.pop$artist))
barplot(table(other.songs$artist))

# Will the year matter?
dp.years <- table(dance.pop$year)
barplot(dp.years, main="Dance Pop Frequency",
        xlab="Year")
other.years <- table(other.songs$year)
barplot(other.years, main="Other Music Frequency",
        xlab="Year")


# Density plots of remaining features
# We can see that they are almost never acoustic
par(mfrow=c(3, 4), mar=c(2,2,2,2))
colnames <- names(dance.pop)
for (i in 6:15) {
  hist(dance.pop[,i], main=colnames[i], probability=TRUE, col="gray", 
       border="white")
}



# What about the other songs?
par(mfrow=c(3, 4), mar=c(2,2,2,2))
colnames <- names(other.songs)
for (i in 6:15) {
  hist(other.songs[,i], main=colnames[i], probability=TRUE, col="gray", 
       border="white")
}



# Lets take a look at the acous variable in more detail
# In general the acous variable is on the low range
range(spotify.data$acous)

mean(spotify.data$acous)
mean(dance.pop$acous)
mean(other.songs$acous)



#But let's not focus all our attention on just the genre. We have 14 other features to consider to let's explore those as well.


#Look to see if any correlations between features?

library("corrplot")

num_data<-subset(spotify.data, select = -c(X, title, artist, top.genre))

vis_num_data<- cor(num_data)

corrplot(vis_num_data, method = "number", type = "upper")

#We then wanted to see if any of the features had overwhelming correlations, and if they did, we could 
#remove so that our data wasn't incredibly biased. We only looked at numeric features, as the categorical 
#features we have in our dataset are (for the most part) all unique and it wouldn't work that well even if 
#factored. From the corrplot, we see that none of our features are very correlated, 
#with the most positive and negatively correlated features only being ~0.50



#look at any features that do not have significant pvalue score

cor.mtest <- function(mat, ...) {
  mat <- as.matrix(mat)
  n <- ncol(mat)
  p.mat<- matrix(NA, n, n)
  diag(p.mat) <- 0
  for (i in 1:(n - 1)) {
    for (j in (i + 1):n) {
      tmp <- cor.test(mat[, i], mat[, j], ...)
      p.mat[i, j] <- p.mat[j, i] <- tmp$p.value
    }
  }
  colnames(p.mat) <- rownames(p.mat) <- colnames(mat)
  p.mat
}

# matrix of the p-value of the correlation
p.mat <- cor.mtest(num_data)
head(p.mat[, 1:5])

# Specialized the insignificant value according to the significant level
corrplot(vis_num_data, method = "color", type="upper", order="hclust", 
         p.mat = p.mat, sig.level = c(0.05, -0.05))





#Frequency of artists - if they appeared multiple times?

artist.freq <- table(spotify.data$artist)
artist.freq

artist.freq<- aggregate(pop~artist, data=spotify.data, FUN = length)

artist.freq.desc<- artist.freq %>% arrange(desc(pop))

top_10_freq_artists<- artist.freq.desc %>% top_n(10)
top_10_freq_artists




#Look to see if artists plays into popularity score at all?

artist.top<- aggregate(pop~artist, data=spotify.data, FUN = weighted.mean)

artist.top2<- artist.top %>% mutate_if(is.numeric, round)

pop.artist.desc<- artist.top2 %>% arrange(desc(pop))
head(pop.artist.desc)

top_10_artists<- pop.artist.desc %>% top_n(10)

#Because artist is a feature we couldn't include in the corrplot, we wanted to look and see if
#artist had any true implication of being a significant feature. First we wanted to investiagte
#if an artist has a high frequency (shows up multiple times) in our dataset, if tehy have a higher avergae
#popularity score. Our thoughts here were that if an artist plays into overall popularity, then they should
#have a high popularity score and a high frequency.

#See most frequent artists average popularity score

freq_artists <-as.data.frame.matrix(top_10_freq_artists)
pop_artists <- as.data.frame.matrix(pop.artist.desc)

artist_overview <- merge(freq_artists, pop_artists, by="artist")
colnames(artist_overview)[2]<- "Freq"
colnames(artist_overview)[3]<- "Avg. Pop"

head(artist_overview %>% arrange(desc(Freq)))


#From this table, we can see that a high frequency doesn't necessarily play into 
#a high popularity score. 

#See most popular artists frequency score

freq_artists2<- as.data.frame.matrix(artist.freq)
pop_artists2 <-as.data.frame.matrix(top_10_artists)

artist_overview2 <- merge(pop_artists2, freq_artists2, by="artist")
colnames(artist_overview2)[2]<- "Avg.Pop"
colnames(artist_overview2)[3]<- "Freq"

head(artist_overview2)


#We then wanted to look at the previous table in reverse, with the most popular artists frequency.
#In this table, we can see that the artists with highest popularity score are not shown frequently.
#Therefore contributing to our conclusion that this feature is not significant enough to include as a feature
#in our model.

#Based on the results of our analysis of the dataset thus far, we have elimited the categorical features and also year. 
#This leaves us with the following year,bpm,nrgy,dnce,dB,live,val and dur

#With the remaining features and the data we have, we can build a model that classifies a song as popular.
#To do this we need 

#Task: Determine if a song is popular or not based on it's features 
#Binary 0 or 1 classification

#Step 1 - Identify features that affect popularity
#Step 2 - Update popularity to 0 and 1 depending on threshold
#Step 3 - Neural Network Model


#Removing X, title, artist ,genre, acous and spch columns
spotify.data <- spotify.data[, -c(1:4)]
spotify.data <- spotify.data[, -c(9:10)]
head(spotify.data)


#Updating pop to binary value
spotify.data$pop[spotify.data$pop < 75] <- 0
spotify.data$pop[spotify.data$pop >= 75] <- 1

table(spotify.data$pop)
str(spotify.data$pop)
summary(spotify.data$pop)



# creating training and test data set
require(caTools)
set.seed(101) 
sample = sample.split(spotify.data, SplitRatio = .75)
train = subset(spotify.data, sample == TRUE)
test  = subset(spotify.data, sample == FALSE)
str(train)
str(test)

#Classification using Neural Networks

#install.packages("neuralnet")
require(neuralnet)

# fit neural network
nn1=neuralnet(pop ~ bpm+nrgy+dnce+dB+live+val+dur , data=train, hidden = 1, act.fct = "logistic", err.fct = "ce",
              linear.output = FALSE)

plot(nn1)


nn2=neuralnet(pop ~ bpm+nrgy+dnce+dB+live+val+dur , data=train, hidden = 2, act.fct = "logistic", err.fct = "ce",
              linear.output = FALSE)

plot(nn2)


#Best Model
nn3 = neuralnet(pop ~ year+bpm+nrgy+dnce+dB+live+val+dur, data=train, hidden = 3, act.fct = "logistic",
                linear.output = FALSE)

# plot neural network
plot(nn3)


## Prediction using neural network

Predict=compute(nn5,test)
Predict$net.result
prob <- Predict$net.result
pred <- ifelse(prob>0.5, 1, 0)
pred

finaltable<-table(pred,test$pop)
finaltable


#Misclassification error
1-sum(diag(finaltable))/sum(finaltable)


######################################################################################################

#Activation Function

#Part 2

#For this part we decided to modified the ReLU function so it would cap at specific point and then descend at the same rate until it reached "0"

# Giving x values  
x <- seq(-25, 100, 1)

#Here is a demonstartion on how a regular ReLU function looks like,

# Normal Relu Function
relu2 <- function(x){
  y <- list()
  y[[1]] <- ifelse (x<0,0,x)
  return(y)
}
plot(x, relu2(x)[[1]], col='blue')
# The function returns 0 if it receives any negative input and for any positive value x, it returns that value back. 
# This function is convex with the left tail end of the function being bounded and the right side of the line is unbounded. 
#Has an output that reaches from 0 to infinity 

#Our ultimate goal was to modify the ReLU activation function so that instead of going towards infinity, it reaches a value and then decreases at the same rate it increased. 
#In the process we found some modifications that could be use for other scenarios 


# ReLu Modified 1
relu <- function(x){
  y <- list()
  y[[1]] <- ifelse(x<0,0, ifelse(x>40,x^-1, ifelse(x>41,0,x)))
  
  return(y)
}

plot(x, relu(x)[[1]], col='blue')

#Here, instead of the right side of the line reaching infinity towards both the x and y direction, the Y value drops to "0" once the line reaches a point in the graph and aproaches to infinity on the x directin 
# bounded, non convex.


# Relu Modified 2
relu <- function(x){
  y <- list()
  y[[1]] <- ifelse(x<0,0, ifelse(x>40,40, ifelse(x>60,0,x)))
  
  return(y)
}

plot(x, relu(x)[[1]], col='blue')

#Here we have a similar outcome to our first modification but instead of dropping to "0" it caps at a value in within the y axes and while it reaches infinity on the x axes
# Bounded, non convex.

# Relu mod 3
relu <- function(x){
  y <- list()
  y[[1]] <- ifelse(x<0,0, ifelse(x>40,x-40, ifelse(x>41,0,x)))
  # y[[1]] <- ifelse(x>0,0,-x)
  # y[[1]] <- ifelse(x<40,x,-x)
  
  return(y)
}

plot(x, relu(x)[[1]], col='blue')
# Here is another modification of the ReLU function. Non convex. Bounded in one side and unbounded in the other

###################################
# Main Idea                       #
###################################

#For this scenario we wanted to find a function that showed how good someone or something can get from its moment of creation or start point of practicing 
#for example a specific sport and how after reaching a specific age or point things get old and start loosing popularity or in an athlete's case they just get worse due to age once they reach a specific age.

#Modified Relu
newx<- -25:100

relu <- function(x){
  y <- list()
  y[[1]] <- ifelse(x<0,0, ifelse(x>50,order(-x), ifelse(x>51,0,x)))
  
  return(y)
}

plot(newx, relu(x)[[1]], col='blue')

#This function a non convex, bonded in one side and unbounded in the other. 
#Basically modified the ReLU function to meet our scenario of reaching a point exponentially and once the point it reaches the line drops at the same rate towards "0"

#Here is an example of the sigmoid function modified.
x <- seq(-25, 100, 1)

#here is the one Annie did
sigmoid3 = function(x) {
  y = list() 
  y[[1]] <- 1 / (1 + exp(-(x-75)))
  y[[2]] <- x * (1 - (x-75))
  return(y)
}
plot(x, sigmoid3(x)[[1]], col='blue')

# This is a sigmoid activation function that has been modified with a decision boundary of x=75. This cuntion is not convex and bounded
