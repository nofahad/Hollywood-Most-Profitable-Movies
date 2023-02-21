#Analysis

#Load data:
MoviesTable<- read.csv("HollywoodsMostProfitableStories.csv")

#Take a look at the data:
View(MoviesTable)

#Load Library:
install.packages("tidyverse")

#Import Library:
library("tidyverse")

#Check data types:
str(MoviesTable)

#Check dimensions of data frame
dim(MoviesTable)

#Check for missing values 
colSums(is.na(MoviesTable))

#Remove missing values in rows
movies <- na.omit(MoviesTable)

#Check for missing values 
colSums(is.na(movies))

#Check dimensions of data frame
dim(movies)

View(movies)

#Check for duplicates

dim(movies[duplicated(movies$Film),])[1]

#round off values to 2 places

movies$Profitability <- round(movies$Profitability ,digit=2)

movies$Worldwide.Gross <- round(movies$Worldwide.Gross ,digit=2)


head(movies)

View(movies)

dim(movies)


#Check for outliers on box plot

library(ggplot2)

#Create a boxplot that highlights the outliers

ggplot(movies,aes(x=Profitability, y=Worldwide.Gross)) +geom_boxplot(outlier.colour= "red",outlier.shape= 1)+scale_x_continuous(labels = scales::comma)+coord_cartesian(ylim= c(0,1000))


#Remove outliers in 'Profitability'
Q1 <- quantile(movies$Profitability, .25)
Q3 <- quantile(movies$Profitability, .75)

#inter quartile range
IQR <- IQR(movies$Profitability)

no_outliers <- subset(movies, movies$Profitability> (Q1 - 1.5*IQR) & movies$Profitability< (Q3 + 1.5*IQR))

dim(no_outliers) 

# Remove outliers in 'Worldwide Gross'
Q1 <- quantile(no_outliers$Worldwide.Gross, .25)
Q3 <- quantile(no_outliers$Worldwide.Gross, .75)

#inter quartile range
IQR <- IQR(no_outliers$Worldwide.Gross)

TopMovies <- subset(no_outliers, no_outliers$Worldwide.Gross> (Q1 - 1.5*IQR) & no_outliers$Worldwide.Gross< (Q3 + 1.5*IQR))

dim(TopMovies) 

#Summary Statistics / Univariate Analysis:
summary(TopMovies)

#Bivariate analysis:

#1- Scatter plot

ggplot(TopMovies, aes(x=Lead.Studio, y=Rotten.Tomatoes..)) + geom_point()+ scale_y_continuous(labels = scales::comma)+coord_cartesian(ylim = c(0, 110))+theme(axis.text.x = element_text(angle = 90))

#2- Bar chart
ggplot(TopMovies, aes(x=Year)) + geom_bar()

#Export clean data

write.csv(TopMovies, "Hollywood_Most_Profitable_Stories_Cleaned.csv")


