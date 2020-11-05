
# Read and preview the data
data(iris)
head(iris)

# Core boxplot
boxplot(Sepal.Length ~ Species, data=iris)

# Core boxplot with labels
boxplot(
  Sepal.Length ~ Species, 
  data=iris, 
  main="Sepal Length of Species", 
  xlab="Species", 
  ylab="Sepal Length"
)

# Core pie chart
pie(table(iris$Species))

# Core scatter plot
plot(Sepal.Width ~ Sepal.Length, data=iris)

# Core scatter plot with labels
plot(
  Sepal.Width ~ Sepal.Length,
  data=iris, 
  ylab="Sepal Width", 
  xlab="Sepal Length"
)

# Load the required library 
# NOTE: Please ignore the warning message.
library(ggplot2)
# Read and preview the data
data(diamonds)
head(diamonds)

# ggplot density plot
ggplot(diamonds) + geom_density(aes(x=carat), fill="blue") + xlab("Carat") + ylab("Density")

# ggplot scatter plot
ggplot(diamonds, aes(x=carat, y=price)) + geom_point() + xlab("Carat") + ylab("Price")

# Storing a ggplot object for future modification
g <- ggplot(diamonds, aes(x=carat, y=price))
g + geom_point(aes(color=color)) + xlab("Carat") + ylab("Price")

g <- ggplot(diamonds, aes(x=carat, y=price))
g + geom_point(aes(color=clarity)) + xlab("Carat") + ylab("Price")

# Separating segments
g + geom_point(aes(color=color)) + facet_wrap(~ color) + xlab("Carat") + ylab("Price")

g + geom_point(aes(color=clarity)) + facet_wrap(~ color) + xlab("Carat") + ylab("Price")

# Further segmentation
g + geom_point(aes(color=color)) + facet_wrap(cut ~ clarity) + xlab("Carat") + ylab("Price")

# Separating out segmentations
g + geom_point(aes(color=color)) + facet_grid(cut ~ clarity) + xlab("Carat") + ylab("Price")

# Read and preview the data
titanic <- read.csv("Datasets/titanic.csv")
head(titanic)

str(titanic)

# Casting Survived as factor/category levels
titanic$Survived <- as.factor(titanic$Survived)

# Renaming factor levels
levels(titanic$Survived) <- c("Dead", "Survived")
levels(titanic$Embarked) <- c("Unknown", "Cherbourg", "Queenstown", "Southampton")

str(titanic[,c("Embarked", "Survived")])

# Checking class distribution using pie chart
survivedTable <- table(titanic$Survived)
pie(survivedTable, labels=c("Died", "Survived"))

# Is Sex a good predictor?
sex.survived.plt <- ggplot(titanic, aes(x=Sex, fill=Survived))
sex.survived.plt + geom_bar()

# Faceting by Sex
survived.sex.plt <- ggplot(titanic, aes(x=Survived, fill=Sex))
survived.sex.plt + geom_bar() + facet_wrap(~ Sex)

# Faceting by Embarked
survived.sex.plt + geom_bar() + facet_wrap(~ Embarked)

# Faceting by more than 1 variable
survived.sex.plt + geom_bar() + facet_grid(Sex ~ Embarked)

# Is Age a good predictor?
summary(titanic$Age)
summary(titanic[titanic$Survived=="Dead",]$Age)
summary(titanic[titanic$Survived=="Survived",]$Age)

# NOTE: Please ignore the warning message
# Age distribution by gender
age.sex.plt <- ggplot(titanic, aes(x=Age, fill=Sex)) + geom_histogram()
age.sex.plt + labs(x="Distribution of Age", y="Frequency Bucket", 
                   title="Distribution of Passenger Ages on Titanic")

# NOTE: Please ignore the warning message
# Age on survivability
survived.age.plt <- ggplot(titanic, aes(x=Survived, y=Age))
survived.age.plt + geom_boxplot()

# NOTE: Please ignore the warning message
# Further exploring Age on survivability
age.survived.plt <- ggplot(titanic, aes(x=Age, color=Survived))
age.survived.plt + geom_density() + ylab("Density")

# NOTE: Please ignore the warning message
# Segmenting Age on survivability by gender and location
age.survived.plt + geom_density() + facet_grid(Sex ~ Embarked) + ylab("Density")

# NOTE: Please ignore the warning message
# Boxplot of this segmentation
survived.age.plt + geom_boxplot() + facet_grid(Embarked ~ Sex)
