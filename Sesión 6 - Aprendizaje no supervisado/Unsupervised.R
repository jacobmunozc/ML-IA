
# K-means

require(ggplot2)
require(caret)

n.objects = 150
n.cluster = 6
set.seed(27)
sim.data <- data.frame(x = unlist(lapply(1:n.cluster, function(i) rnorm(n = 150/n.cluster, runif(1)*i^2))), 
                       y = unlist(lapply(1:n.cluster, function(i) rnorm(n = 150/n.cluster, runif(1)*i^2))))


# Normalización
data.scale <- scale(sim.data) 
data.scale[is.na(data.scale)] <- 0

# Vector de almacenamiento
clusters.sum.squares <- rep(0.0, 9)

# Insprección del número de clusters (heterogeneidad de los datos)
cluster.params <- 2:15

for (i in cluster.params) {
  # Get the total sum of squared distances for all points
  # in the cluster and store it for plotting later.
  clusters.sum.squares[i - 1] <- sum(kmeans(data.scale, centers = i)$withinss)
} 

ggplot(NULL, aes(x=cluster.params, y=clusters.sum.squares)) +
  theme_bw() +
  geom_point() +
  geom_line() +
  labs(x = "Number of Clusters",
       y = "Suma cuadrada de distancias de clusters",
       title = "Scree Plot de Datos de Jurídicos")



# Ejercicio: usar k-means con titanic




titanic <- read.csv('https://raw.githubusercontent.com/jacobmunozc/ML-IA/main/Sesi%C3%B3n%205%20-%20Regresi%C3%B3n%20y%20clasificaci%C3%B3n%203/Data/titanic.csv')

# As we've seen previously, replace missing values for 
# Embarked with the mode.
titanic$Embarked[titanic$Embarked == ""] <- "S"


# Engineer a new feature for family size.
titanic$FamilySize <- 1 + titanic$SibSp + titanic$Parch


# Engineer a new feature to track which Age values are
# missing.
titanic$AgeMissing <- ifelse(is.na(titanic$Age),
                             "Y", "N")


# Set up all the factors on the data.
titanic$Survived <- as.factor(titanic$Survived)
titanic$Pclass <- as.factor(titanic$Pclass)
titanic$Sex <- as.factor(titanic$Sex)
titanic$Embarked <- as.factor(titanic$Embarked)
titanic$AgeMissing <- as.factor(titanic$AgeMissing)


# Use a very naive (i.e., don't use this in Production)
# model for imputing missing ages.
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, 
                                          na.rm = TRUE)


# Define the subset of features that we will use.
features <- c("Pclass", "Sex", "Age",
              "SibSp", "Parch", "Fare", "Embarked",
              "FamilySize", "AgeMissing")


# Use the mighty caret package to convert factors to
# dummy variables.
dummy.vars <- dummyVars(~ ., titanic[, features])
titanic.dummy <- predict(dummy.vars, titanic[, features])
View(titanic.dummy)


# As we have data on different scales (e.g., Age vs. Fare),
# center and scale (i.e., normalize) the data for K-means.
titanic.dummy <- scale(titanic.dummy)
View(titanic.dummy)


# We'll use the "elbow method" to determine an optimal
# number of clusters for the Titanic training data. Our
# metric will be the the grand total of all squared 
# distances of each point in a cluster to the center.

# First, establish a vector to hold all values.
clusters.sum.squares <- rep(0.0, 14)

# Repeat K-means clustering with K equal to 2, 3, 4,...15.
cluster.params <- 2:15

set.seed(893247)
for (i in cluster.params) {
  # Cluster data using K-means with the current value of i.
  kmeans.temp <- kmeans(titanic.dummy, centers = i)
  
  # Get the total sum of squared distances for all points
  # in the cluster and store it for plotting later.
  clusters.sum.squares[i - 1] <- sum(kmeans.temp$withinss)
}   


# Take a look at our sum of squares.
clusters.sum.squares


# Plot our scree plot using the mighty ggplot2.
ggplot(NULL, aes(x = cluster.params, y = clusters.sum.squares)) +
  theme_bw() +
  geom_point() +
  geom_line() +
  labs(x = "Number of Clusters",
       y = "Cluster Sum of Squared Distances",
       title = "Titanic Training Data Scree Plot")


# OK, cluster the data using the value from the elbow method.
titanic.kmeans <- kmeans(titanic.dummy, centers = 4)


# Add cluster assignments to our data frame
titanic$Cluster <- as.factor(titanic.kmeans$cluster)


# Visualize survivability by cluster assignment.
ggplot(titanic, aes(x = Cluster, fill = Survived)) +
  theme_bw() +
  geom_bar() +
  labs(x = "Cluster Assignment",
       y = "Passenger Count",
       title = "Titanic Training Survivability by Cluster")


