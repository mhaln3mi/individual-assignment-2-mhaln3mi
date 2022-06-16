# In this file we will be exploring the data using plain R script.


# First we will need to Initialize necessary packages 
# We will try to use tidyverse package the most in this file
# We also need janitor package to clean our data and here package to set
# top-level directory

# Installing janitor and corrplot

install.packages("janitor")
install.packages("corrplot")
install.packages("here")
install.packages("GGally")
install.packages("reactable")

library(tidyverse)
library(janitor)
library(here)
library(GGally)
library(reactable)




# Second we need to import our dataset from our data folder
# We will use read.csv to import the data then store it in wine object

wine <- read.csv(here("data/winequality-red.csv")


# Now we will clean our data names using janitor clean_names() 
# and store it in wine_df object

wine %>%
  clean_names() -> wine_df

# Lets also check for missing values 

sum(is.na(wine_df))

# The dataset doesn't contain any missing values



# EDA
# Here we will be exploring and visulaizing our data


# First we need to take a quick look at our data using summary()
# and head() tail()

summary(wine_df)

head(wine_df)
tail(wine_df)

# We will also take a look at the structure of the dataset using str()

str(wine_df)

# The dataset has 1559 rows and 12 cols

# Use reactable to produce an interactive table

reactable(wine_df)

# Now lets plot the data

ggplot(wine_df, aes(x=quality, fill = quality)) +
  geom_bar(stat="count") +
  geom_text(position = "stack", stat='count',aes(label=..count..), vjust = -0.5)+
  labs(y="Num of Observations", x="Wine Quality") +
  labs(title= "Red Wine Quality Distribution")

# We can see from this graph that most of our data is in mid range quality 
# with few very high or low quality


# Lets take a look at the correlation heatmap 

corrplot(cor(wine_df))

# We can see from the plot that quality has the strongest correlation with 
# alcohol then sulphates, and the weakest correlation with volatile_acidity



# Lets take a look at the distribution of the numeric variables via histograms

wine_df %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value,fill=key)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram(bins=sqrt(nrow(wine_df))) +
  theme(legend.position="none") 

# one can see that density and pH seem to be symmetric like a normal distribution.
# Volatile acidity also shows a somewhat normal distribution. Looking at the 
# remaining variables, we see that a number of distributions are
# skewed positivly, like chlorides, fixed acidity, residual sugar,
# sulphates among others. This means these distributions have mostly lower end
# values with a few wines with relatively higher values.


# Now we will box plot each factor with quality 
gplot(wine_df, aes(x=as.factor(quality), y=sulphates, fill=as.factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality", y="Sulphates")

ggplot(wine_df, aes(x=as.factor(quality), y=alcohol, fill=as.factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality", y="Alcohol") 

ggplot(wine_df, aes(x=as.factor(quality), y=volatile_acidity, fill=as.factor(quality))) +
  geom_boxplot() +
  labs(x = "Quality", y="Volatile acidity") 




