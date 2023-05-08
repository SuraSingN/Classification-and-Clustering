library(ggplot2)
library(dplyr)
library(lattice)
library(factoextra)
library(class)

setwd("D:/MSc. Business Analysis & Consulting/Course Material/Business Analytics (MS980)/R programming/Lab_Exam_2022")
DATA <- read.csv("water_exam_2022.csv") 
set.seed(202259078)    
n.sample.omit <- rpois(1, nrow(DATA)/3)  
n.sample <- nrow(DATA) - n.sample.omit  
z.sel<- sample(1:nrow(DATA),n.sample,replace=FALSE)  
my.data<- DATA[z.sel,]

getwd()
dim(my.data)
str(my.data)

my.data <- my.data %>% as_tibble()
my.data

# How many and what percentage of Chloramines of sample are in each group?
class_Chloramines <- as.factor(ifelse(my.data$Chloramines > 8, 
                                      "Chloramines > 8", 
                                      "Chloramines < or equal 8"))
my.data$class_Chloramines <- class_Chloramines
my.data %>%
  count(class_Chloramines)
my.data.1 <- table(my.data$Chloramines, class_Chloramines)
barplot(my.data.1, 
        ylab = "Frequency",
        main = "Chloramines group by value")
round(prop.table(table(my.data$class_Chloramines)), 2)

# Within each of these two groups what proportion of water samples are suitable for drinking (potability = 1).  Display these proportions in a suitable plot.
my.data.2 <- prop.table(table(my.data$Potability, my.data$class_Chloramines), 2)
my.data.2
table(my.data$Potability, my.data$class_Chloramines)
round(my.data.2, 2)
barplot(my.data.2,
        beside = T,
        ylab = "Proportion",
        main = "Water samples group by Chloramines level whether it is potable or not",
        col = c(1,2))
legend("topleft", c("Not Potable", "Potable"),
       col = c(1,2),
       fill = c(1,2))


# Investigate the distributions of, and provide appropriate numerical summaries of, the 2 variables, Solids and Sulfate
par(mfrow = c(1,2))
hist(my.data$Solids, 
     xlab = "Solids: Total dissolved solids in ppm",
     main = "Histogram of Solids")
boxplot(my.data$Solids, outline = F, 
        xlab = "Solids: Total dissolved solids in ppm",
        main = "Histogram of Solids")

par(mfrow = c(1,2))
hist(my.data$Sulfate, 
     xlab = "Sulfate: Amount of Sulfates dissolved in mg/L",
     main = "Histogram of Sulfate")
boxplot(my.data$Sulfate, outline = F, 
        xlab = "Sulfate: Amount of Sulfates dissolved in mg/L",
        main = "Boxplot of Sulfate")

summary(my.data$Sulfate)
summary(my.data$Solids)

sd(my.data$Sulfate)
sd(my.data$Solids)

quantile(my.data$Solids, probs = seq(0,1,0.25))
quantile(my.data$Sulfate, probs = seq(0,1,0.25))

# examine the distributions of Solids and Sulfate, over the two groups of water potability
par(mfrow = c(1,1))
histogram(~Sulfate|factor(Potability),
            data = my.data, 
            layout = c(1,2),
            col="black",
            main = "Sulfate group by Water Potability")
boxplot(Sulfate~Potability, 
        data = my.data,
        outline = F,
        main = "Sulfate group by Water Portability")


histogram(~Solids|factor(Potability),
          data = my.data, 
          layout = c(1,2),
          col="black",
          main = "Solids group by Water Potability")
boxplot(Solids~Potability, 
        data = my.data,
        outline = F,
        main = "Solids group by Water Potability")

by(my.data[,c(3,5)], list(my.data$Potability), summary)

# Explore the 9 measurements and carry out a principal components analysis (PCA) using the correlation matrix
round(cor(my.data[,-10:-11]),2)
pairs(my.data[,-10:-11])

pca_water <- prcomp(my.data[,-10:-11], scale = T)
print(pca_water, digits = 2)
round(pca_water$sdev, 2)

sum(pca_water$sdev^2)

round((pca_water$sdev^2/sum(pca_water$sdev^2)*100), 2)

round(cumsum(pca_water$sdev^2/sum(pca_water$sdev^2)*100), 2)

summary(pca_water)
round((pca_water$rotation), 2) 

plot(pca_water, main = "")
mtext(side =1,
      "Water Level Principal Components",
      line = 1,
      font = 2)

# Explore the first two principal components and plot them out using the water potability to differentiate the points.
round((pca_water$rotation[,1:2]), 2)
waterpc <- predict(pca_water)

plot(waterpc[,1:2], type = "n")
points(x = waterpc[,1],
       y = waterpc[,2],
       col = factor(my.data$Potability),
       pch = 16)
legend("topleft", c("Not Potable", "Potable"),
      col = c(1,2),
      fill = c(1,2))

pc1_water <- round((pca_water$rotation[,1]), 2)
boxplot(waterpc[,1]~Potability, 
        data = my.data, 
        outline = F,
        ylab = "Water_PC1",
        main = "PC1 group by Portability")
by(waterpc[,1], list(my.data$Potability), summary)

# Scale the 9 measurements and carry out a kmeans analysis of the 9 measurements using 5 groups.  Give Example by plotting the Solids against Sulphate and label with points to identify which kmeans group each sample belongs to.  
set.seed(1)
my.data.scale <- scale(my.data[,-10:-11])
my.data.scale <- my.data.scale %>% as_tibble()
my.data.scale
grpWater <- kmeans(my.data.scale, 
                   centers = 5,
                   nstart = 10)
plot(my.data.scale[,c("Solids", "Sulfate")],
     xlab = "Solids",
     ylab = "Sulfate",
     type = "n")
points(my.data.scale[,c("Solids", "Sulfate")],
       pch = 16,
       col = grpWater$cluster)
legend("topright",
       legend = paste("Cluster", 1:5),
       pch = 16,
       col = 1:5)
points(grpWater$centers,
       pch = "+",
       col = 1:5,
       cex = 3)
legend("bottomright",
       legend = paste("Cluster Mean",1:5),
       pch = "+",
       col = 1:5)
round(grpWater$centers, 2)
     
# Tabulate the kmeans clusters against water potability and identify the clusters with the highest and lowest proportions of samples which are potable.
o <- order(grpWater$cluster)
cluster_port <- table(Cluster = grpWater$cluster[o], 
                Potability = my.data$Potability)
cluster_port
round(prop.table(cluster_port,1), 2)

round(grpWater$center, 2)


# Use cross validated k nearest neighbour modelling of water potability using all 9 measurements to find an appropriate number of neighbours to use.  With this selected number of neighbours form the confusion matrix of the tabulation of the predicted water potability from the knn model with the observed value in the data set.  Hence calculate the misclassification rate, the sensitivity and specificity and hence provide an interpretation of these quantities. 
par(mfrow = c(3,4))
hist(my.data$ph)
hist(my.data$Hardness)
hist(my.data$Solids)
hist(my.data$Sulfate)
hist(my.data$Chloramines)
hist(my.data$Conductivity)
hist(my.data$Organic_carbon)
hist(my.data$Trihalomethanes)
hist(my.data$Turbidity)

z.water <- scale(my.data[,-10:-11])
z.water <- z.water %>% as_tibble()
z.water
summary(z.water, digits = 3)
round(var(z.water), 2)

set.seed(1)
z.percentage.mis <- numeric(20)
for (i in 1:20)
{
  z.1 <- knn.cv(train = z.water, 
              cl = my.data$Potability,
              k = i)
  z.percentage.mis[i] <- 100*(1 - sum(z.1 == my.data$Potability) / nrow(z.water))
}
z.percentage.mis
par(mfrow = c(1,1))
plot(1:20,
     z.percentage.mis,
     xlab = "k value",
     ylab = "% missclassified rate",
     type = "l")

z.1 <- knn.cv(train = z.water,
              cl = my.data$Potability,
              k = 5)
z.tab <- table(knn.cv = z.1, Potability = my.data$Potability)
z.tab
100 * (1 - sum(z.1 == my.data$Potability) / nrow(z.water))

Sensitivity <- z.tab[2,2]/sum(z.tab[,2])
Sensitivity
Specificity <- z.tab[1,1]/sum(z.tab[,1])
Specificity
round(c(Sensitivity, Specificity), 2)

