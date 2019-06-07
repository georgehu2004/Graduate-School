# STAT 551 Regression Tree Assignment 1
# Yuchi Hu

# Packages used in this homework
library(ggplot2)
library(gridExtra)
library(datasets)
library(rpart)
library(maptree)
library(faraway)
library(party)
library(tree)

# Load the iris data
data(iris)
summary(iris)

## Descriptive plots

theme_update(plot.title=element_text(hjust=0.5), legend.position='none', text=element_text(size=17))
# Histogram of sepal length
p1 <- ggplot(data=iris, aes(x=Sepal.Length)) + geom_histogram(aes(y=..density..), color='black', fill='lightblue') + 
  labs(title='Sepal Length', y='Density', x='Sepal Length (cm)') + geom_density(alpha=0.2, fill='red')
# Histogram of sepal width
p2 <- ggplot(data=iris, aes(x=Sepal.Width)) + geom_histogram(aes(y=..density..), color='black', fill='lightblue') + 
  labs(title='Sepal Width', y='Density', x='Sepal Width (cm)') + geom_density(alpha=0.2, fill='red')
# Histogram of petal length
p3 <- ggplot(data=iris, aes(x=Petal.Length)) + geom_histogram(aes(y=..density..), color='black', fill='lightblue') + 
  labs(title='Petal Length', y='Density', x='Petal Length (cm)') + geom_density(alpha=0.2, fill='red')
# Histogram of petal width
p4 <- ggplot(data=iris, aes(x=Petal.Width)) + geom_histogram(aes(y=..density..), color='black', fill='lightblue') + 
  labs(title='Petal Width', y='Density', x='Petal Width (cm)') + geom_density(alpha=0.2, fill='red')
grid.arrange(p1, p2, p3, p4, ncol=2)

# Boxplot of sepal length by species
p1 <- ggplot(data=iris, aes(x=Species, y=Sepal.Length, fill=Species)) + geom_boxplot() +
  labs(title='Sepal Length by Species', y='Sepal Length (cm)')
# Boxplot of sepal width by species
p2 <- ggplot(data=iris, aes(x=Species, y=Sepal.Width, fill=Species)) + geom_boxplot() +
  labs(title='Sepal Width by Species', y='Sepal Width (cm)')
# Boxplot of petal length by species
p3 <- ggplot(data=iris, aes(x=Species, y=Petal.Length, fill=Species)) + geom_boxplot() +
  labs(title='Petal Length by Species', y='Petal Length (cm)')
# Boxplot of petal width by species
p4 <- ggplot(data=iris, aes(x=Species, y=Petal.Width, fill=Species)) + geom_boxplot() +
  labs(title='Petal Width by Species', y='Petal Width (cm)')
grid.arrange(p1, p2, p3, p4, ncol=2)


## 1. Classification tree using rpart

# Build the tree
set.seed(1)
tree.rpart <- rpart(Species ~ ., data=iris, method='class')
# Graph the tree
draw.tree(tree.rpart, cex=1.2, size=5, nodeinfo=T, digits=2, print.levels=T, new=T)
title(main='Classification Tree for the Iris Data (rpart)')
# cptable (lowest CV error occurs at 2 splits, i.e. no pruning necessary)
printcp(tree.rpart)
# Confusion matrix
predicted <- predict(tree.rpart, type='class')
confusion.matrix <- table(predicted, iris$Species)
confusion.matrix
# Error rate
mean(predicted != iris$Species)
# Prune the tree (let's do it anyway)
opt <- which.min(tree.rpart$cptable[,'xerror'])  # Minimum CV error
cp <- tree.rpart$cptable[opt, 'CP']  # Complexity parameter associated with minimum CV error
prune.tree.rpart <- prune(tree.rpart, cp=cp)
draw.tree(prune.tree.rpart, cex=1.2, size=5, nodeinfo=T, digits=2, print.levels=T, new=T)
title(main='Pruned Classification Tree for the Iris Data (rpart)')

## 2. Classification tree using party

# Build the tree
tree.party <- ctree(Species ~ ., data=iris)
# Confusion matrix
predicted <- predict(tree.party, type='response')
confusion.matrix <- table(predicted, iris$Species)
confusion.matrix
# Error rate
mean(predicted != iris$Species)
# Graph the tree
plot(tree.party, main='Classification Tree for the Iris Data (party)')

## 3. Classification tree using tree

# Build the tree
tree.tree <- tree(Species ~ ., data=iris)
summary(tree.tree)
# Graph the tree
draw.tree(tree.tree, cex=1.0, nodeinfo=T, digits=2, print.levels=T, new=T) 
title(main='Classification Tree for the Iris Data (tree)')
# Perform cross-validation
set.seed(1)
cv <- cv.tree(tree.tree, FUN=prune.misclass)
cv$size  # Size (number of terminal nodes) of trees considered
cv$dev  # Cross-validation error rate of trees considered
cv$size[cv$dev == min(cv$dev)]  # Size of tree with lowest CV error rate  
# Prune the tree to 4 nodes 
prune.tree.tree <- prune.misclass(tree.tree, best=4)
summary(prune.tree.tree)  # Error rate = 4/150 = 0.02667
draw.tree(prune.tree.tree, cex=1.0, nodeinfo=T, digits=2, print.levels=T, new=T)  # Graph the pruned tree 
title(main='Pruned Classification Tree for the Iris Data (tree)')
