Iris= read.csv("Iris.csv")
Iris$Id= NULL
str(Iris)

# Making a model 
library(rpart)
Rtree= rpart(Species~., 
             data=Iris, 
             minsplit= 2, 
             minbucket= 1, 
             cp= -1)
Rtree

# Visualisation of the tree
library(rpart.plot)
rpart.plot(Rtree,
           extra= 2)




