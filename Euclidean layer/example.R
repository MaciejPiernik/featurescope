#source("Points.R")
source("EuclideanLayer.R")
library(keras)
library(readMLData)
library(dplyr)
library(caret)
library(ggplot2)
#library(ggpubr)
#library(plotly)
#library(PMCMR)
#library(scmamp)

preprocessData <- function(dataset, dataset.name) {
    dataset = na.omit(dataset)
    
    numeric.columns = sapply(dataset, is.numeric)
    columns.with.no.variance = names(dataset[, numeric.columns])[sapply(dataset[, numeric.columns], var) == 0]
    
    dataset[, numeric.columns] = scale(dataset[, numeric.columns])
    dataset = dataset[, !names(dataset) %in% columns.with.no.variance]
    
    dataset
}

getDataset <- function(name, type, dsList=NULL, responseName="Class",
                       pathData="../datasets/UCI_ML_DataFolders/",
                       pathDescription="../datasets//UCI_ML_DataDescription") {
    if (type == "package") {
        data(list=name)   
        get(name)
    } else if (type == "UCI") {
        if (is.null(dsList)){
            dsList = prepareDSList(pathData, pathDescription)
        }
        dsDownload(dsList, name, "wget", "links.txt")
        dsRead(dsList, name, responseName)
    } else {
        stop(paste("Unknown dataset type: ", type))
    }
}

splitDataset <- function(dataset, splitRatio) {
    index = createDataPartition(dataset$Class, p=splitRatio, list=FALSE)
    trainSet = dataset[ index,]
    testSet = dataset[-index,]
    
    list(train=trainSet, test=testSet)
}

set.seed(10)
dataset.name = "iris"
dataset = getDataset(dataset.name, "UCI") %>% select(V2, V3, Class)
dataset = preprocessData(dataset)
list[trainSet, testSet] <- splitDataset(dataset, 0.7)

x_train = trainSet %>% dplyr::select(-Class) %>% as.matrix()
y_train = trainSet %>% dplyr::select(Class)
x_test = testSet %>% dplyr::select(-Class) %>% as.matrix()
y_test = testSet %>% dplyr::select(Class)

y_train <- keras::to_categorical(as.numeric(y_train$Class)-1, 3)
y_test <- keras::to_categorical(as.numeric(y_test$Class)-1, 3)

# Regular layer
model.std <- keras_model_sequential()
model.std %>%
    layer_dense(units = 9, activation = 'relu', input_shape=2, trainable = TRUE) %>%
    layer_dense(units = 3, activation = 'softmax', kernel_initializer = initializer_constant(1.0))

model.std %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_sgd(),
    metrics = c('accuracy')
)

history.std <- model.std %>% fit(
    x_train, y_train, 
    epochs = 100, batch_size = 1, 
    validation_split = 0.2
)

accuracy.std = model.std %>% evaluate(x_test, y_test, batch_size=1)

plot(history.std)

# Euclidean layer
model.euc <- keras_model_sequential()
model.euc %>%
    layer_euclidean(output_dim = 9) %>%
    layer_dense(units = 3, activation = 'softmax', kernel_initializer = initializer_constant(1.0))

model.euc %>% compile(
    loss = 'categorical_crossentropy',
    optimizer = optimizer_sgd(),
    metrics = c('accuracy')
)

history.euc <- model.euc %>% fit(
    x_train, y_train, 
    epochs = 150, batch_size = 1, 
    validation_split = 0.2
)

accuracy.euc = model.euc %>% evaluate(x_test, y_test, batch_size=1)

weights = data.frame(x=as.numeric(model.euc$weights[[1]][[0]]), y=as.numeric(model.euc$weights[[1]][[1]]))

plot(history.euc)

# Reference points selected using the Euclidean layer
ggplot(trainSet, aes(x=V2, y = V3, color = Class)) +
    geom_point() +
    geom_point(data=weights, mapping=aes(x=x, y=y, color = 'REFERENCE POINTS', size=5), show.legend = FALSE) +
    theme_bw()
