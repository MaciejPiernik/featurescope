library(readMLData)
library(caret)
source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")

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

trainClassifier <- function(dataset, classifier, folds, repeats) {
    fitControl = trainControl(method = "repeatedcv",
                               number = folds,
                               repeats = repeats)
    
    capture.output(
        if(classifier == "multinom") {
            fit = train(x = dataset[!names(dataset) %in% "Class"],
                        y = dataset[, "Class"],
                        method = classifier,
                        MaxNWts = 10000000,
                        trControl = fitControl,
                        tuneLength = 10)
        } else {
            fit = train(x = dataset[!names(dataset) %in% "Class"],
                        y = dataset[, "Class"],
                        method = classifier,
                        trControl = fitControl,
                        tuneLength = 10)
            
        }
    )
    
    fit
}

evaluateClassifier <- function(fit, testSet) {
    predictions = predict.train(object=fit, testSet[!names(testSet) %in% "Class"], type="raw")
    
    confusionMatrix(predictions, testSet[, "Class"])
}

trainTestEvaluate <- function(trainSet, testSet, classifier, folds, repeats) {
    
    preProcValues <- preProcess(trainSet[!names(trainSet) %in% "Class"], method = c("center", "scale"))
    
    trainSet[!names(trainSet) %in% "Class"] <- predict(preProcValues, trainSet[!names(trainSet) %in% "Class"])
    testSet[!names(testSet) %in% "Class"] <- predict(preProcValues, testSet[!names(testSet) %in% "Class"])
    
    ### Prepare classifier
    fit = trainClassifier(trainSet, classifier, folds, repeats)
    
    ### Test and evaluate on training data
    train.predictions = predict.train(object=fit, trainSet[!names(trainSet) %in% "Class"], type="raw")
    train.cm = confusionMatrix(train.predictions, trainSet[, "Class"])
    
    ### Test and evaluate on testing data
    test.predictions = predict.train(object=fit, testSet[!names(testSet) %in% "Class"], type="raw")
    test.cm = confusionMatrix(test.predictions, testSet[, "Class"])

    list(trainAccuracy=train.cm$overall[["Accuracy"]], testAccuracy=test.cm$overall[["Accuracy"]])
}

saveResults <- function(output, dataset, classifier, clusterer, trainAccuracy, testAccuracy, k) {
    result.row = cbind(Dataset=dataset,
                       Classifier=classifier,
                       Clustering=clusterer,
                       K=k,
                       TrainAccuracy=trainAccuracy,
                       TestAccuracy=testAccuracy)
    
    header = FALSE
    if(!file.exists(output)){
        header = TRUE
    }
    
    write.table(x=result.row, file=output, 
                row.names=FALSE, col.names=header, append=TRUE, quote=FALSE, sep=",")
    
    result.row
}

showResultsSummary <- function(result, method="ap", by="classifier") {
    no = result %>%
        dplyr::filter(FeatureType == "numeric", Clustering %in% c("no")) %>%
        dplyr::select(Dataset, Classifier, Accuracy)
    
    ap = result %>%
        dplyr::filter(FeatureType == "numeric", Clustering %in% c(method)) %>%
        dplyr::select(Dataset, Classifier, Accuracy)
    
    result = c()
    if(by == "classifier") {
        result = no %>%
            dplyr::full_join(ap, by = c("Dataset", "Classifier")) %>%
            dplyr::group_by(Classifier) %>%
            dplyr::summarise(Better = sum(Accuracy.y > Accuracy.x), Worse = sum(Accuracy.y < Accuracy.x), Equal = sum(Accuracy.y == Accuracy.x)) %>%
            dplyr::arrange(-Better, Worse)
    }else {
        result = no %>%
            dplyr::full_join(ap, by = c("Dataset", "Classifier")) %>%
            dplyr::group_by(Dataset) %>%
            dplyr::summarise(Better = sum(Accuracy.y > Accuracy.x), Worse = sum(Accuracy.y < Accuracy.x), Equal = sum(Accuracy.y == Accuracy.x)) %>%
            dplyr::arrange(-Better, Worse)
    }
    
    result
}

runExperiment <- function(output, datasets, classifiers, clusterers,
                                    featureType, scaling, measure, noClusters, newFeaturesOnly=FALSE,
                                    semi_supervised=FALSE, local=FALSE, split=0.5, folds=5, repeats=2) {
    
    for(datasetName in datasets) {
        print(paste("Processing dataset", datasetName))
        
        ## Read dataset
        dataset = getDataset(datasetName, "UCI")
        dataset = preprocessData(dataset)
        
        ## Split into training and testing datasets
        sets = list()
        sets$no = splitDataset(dataset, split)
        sets$no$k = 0
        
        for(clusterer in setdiff(clusterers, "no")) {
            sets[[clusterer]] = addClusteringFeatures(clusterer, sets$no$train, sets$no$test,
                                                      featureType, scaling, noClusters, measure, local,
                                                      newFeaturesOnly, semi_supervised)
        }
        
        for(classifier in classifiers) {
            tryCatch({
                print(paste("Processing classifier", classifier))
                
                for(clusterer in clusterers) {
                    
                    list[trainAccuracy, testAccuracy] = trainTestEvaluate(sets[[clusterer]]$train, sets[[clusterer]]$test,
                                                                          classifier, folds, repeats)
                    
                    saveResults(output, datasetName, classifier, clusterer, trainAccuracy, testAccuracy, sets[[clusterer]]$k)
                }
                
            }, error = function(e) {
                print(paste("Error during processing dataset", datasetName))
                print(paste("Original error:", e))
            })
        }
    }
}
