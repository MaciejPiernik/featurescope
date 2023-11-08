source("Points.R")
source("Points.Util.R")
library(doParallel)
library(foreach)

datasets = c("breast-cancer-wisconsin", "yeast")

repeats = 30
trainTestSplit = 0.7

set.seed(23)

#cores = detectCores()
#cl <- makeCluster(cores[1]-1)
#registerDoParallel(cl)

results = c()
#results <- foreach(run=1:repeats, .combine=rbind, .packages=c("caret", "readMLData", "dplyr", "fields")) %dopar% {
for (run in 1:repeats) {
    #source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")
    #source("Points.R")
    
    run.results = data.frame(train=c(), test=c(), method=c(), run=c(), dataset=c(), classifier=c(), noPoints=c())
    for (dataset.name in datasets) {
        dataset = getDataset(dataset.name, "UCI")
        dataset = preprocessData(dataset)
        
        list[trainSet, testSet] <- splitDataset(dataset, trainTestSplit)
        
        M = as.matrix(dist(trainSet %>% select(-Class)))
        
        reference.points = M
        
        trainSet.new = TransformDataset(reference.points, trainSet, "Class")
        testSet.new = TransformDataset(reference.points, testSet, "Class")   
        
        list[accuracy.train, accuracy.test] = trainTestEvaluate(trainSet.new, testSet.new, "rf", 2, 5)
        
        resultsRow = data.frame(train=accuracy.train, test=accuracy.test, method="all", run=run, dataset=dataset.name, classifier="rf", noPoints="all")
        
        write.table(resultsRow, paste0("results/distance_matrix_as_dataset_dataset_", dataset.name, ".csv"), append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
        
        run.results = rbind(run.results, resultsRow)
    }
    
    results = rbind(results, run.results)
}
