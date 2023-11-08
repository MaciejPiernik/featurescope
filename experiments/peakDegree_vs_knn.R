source("Points.R")
source("Points.Util.R")
library(doParallel)
library(foreach)

datasets = c("wine", "breast-cancer-wisconsin", "yeast", "glass", "ecoli",
             "vowel-context", "iris", "pima-indians-diabetes", "sonar.all",
             "image-segmentation", "ionosphere", "spectrometer", "statlog-vehicle",
             "optdigits", "statlog-satimage", "pendigits")

repeats = 30
trainTestSplit = 0.7

set.seed(2)

#cores = detectCores()
#cl <- makeCluster(cores[1]-1)
#registerDoParallel(cl)

#results <- foreach(run=1:repeats, .combine=rbind, .packages=c("caret", "readMLData", "dplyr", "fields")) %dopar% {
for (run in 1:repeats) {
    source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")
    
    run.results = data.frame(train=c(), test=c(), method=c(), run=c(), dataset=c(), classifier=c(), noPoints=c())
    for (dataset.name in datasets) {
        dataset = getDataset(dataset.name, "UCI")
        dataset = preprocessData(dataset)
        
        list[trainSet, testSet] <- splitDataset(dataset, trainTestSplit)
        
        M = as.matrix(dist(trainSet %>% select(-Class)))
        
        list[accuracy.train, accuracy.test] = trainTestEvaluate(trainSet, testSet, "knn", 2, 5)
        
        resultsRow = data.frame(train=accuracy.train, test=accuracy.test, method="knn", run=run, dataset=dataset.name, classifier="svmLinear", noPoints="-")
        
        write.table(resultsRow, paste0("results/peakDegree_vs_knn_dataset_", dataset.name, ".csv"), append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
        
        run.results = rbind(run.results, resultsRow)
    }
    
    run.results
}
