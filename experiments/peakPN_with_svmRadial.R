source("Points.R")
source("Points.Util.R")
#library(doParallel)
#library(foreach)

datasets = c("wine", "breast-cancer-wisconsin", "yeast", "glass", "ecoli",
             "vowel-context", "iris", "pima-indians-diabetes", "sonar.all",
             "image-segmentation", "ionosphere", "spectrometer", "statlog-vehicle",
             "optdigits", "statlog-satimage", "pendigits")

repeats = 30
trainTestSplit = 0.7

set.seed(1)

#cores = detectCores()
#cl <- makeCluster(cores[1]-1)
#registerDoParallel(cl)

#results <- foreach(run=1:repeats, .combine=rbind, .packages=c("caret", "readMLData", "dplyr", "fields")) %dopar% {

pb <- txtProgressBar(min = 0, max = repeats * length(datasets), style = 3)

results = data.frame(train=c(), test=c(), method=c(), run=c(), dataset=c(), classifier=c(), noPoints=c())

for (run in 1:repeats) {
    source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")
    source("Points.R")
    
    run.results = data.frame(train=c(), test=c(), method=c(), run=c(), dataset=c(), classifier=c(), noPoints=c())
    for (dataset.name in datasets) {
        setTxtProgressBar(pb, (run-1)*length(datasets) + match(dataset.name, datasets))
        
        dataset = getDataset(dataset.name, "UCI")
        dataset = preprocessData(dataset)
        
        list[trainSet, testSet] <- splitDataset(dataset, trainTestSplit)
        
        M = as.matrix(dist(trainSet %>% select(-Class)))
        
        reference.points = GenerateReferencePoints(dataset=trainSet, distanceMatrix=M, method="PN", angle=60)
        
        trainSet.new = TransformDataset(reference.points, trainSet, "Class")
        testSet.new = TransformDataset(reference.points, testSet, "Class")   
    
        list[accuracy.train, accuracy.test] = trainTestEvaluate(trainSet.new, testSet.new, "svmLinear", 2, 5)
        
        resultsRow = data.frame(train=accuracy.train, test=accuracy.test, method="peakPN", run=run, dataset=dataset.name, classifier="svmLinear", noPoints=nrow(reference.points))
        
        write.table(resultsRow, "results/peakPN_with_svmLinear.csv", append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
        
        run.results = rbind(run.results, resultsRow)
    }
  
    results = rbind(results, run.results)
}
