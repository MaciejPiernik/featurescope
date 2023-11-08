source("Points.R")
source("Points.Util.R")
library(doParallel)
library(foreach)

datasets = c("wine", "breast-cancer-wisconsin", "yeast", "glass", "ecoli",
             "vowel-context", "iris", "pima-indians-diabetes", "sonar.all",
             "image-segmentation", "ionosphere", "spectrometer", "statlog-vehicle",
             "optdigits", "statlog-satimage", "pendigits")
methods = c("peakDegree", "peakClustCoef", "peakStatus", "peakPN")
methods.parameters = list(
    "peakDegree" = list(
        method = "degree",
        angle = 60
    ),
    "peakClustCoef" = list(
        method = "clust.coef",
        angle = 60
    ),
    "peakPageRank" = list(
        method = "page.rank",
        angle = 60
    ),
    "peakStatus" = list(
        method = "status",
        angle = 60
    ),
    "peakPN" = list(
        method = "PN",
        angle = 60
    )
)

repeats = 30
trainTestSplit = 0.7

set.seed(23)

#cores = detectCores()
#cl <- makeCluster(cores[1]-1)
#registerDoParallel(cl)

#results <- foreach(run=1:repeats, .combine=rbind, .packages=c("caret", "readMLData", "dplyr", "fields")) %dopar% {
for (run in 1:repeats) {
    source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")
    source("Points.R")
    
    run.results = data.frame(train=c(), test=c(), method=c(), run=c(), dataset=c(), classifier=c(), noPoints=c())
    for (dataset.name in datasets) {
        dataset = getDataset(dataset.name, "UCI")
        dataset = preprocessData(dataset)
        
        list[trainSet, testSet] <- splitDataset(dataset, trainTestSplit)
        
        M = as.matrix(dist(trainSet %>% select(-Class)))
        
        for (method in methods) {
            reference.points = do.call(GenerateReferencePoints, append(list(dataset=trainSet, distanceMatrix=M), as.list(methods.parameters[[method]])))
            currNoPoints = nrow(reference.points)
            
            trainSet.new = TransformDataset(reference.points, trainSet, "Class")
            testSet.new = TransformDataset(reference.points, testSet, "Class")   
            
            list[accuracy.train, accuracy.test] = trainTestEvaluate(trainSet.new, testSet.new, "svmLinear", 2, 5)
            
            resultsRow = data.frame(train=accuracy.train, test=accuracy.test, method=method, run=run, dataset=dataset.name, classifier="svmLinear", noPoints=currNoPoints)
            
            write.table(resultsRow, "results/peakDegree.csv", append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
            
            run.results = rbind(run.results, resultsRow)
        }
    }
    
    run.results
}
