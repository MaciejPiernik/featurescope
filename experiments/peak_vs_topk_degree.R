source("Points.R")
source("Points.Util.R")
library(doParallel)
library(foreach)

datasets = c("wine", "breast-cancer-wisconsin", "yeast", "glass", "ecoli",
             "vowel-context", "iris", "pima-indians-diabetes", "sonar.all",
             "image-segmentation", "ionosphere", "spectrometer", "statlog-vehicle",
             "optdigits", "statlog-satimage", "pendigits")
methods = c("peakDegree", "topDegree")
methods.parameters = list(
    "randomExamples" = list(
        method = "randomExamples"
    ),
    "topDegree" = list(
        method = "degree"
    ),
    "peakDegree" = list(
        method = "degree",
        angle = 60
    ),
    "clustering" = list(
        method = "clustering"
    )
)

repeats = 30
trainTestSplit = 0.7

set.seed(23)

cores = detectCores()
cl <- makeCluster(cores[1]-1)
registerDoParallel(cl)

results <- foreach(run=1:repeats, .combine=rbind, .packages=c("caret", "readMLData", "dplyr", "fields")) %dopar% {
    source("https://raw.githubusercontent.com/ggrothendieck/gsubfn/master/R/list.R")
    
    run.results = data.frame(train=c(), test=c(), method=c(), run=c(), dataset=c(), classifier=c(), noPoints=c())
    for (dataset.name in datasets) {
        dataset = getDataset(dataset.name, "UCI")
        dataset = preprocessData(dataset)
        
        list[trainSet, testSet] <- splitDataset(dataset, trainTestSplit)
        
        M = as.matrix(dist(trainSet %>% select(-Class)))
        
        currNoPoints = 0
        
        for (method in methods) {
            reference.points = do.call(GenerateReferencePoints, append(list(dataset=trainSet, distanceMatrix=M, noPoints=currNoPoints), as.list(methods.parameters[[method]])))
            if (method == "peakDegree") {
                currNoPoints = nrow(reference.points)
            }
            trainSet.new = TransformDataset(reference.points, trainSet, "Class")
            testSet.new = TransformDataset(reference.points, testSet, "Class")   
            
            list[accuracy.train, accuracy.test] = trainTestEvaluate(trainSet.new, testSet.new, "svmLinear", 2, 5)
            
            resultsRow = data.frame(train=accuracy.train, test=accuracy.test, method=method, run=run, dataset=dataset.name, classifier="svmLinear", noPoints=currNoPoints)
            
            write.table(resultsRow, paste0("results/peak_vs_topk_degree_dataset_", dataset.name, ".csv"), append = TRUE, quote = FALSE, row.names = FALSE, col.names = FALSE)
         
            run.results = rbind(run.results, resultsRow)
        }
    }
    
    run.results
}
