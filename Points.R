library(fields)
library(dplyr)

GenerateReferencePoints = function(dataset, distanceMatrix, method, noPoints = NULL, angle = NULL) {
    
    if (method == "randomExamples") {
        result = dataset[sample(1:nrow(dataset), noPoints, replace=F), ] %>% select(-Class)
    }
    else if(method == "clustering") {
        stop("Clustering is not implemented yet!")
    }
    else {
        heights = do.call(method, list(dataset=dataset, distanceMatrix=distanceMatrix))
        
        if (!is.null(angle)) {
            result = GetPeaks(dataset, distanceMatrix, heights, angle = angle)
        }
        else if(!is.null(noPoints)) {
            result = cbind(rawData, heights) %>%
                dplyr::top_n(noPoints, heights) %>%
                dplyr::select(-heights)
        }
    }
    
    result
}

GetPeaks = function(data, distanceMatrix, heights, angle = 45) {
    peaks = c()
    
    for (p in 1:nrow(data)) {
        NN = GetNearestNeighbors(p, distanceMatrix, angle)
        
        if(all(heights[p] > heights[NN])){
            peaks = c(peaks, p)
        }
    }
    
    data[peaks, ] %>% select(-Class)
}

GetNearestNeighbors = function(p, distanceMatrix, angle) {
    theta = angle * pi / 180
    
    notNN = c()
    
    for (k in setdiff(1:nrow(distanceMatrix), p)) {
        for (x in setdiff(1:nrow(distanceMatrix), c(p, k))) {
            a = distanceMatrix[p, k]
            b = distanceMatrix[p, x]
            c = distanceMatrix[k, x]
            
            cosAlpha = (a^2 + b^2 - c^2) / (2 * a * b)
            cosBeta = (a^2 + c^2 - b^2) / (2 * a * c)
            
            cosAlpha = min(max(cosAlpha, -1), 1)
            cosBeta = min(max(cosBeta, -1), 1)
            
            alphaAngle = acos(cosAlpha)
            betaAngle = acos(cosBeta)
            
            if (b < a & c < a & alphaAngle < theta & betaAngle < theta) {
                notNN = c(notNN, k)
                break
            }
        }
    }
    
    result = setdiff(1:nrow(distanceMatrix), c(unique(notNN), p))
    
    result
}

GeneratePoints = function(dataset, method = "randomExamples", noPoints, decisionAttr, peaks = FALSE) {
    
    rawData = RemoveFeature(dataset, decisionAttr)
    
    switch(method,
           randomPoints = {
               
           },
           randomExamples = {
               points = rawData[sample(1:nrow(rawData), noPoints, replace=F), ]
           },
           PN = {
               
           },
           degree = {
               degree = Propositionalize(dataset, decisionAttr, k = 5, measure = "degree")
               points = cbind(dataset, degree) %>%
                   dplyr::group_by(Class) %>%
                   dplyr::top_n(noPoints, degree) %>%
                   dplyr::select(-degree)
           },
           status = {
               
           },
           clust.coef = {
               clust.coef = Propositionalize(dataset, decisionAttr, k = 5, measure = "clust.coef")
               points = cbind(dataset, clust.coef) %>%
                   dplyr::group_by(Class) %>%
                   dplyr::top_n(noPoints, clust.coef) %>%
                   dplyr::select(-clust.coef)
           },
           {
               stop('Method not supported!')
           })
    
    points
}

TransformDataset = function(points, dataset, decisionAttr, measure = "euclid") {
    
    rawData = RemoveFeature(dataset, decisionAttr)
    
    switch(measure,
           euclid = {
               features = data.frame(rdist(rawData, points))
           },
           {
               stop('Measure not supported!')
           })
    
    cbind(features, dataset[decisionAttr])
}

RemoveFeature = function(dataset, featureName) {
    i <- match(featureName, names(dataset))
    datasetWithoutFeature = dataset[, -i]
    
    datasetWithoutFeature
}

degree = function(dataset, distanceMatrix) {
    W = GetEdgeWeights(dataset, distanceMatrix)
    
    result = c(rep(1, nrow(W)) %*% W)
    
    result
}

clust.coef = function(dataset, distanceMatrix) {
    W = GetEdgeWeights(dataset, distanceMatrix)
    
    result <- rep(0, nrow(W))
    
    result <- apply(W, 1, function(x) {
        r <- 0
        
        ids <- cbind(expand.grid(1:nrow(W), 1:nrow(W)), rep(0, nrow(W) * nrow(W)))
        k <- sum((x != 0) * 1)
        if (k == 0 || k == 1) {
            r <- 1
        }
        else {
            ids[, 3] <- (x[ids[, 1]] + x[ids[, 2]] + W[as.matrix(ids[, 1:2])]) * ((x[ids[, 1]] * x[ids[, 2]] * W[as.matrix(ids[, 1:2])]) != 0)
            r <- sum(ids[, 3]) / (3 * (k * (k - 1)))
        }
        
        r
    })
    
    as.vector(result)
}

PN <- function(dataset, distanceMatrix) {
    W = GetEdgeWeights(dataset, distanceMatrix)
    
    n <- nrow(W)
    
    V1 <- rep(1, n)
    I <- diag(n)
    P <- (W > 0) * 1
    N <- (W < 0) * 1
    A <- P - 2 * N
    
    result <- solve(I - 1 / (2*n - 2) * A) %*% V1
    
    as.vector(result)
}

status <- function(dataset, distanceMatrix) {
    W = GetEdgeWeights(dataset, distanceMatrix)
    n_iter = 10000
    
    result <- rep(1, nrow(W))
    prev <- rep(0, nrow(W))
    for (i in 1:n_iter) {
        result <- result %*% W
        if(max(abs(result)) == 0) {
            break
        }
        result <- result / max(abs(result))
        if (all(prev == result)) {
            print(paste(i, "iterations"))
            break
        }
        prev <- result
    }
    
    as.vector(result)
}

page.rank = function(dataset, distanceMatrix) {
    W = GetEdgeWeights(dataset, distanceMatrix)
    
    g = igraph::graph.adjacency(W, "undirected", weighted = TRUE)
    
    result = c(igraph::page.rank(g, directed = FALSE)$vector)
    
    result
}

GetEdgeWeights = function(dataset, distanceMatrix) {
    W = 1/distanceMatrix
    W[W == Inf] = 0
    
    classes.as.numbers = as.numeric(dataset$Class)
    
    W.signs = classes.as.numbers %*% t(classes.as.numbers)
    
    noClasses = length(unique(dataset$Class))
    W.signs[W.signs %in% (1:noClasses)^2] = 1
    W.signs[!(W.signs %in% (1:noClasses)^2)] = -1
    
    W = W * W.signs
    
    W
}


