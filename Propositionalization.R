source("Network.Measures.R")
library(infotheo)
library(igraph)
library(dplyr)

###########################################################################
# Computes the weights of each instance in the dataset
#
# Args:
#	 data [data frame]: The dataset to process.
#
# Returns:
#	 Weights for each instance in the dataset.
###########################################################################
Propositionalize <- function(dataset, decisionAttribute, k = NULL, threshold = NULL, measure = NA, weighted = TRUE) {
    ###########################################################################
    # internal functions
    ###########################################################################
    
    CalculateNetworkAttributes <- function(g) {
        degree <- graph.strength(g, loops = FALSE);
        betweenness <- betweenness(g, directed = FALSE, normalized = FALSE);
        closeness <- closeness(g);
        clustering.coefficient <- transitivity(g, type = "weighted");
        eigenvector <- eigen_centrality(g)$vector;
        page.rank <- page.rank(g, directed = FALSE)$vector;
        authority <- authority.score(g)$vector;
        network.attributes <- cbind(degree, betweenness, closeness, clustering.coefficient,
                                    eigenvector, page.rank, authority);
        names(network.attributes) <- c("degree", "betweenness", "closeness", "clustering.coefficient",
                                       "eigenvector", "page.rank", "authority");
        
        network.attributes;
    }
    
    CreateOrAddColumn <- function(result, colHeader, values) {
        if (is.null(result)) {
            result <- eval(parse(text = paste("data.frame(", colHeader, " = values)", sep = "")))
        }
        else {
            result <- cbind(result, eval(parse(text = paste("data.frame(", colHeader, " = values)", sep = ""))))
        }
        
        result
    }
    
    CalculateNewMeasures <- function(g, dataset, measure = NA, weighted = TRUE) {
        
        if(weighted) {
            adj.matrix <- as_adj(g, attr = "weight")
        } else {
            adj.matrix <- as_adj(g)
        }
        
        result = NULL
        switch(measure,
               degree = {
                   result <- degree.neg(adj.matrix)
               },
               status = {
                   result <- status(adj.matrix)
               }, 
               pn = {
                   result <- PN(adj.matrix)
               },
               clust.coef = {
                   result <- clustering.coefficient.neg(adj.matrix)  
               },
               {
                   result = data.frame(degree = degree.neg(adj.matrix), 
                                       status = status(adj.matrix), 
                                       pn = PN(adj.matrix), 
                                       clust.coef = clustering.coefficient.neg(adj.matrix))
               })
        
        result
    }
    
    ###########################################################################
    
    g <- CreateGraph(dataset, decisionAttribute, k, threshold)
    
    # attributes <- CalculateNetworkAttributes(g);
    measures = CalculateNewMeasures(g, dataset, measure, weighted)
    
    measures
}

CreateGraph <- function(dataset, decisionAttribute, k = NULL, threshold = NULL) {
    
    edges <- GetWeightedEdges(dataset, decisionAttribute);
    
    if(is.null(k) || is.na(k)) { # a) Remove edges by threshold
        edges.filtered <- edges[edges$start < edges$end, ];
        g <- graph.data.frame(edges.filtered, directed = FALSE);
        g <- delete.edges(g, E(g)[abs(weight) < threshold]);
    }
    else { # b) knn graph
        if (k < 1) {
            k <- ceiling(k * nrow(dataset))
        }
        
        # Take edges with weights closest to 1
        edges <- edges %>%
            filter(start != end) %>%
            arrange(start, -abs(weight));
        edges.knn <- edges[0, ];
        
        for (i in 1:k) {
            edges.knn <- rbind(edges.knn, edges[seq(from = i, to = nrow(edges), by = nrow(dataset) - 1), ]);
        }
        
        g <- graph.data.frame(edges.knn, directed = TRUE);
    }
    
    if (length(E(g)) == 0) {
        stop("Empty graph!")
    }
    
    g
}

GetWeightedEdges <- function(dataset, decisionAttribute) {
    
    X <- cbind(id = 1:nrow(dataset), dataset[, names(dataset) != decisionAttribute]);
    Y <- dataset[, names(dataset) == decisionAttribute]
    
    result <- cbind(expand.grid(X[, 1], X[, 1]), numeric(nrow(X)^2));
    colnames(result) <- c("start", "end", "weight");
    
    for (i in 2:ncol(X)) {
        if(is.numeric(X[1, i])) {
            x <- expand.grid(X[, i], X[, i]);
            abs.diff.x <- abs(x[, 1] - x[, 2]);
            mi <- min(abs.diff.x[!is.na(abs.diff.x)])
            ma <- max(abs.diff.x[!is.na(abs.diff.x)])
            if(mi == ma) {
                tmp <- rep(0, length(abs.diff.x));
            }
            else {
                tmp <- (abs.diff.x - mi) / (ma - mi);
            }
            tmp[is.na(tmp)] <- 0;
            result[, 3] <- result[, 3] + tmp;
        }
        else {
            x <- as.numeric(as.factor(X[, i]));
            x <- expand.grid(x, x);
            tmp <- (x[, 1] != x[, 2]) * 1;
            tmp[is.na(tmp)] <- 0;
            result[, 3] <- result[, 3] + tmp;
        }
    }
    
    # Normalize to <0-1>
    result[, 3] <- result[, 3] / (ncol(X) - 1);
    
    # Convert to similarity
    result[, 3] <- 1 / (1 + result[, 3]);
    
    # The output of previous line produces values between 0.5 and 1,
    # so we scale it to fall into <0-1> range.
    result[, 3] <- (result[, 3] - 0.5) / (0.5);
    
    result[, 3] <- result[, 3] * ifelse(Y[result[, 1]] == Y[result[, 2]], 1, -1);
    
    result;
}
