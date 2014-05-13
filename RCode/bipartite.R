
# Code from https://github.com/pedroj/bipartite_plots/blob/master/bipartite_plots.md
# -------------------------------------------------------------------------
# Function bip_binplot, to plot a network object corresponding 
# to a binary adjacency matrix.
# mymat is the adjacency matrix, to get the dimensions from.
# net is a network object corresponding to mymat. The object net can
# be initialized with function bip_init_network.R
#

bip_binplot <- function (mymat, net) {
    #   require(grDevices)
    if(!is.matrix(mymat)) mymat <- as.matrix(mymat)
    if (!is.network(net)) 
        stop("plot.network requires a network object.")
    if (network.size(net) == 0) 
        stop("plot.network called on a network of order zero - 
             nothing to plot.")
    plot.network(net,
                 usearrows=FALSE, jitter= T,
                 #   mode= "circle",
                 #   mode= "fruchtermanreingold",
                 mode= "kamadakawai", 
                 label=network.vertex.names(net), 
                 displaylabels = T,
                 boxed.labels= F, 
                 label.pad= 0, 
                 label.pos= 5, 
                 label.cex= 0.6,
                 #  vertex.col=c(rep(rgb(1, 0, 0, 0.6),dim(mymat)[1]),
                 #               rep(rgb(0, 1, 0, 0.6),dim(mymat)[2])),
                 vertex.col=c(rep("coral3", dim(mymat)[1]),
                              rep("darkolivegreen3", dim(mymat)[2])),
                 vertex.cex= 2,
                 vertex.sides= c(rep(5, dim(mymat)[1]),
                                 rep(20, dim(mymat)[2])),
                 vertex.lty= 0, 
                 edge.lty= 0.7, 
                 edge.col= 8, 
                 label.lty= NULL,
                 usecurve = FALSE)
}



# -------------------------------------------------------------------------
# Bipartite network initialization, starting from an adjacency matrix.
# Code from François Briatte and Pedro Jordano.
#
bipartite.network <- function(M, modes = c("A", "P")) {
    require(network)
    stopifnot(length(modes) == 2)
    if(!is.matrix(M)) M <- as.matrix(M)
    x = dim(M)[1]
    y = dim(M)[2]
    net <- network.initialize(x + y, bipartite = x, directed = FALSE)
    net <- network.bipartite(M, net, 
                             names.eval = list(rownames(M), colnames(M)))
    x = rep(modes[1], x)
    y = rep(modes[2], y)
    network:::set.vertex.attribute(net, "mode", c(x, y))
    return(net)
}
#
# Compte edge weights
# x is a constant helping to scale the edge thickness in the plots;
# just tune it to adequate values for the specific dataset.
#
edge.weights <- function(M, x = 30) {
    # Transpose.
    M <- t(M)
    # Edge list and weights.
    M <- cbind(expand.grid(dimnames(M))[2:1], as.vector(M))
    # Discard null weights.
    M <- subset(M, M[, 3] != 0)
    # Scaled weights.
    M.scaled <- x*logNeg(M[, 3] + 1) / max(logNeg(M[, 3] + 1))
    # Vector of edge weights.
    return(M.scaled)
}


# Plotting bipartite networks from adjacency weighted matrix 
# of two-mode network.
# A modified version of the plotting, with package igraph.
# Requires input of the weighted "mymmat" matrix and its graph object, "g".
#
bip_gplot <- function (mymat, g) {
    ewt <- vectorize(mymat)  
    ewt.scaled <- log(ewt[,3] + 1.0) / max(log(ewt[,3] + 1.0))
    if (!is.igraph(g)) 
        stop("bip_gplot requires a graph object.")
    igraph.options(label.dist= 0, label.degree= pi/2, label.cex= 1,
                   label.family= "mono",
                   edge.lty= 1, 
                   curved= 0.5,
                   edge.color= rgb(0, 0, 0, 0.5),
                   verbose=TRUE)
    plot(g,
         layout= layout.kamada.kawai,
         # vertex.color=c(rep(c("coral3",alpha=0.6), dim(mymat)[1]),
         #          rep(c("darkolivegreen3",alpha=0.6), dim(mymat)[2])),
         vertex.color=c(rep(rgb(0.8, 0.1, 0, 0.8), dim(mymat)[1]),
                        rep(rgb(0, 0.8, 0.2, 0.8), dim(mymat)[2])),
         #        mark.col= c("coral3","darkolivegreen3"),
         vertex.size= 12,
         vertex.shape= c(rep("square",dim(mymat)[1]),
                         rep("circle",dim(mymat)[2])),
         edge.width=25*(ewt.scaled)) # Adjust constant for 
    # better line widths
    #        edge.width=E(g)$weight/20)
}

# -------------------------------------------------------------------------
# Function to initialize a bipartite web for library network (type network),
# starting from an adjacency matrix.
bip_init_network <- function (mymat, modes = c("A", "P")) {
    require(network)
    if(!is.matrix(mymat)) mymat <- as.matrix(mymat)
    a = dim(mymat)[1]
    p = dim(mymat)[2]
    net <- network.initialize(a + p,
                               bipartite = a, directed=FALSE)
    network.bipartite(mymat, net, 
                      names.eval=list(row.names(mymat), colnames(mymat)))
    network:::set.vertex.attribute(net,"mode",
                         c(rep(modes[1], a), rep(modes[2], p)))
}



# Function to initialize a bipartite web for library 
# igraph (type igraph), starting from a weighted adjacency matrix.
# Create the graph object (a bipartite, weighted graph) for igraph.
bip_init_igraph <- function (mymat) {
            require(igraph)
            graph.incidence(mymat, weighted = T)
}
 
vectorize <- function(mat)
{
  mat <- t(mat)
  cbind(expand.grid(dimnames(mat))[2:1], as.vector(mat))
}

#


