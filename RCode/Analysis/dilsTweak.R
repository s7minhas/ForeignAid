
# wrapper function for GenerateDilsNetwork
getPCA <- function(D, yr, n.sub){

# subset the data per y
D = D[which(D$year == yr), ]
# get number of observations per year
n = dim(D)[1]

# igoDist missing after 2005; remove from PCA analysis when after 2005 
if(yr >2005){
  D = D[, -which(names(D) == "igoDist")]
}

# impute unDist where missing
if(any(is.na(D$unDist)) == T) {
    D$unDist[which(is.na(D$unDist))] = rowMeans(D[which(is.na(D$unDist)), -which(names(D) %in% c( "unDist", "ccode1", "ccode2", "year"))])
    print(paste("impute unDist for",  yr ))
}  

    PCA = GenerateDilsNetwork(D, subsample = n, n.subsamples = n.sub, ignore.cols = c(1, 2, 3), return.sds = T)
    print(paste("Finished PCA for", yr))
 
    return(PCA)

}



# adjust GenerateDilsNetwork to return standard deviations of eigenvalues; this allows you to compute the contribution of variance for the first component
# adjust Generate DilsNetwork to return Standard deviations of bootstrap; this allows you to assess the precision of the first component
GenerateDilsNetwork <- function (x, subsample = 10000, n.subsamples = 10000, ignore.cols, 
    use.cols, progress.bar = FALSE, return.sds ) 
{
    if (missing(ignore.cols) && missing(use.cols)) {
        stop("ignore.cols or use.cols must be specified")
    }
    if (missing(use.cols)) {
        use.cols <- setdiff(1:ncol(x), ignore.cols)
    }
    spca <- ScalablePCA(x = x, subsample = subsample, n.subsamples = n.subsamples, 
        use.cols = use.cols, return.sds = TRUE, progress.bar = progress.bar)
    net.coefficients <- spca$coefficients
    net.sds <- spca$sds
    use.x <- as.matrix(x[, use.cols])
    dils.link.coefficients <- as.vector(use.x %*% net.coefficients)
    dils.link.se.upper <- as.vector(use.x %*% c(net.coefficients + 1.96 * spca$bootstrap.sds))
    dils.link.se.lower <- as.vector(use.x %*% c(net.coefficients - 1.96 * spca$bootstrap.sds))
    comparitor.weights <- net.coefficients/net.sds
    comparitor.weights <- comparitor.weights/sum(comparitor.weights)
    out <- list(dils = dils.link.coefficients, dils.edgelist = cbind(x[, 
        -use.cols], dils.link.coefficients, dils.link.se.upper, dils.link.se.lower), coefficients = net.coefficients, 
        weights = comparitor.weights, sdev = spca$sdev, bootstrap.sds = spca$bootstrap.sds)
    return(out)
}


 
# change to set scale = T, center = T
# rotate eigenvectors to the same directions across samples
ScalablePCA <- function (x, filename = NULL, db = NULL, subsample = 10000, n.subsamples = 1000, 
    ignore.cols, use.cols, return.sds = FALSE, progress.bar = FALSE) 
{
    if (missing(x)) {
        if (is.null(filename)) {
            if (is.null(db)) {
                stop("One of x, filename, or db must be specified.")
            }
            else {
                source.type <- "db"
                stop("db option not yet implemented")
            }
        }
        else {
            source.type <- "file"
            if (length(filename) > 1) {
                filename <- filename[1]
                warning("filename has multiple values; using only the first value")
            }
            if (is.character(filename)) {
                if (!file.exists(filename)) {
                  stop("Unable to make a connection to ", filename)
                }
            }
            else {
                stop("filename must be a character (length=1) vector specifying the name of the file to open")
            }
        }
    }
    else {
        source.type <- "data.frame"
        if (!is.data.frame(x)) {
            stop("x must be a data.frame")
        }
    }
    subsample.error.message <- "subsample must be a positive whole number or FALSE"
    if (length(subsample) > 1) {
        subsample <- subsample[1]
        warning("subsample has multiple values; using only the first value")
    }
    if (is.logical(subsample)) {
        if (subsample) {
            stop(subsample.error.message)
        }
        else {
            n.subsamples <- 1
        }
    }
    else {
        if (is.numeric(subsample)) {
            if (subsample <= 0) 
                stop(subsample.error.message)
            if (0 != subsample%%1) 
                stop(subsample.error.message)
        }
        else {
            stop(subsample.error.message)
        }
    }
    n.subsamples.error.message <- "n.subsamples must be a positive whole number"
    if (length(n.subsamples) > 1) {
        n.subsamples <- n.subsamples[1]
        warning("n.subsamples has multiple values; using only the first value")
    }
    if (is.numeric(n.subsamples)) {
        if (n.subsamples <= 0) {
            stop(n.subsamples.error.message)
        }
    }
    else {
        stop(n.subsamples.error.message)
    }
    ignore.cols.error.message <- "ignore.cols must be numeric with positive whole values"
    if (!missing(ignore.cols)) {
        ignore.cols <- sort(unique(ignore.cols))
        if (!is.numeric(ignore.cols)) 
            stop(ignore.cols.error.message)
        if (any(ignore.cols <= 0)) 
            stop(ignore.cols.error.message)
        if (any(0 != ignore.cols%%1)) 
            stop(ignore.cols.error.message)
    }
    use.cols.error.message <- "use.cols must be numeric with positive whole values"
    if (!missing(use.cols)) {
        use.cols <- sort(unique(use.cols))
        if (!is.numeric(use.cols)) 
            stop(use.cols.error.message)
        if (any(use.cols <= 0)) 
            stop(use.cols.error.message)
        if (any(0 != use.cols%%1)) 
            stop(use.cols.error.message)
    }
    if ("db" == source.type) {
        pca.sample <- function(n = subsample) GetSampleFromDb(n = n, 
            db = db)
    }
    else if ("file" == source.type) {
        n.rows.full.dataset <- -1
        con <- file(filename, "r")
        while (length(input <- readLines(con, n = 10000)) > 0) {
            n.rows.full.dataset <- n.rows.full.dataset + length(input)
        }
        close(con)
        pca.sample <- function(n = subsample) GetSampleFromFile(n = n, 
            out.of = n.rows.full.dataset, filename = filename)
    }
    else if ("data.frame" == source.type) {
        n.rows.full.dataset <- nrow(x)
        pca.sample <- function(n = subsample) GetSampleFromDataFrame(n = n, 
            x = x)
    }
    else {
        stop("source.type must be one of db, file, or data.frame")
    }
    if (missing(x) && identical(subsample, FALSE)) 
        subsample <- n.rows.full.dataset
    samp <- pca.sample(n = 1)
    if (!missing(ignore.cols)) {
        use.cols <- setdiff(1:ncol(samp), ignore.cols)
    }
    else {
        if (!all(use.cols %in% 1:ncol(samp))) {
            stop("use.cols must set values between 1 and ", ncol(samp), 
                " for this data set")
        }
    }
    n.cols.used <- length(use.cols)
    if ("db" == source.type) {
    }
    else if ("file" == source.type) {
    }
    else if ("data.frame" == source.type) {
        use.names <- names(x)[use.cols]
    }
    else {
        stop("source.type must be one of db, file, or data.frame")
    }
    draws <- matrix(0, nrow = n.subsamples, ncol = n.cols.used)
    colnames(draws) <- use.names
    if (return.sds) {
        sd.draws <- matrix(0, nrow = n.subsamples, ncol = n.cols.used)
        colnames(sd.draws) <- use.names
        sdev <- matrix(0, nrow = n.subsamples, ncol = n.cols.used)
    }
    if (progress.bar) 
        pb <- txtProgressBar(max = n.subsamples, style = 2)
    for (g in 1:n.subsamples) {
        samp <- pca.sample()
        pca.result <- prcomp(samp[, use.cols], center = TRUE, 
            scale = TRUE)
        draws[g, ] <- pca.result$rotation[, 1]
        if (return.sds) {
            sd.draws[g, ] <- apply(samp[, use.cols], 2, sd)
            sdev[g,] <- pca.result$sdev
        }
        if (progress.bar) 
            setTxtProgressBar(pb, g)
    }
    if (progress.bar) 
        close(pb)
 # rotate all the samples in the same direction; do this by constraining the first value in the eigenvector to always be positive
  draws =  t(apply(draws, 1, function(x) if (sign(x[1])==1){x = x} else { x = -1 * x} ))
    out <- colMeans(draws)
    bootstrap.sds = apply(draws, 2, sd)
    if (return.sds) {
        out <- list(coefficients = out, sds = colMeans(sd.draws), sdev = colMeans(sdev), bootstrap.sds = bootstrap.sds )
    }
    return(out)
}


 
# adjust to sample with replacement

GetSampleFromDataFrame <- function(n, x) {
  # Guardians
  if(length(n) > 1) {
    n <- n[1]
    warning("n has multiple values; using only the first value")
  }
  stopifnot(is.numeric(n),
            n > 0,
            0 == n %% 1)
  stopifnot(is.data.frame(x),
            nrow(x) > 0,
            nrow(x) >= n)
  
  # perform the function
  out <- x[sample(1:nrow(x), n, replace = T),]
 
  # prepare and return the output
  return(out)
}




