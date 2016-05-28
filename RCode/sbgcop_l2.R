# Y = nodeData[,-1]
# S0 = diag(dim(Y)[2])
# n0 = dim(Y)[2] + 2

# Y_l2 = dyadData[,-c(1:2)]

# nsamp = 10
# odens = max(1, round(nsamp/1000))
# plugin.threshold = 100
# plugin.marginal = (apply(Y, 2, function(x) { length(unique(x)) }) > plugin.threshold)

# seed = 6886
# verb = TRUE

sbgcop.mcmc_l2 <- function (
    Y,
    S0 = diag(dim(Y)[2]),
    n0 = dim(Y)[2] + 2,
    Y_l2,
    nsamp = 100, 
    odens = max(1, round(nsamp/1000)),
    plugin.threshold = 100, 
    plugin.marginal = (apply(Y, 2, function(x) { length(unique(x)) }) > plugin.threshold),
    seed = 1,
    verb = TRUE) 
{

    # Some checks and bookkeeping set up before running mcmc for level 1
    ok_S0 <- all(eigen(S0)$val > 0) & dim(S0)[1] == dim(Y)[2] & dim(S0)[2] == dim(Y)[2]
    ok_n0 <- (n0 >= 0)
    if (!ok_S0) { cat("Error: S0 must be a positive definite p x p matrix in Level 1 \n") }
    if (!ok_n0) { cat("Error: n0 must be positive in Level 1 \n") }
    vnames <- colnames(Y)
    Y <- as.matrix(Y)
    colnames(Y) <- vnames
    n <- dim(Y)[1]
    p <- dim(Y)[2]
    set.seed(seed)
    R <- NULL
    for (j in 1:p) { R <- cbind(R, match(Y[, j], sort(unique(Y[, j])))) }
    Rlevels <- apply(R, 2, max, na.rm = TRUE)
    Ranks <- apply(Y, 2, rank, ties.method = "max", na.last = "keep")
    N <- apply(!is.na(Ranks), 2, sum)
    U <- t(t(Ranks)/(N + 1))
    Z <- qnorm(U)
    Zfill <- matrix(rnorm(n * p), n, p)
    Z[is.na(Y)] <- Zfill[is.na(Y)]
    S <- cov(Z)
    LPC <- NULL
    C.psamp <- array(dim = c(p, p, floor(nsamp/odens)))
    Y.imp <- array(dim = c(n, p, floor(nsamp/odens)))
    dimnames(C.psamp) <- list(colnames(Y), colnames(Y), 1:floor(nsamp/odens))

    # Overall imputation model
    for (ns in 1:nsamp) {

        # Begin imputation process for level 1 data

        # Run through variables
        for (j in sample(1:p)) {
            Sjc <- S[j, -j] %*% solve(S[-j, -j])
            sdj <- sqrt(S[j, j] - S[j, -j] %*% solve(S[-j, -j]) %*% S[-j, j])
            muj <- Z[, -j] %*% t(Sjc)
            if (!plugin.marginal[j]) {
              for (r in 1:Rlevels[j]) {
                ir <- (1:n)[R[, j] == r & !is.na(R[, j])]
                lb <- suppressWarnings(max(Z[R[, j] == r - 1, j], na.rm = TRUE))
                ub <- suppressWarnings(min(Z[R[, j] == r + 1, j], na.rm = TRUE))
                Z[ir, j] <- qnorm(runif(length(ir), pnorm(lb, 
                  muj[ir], sdj), pnorm(ub, muj[ir], sdj)), muj[ir], sdj)
              }
            }
            ir <- (1:n)[is.na(R[, j])]
            Z[ir, j] <- rnorm(length(ir), muj[ir], sdj)
        }

        # Pull out S and save if at the right ns oden combination
        S <- solve(rwish(solve(S0 * n0 + t(Z) %*% Z), n0 + n))
        # Comment out odents check, always need this to be run for second level model
        # if (ns%%odens == 0) {
            C <- S/(sqrt(diag(S)) %*% t(sqrt(diag(S))))
            lpc <- ldmvnorm(Z %*% diag(1/sqrt(diag(S))), C)
            LPC <- c(LPC, lpc)
            C.psamp[, , ns/odens] <- C
            Y.imp.s <- Y
            for (j in 1:p) {
            Y.imp.s[is.na(Y[, j]), j] <- quantile(Y[, j], 
                pnorm(Z[is.na(Y[, j]), j], 0, sqrt(S[j,j])), na.rm = TRUE, type = 1) }
            Y.imp[, , ns/odens] <- Y.imp.s
        # }

        # Merge level 1 imputation with level 2 data
        Y.imp.s = data.frame( Y.imp.s , row.names=NULL)
        Y.imp.s$id = paste0(Y.imp.s$ccodeR, Y.imp.s$year)
        vars = names(Y.imp.s)[3:(ncol(Y.imp.s)-1)]
        
        # Drop Y_l2 i/j vars from ns-1 imputation so they can
        ## be replaced by i/j vars from ns imputation

        if(ns==1){print(colnames(Y_l2))
            print(head(Y_l2))}
        if(ns>1){
            print (paste(c('****************', 'nsamp is', ns), collasep = ' '))
            head(Y_l2)
           # print(paste(c("old column names"), paste(colnames(Y_l2), collapse = ' '), sep = ' '))
                  Y_l2 = Y_l2[,-which(colnames(Y_l2) %in% c(paste0('S',vars), vars))] 
            print(head(Y_l2))}
            #print(paste("new column names", paste(colnames(Y_l2), collapse = ' '), sep = ' '))}
        
        # Resume merge
        fData = data.frame( Y_l2 , row.names=NULL )
        fData$cyearS = paste0(fData$ccodeS, fData$year)
        fData$cyearR = paste0(fData$ccodeR, fData$year)        
        for(var in vars){
            fData$tmp = Y.imp.s[,var][match(fData$cyearS, Y.imp.s$id)] 
            stopifnot( sum( is.na(fData$tmp) )==0 )
            names(fData)[ncol(fData)] = paste0('S',var) }
        for(var in vars){
            fData$tmp = Y.imp.s[,var][match(fData$cyearR, Y.imp.s$id)] 
            stopifnot( sum( is.na(fData$tmp) )==0 )
            names(fData)[ncol(fData)] = var }

        # Cleanup level 2 data of extraneous vars
        fData = fData[,-which(names(fData) %in% c('cyearS', 'cyearR'))]
        Y_l2 = as.matrix( fData )

        # Some checks and bookkeeping set up before running mcmc for level 2
        if(ns==1){
                S0_l2 = diag(dim(Y_l2)[2])
                n0_l2 = dim(Y_l2)[2] + 2
                plugin.marginal_l2 = (apply(Y_l2, 2, function(x) { length(unique(x)) }) > plugin.threshold)                    
                ok_S0_l2 <- all(eigen(S0_l2)$val > 0) & dim(S0_l2)[1] == dim(Y_l2)[2] & dim(S0_l2)[2] == dim(Y_l2)[2]
                ok_n0_l2 <- (n0_l2 >= 0)
                if (!ok_S0_l2) { cat("Error: S0_l2 must be a positive definite p x p matrix in Level 2 \n") }
                if (!ok_n0_l2) { cat("Error: n0_l2 must be positive in Level 2 \n") }
                vnames_l2 <- colnames(Y_l2)
                colnames(Y_l2) <- vnames_l2
                n_l2 <- dim(Y_l2)[1]
                p_l2 <- dim(Y_l2)[2]
                set.seed(seed)
                R_l2 <- NULL 
                for (j in 1:p_l2) { R_l2 <- cbind(R_l2, match(Y_l2[, j], sort(unique(Y_l2[, j])))) }
                Rlevels_l2 <- apply(R_l2, 2, max, na.rm = TRUE)
                Ranks_l2 <- apply(Y_l2, 2, rank, ties.method = "max", na.last = "keep")
                N_l2 <- apply(!is.na(Ranks_l2), 2, sum)
                U_l2 <- t(t(Ranks_l2)/(N_l2 + 1))
                Z_l2 <- qnorm(U_l2)
                Zfill_l2 <- matrix(rnorm(n_l2 * p_l2), n_l2, p_l2)
                Z_l2[is.na(Y_l2)] <- Zfill_l2[is.na(Y_l2)]
                S_l2 <- cov(Z_l2)
                LPC_l2 <- NULL
                C.psamp_l2 <- array(dim = c(p_l2, p_l2, floor(nsamp/odens)))
                Y.imp_l2 <- array(dim = c(n_l2, p_l2, floor(nsamp/odens)), dimnames=list(NULL, colnames(Y_l2), NULL) )
                dimnames(C.psamp_l2) <- list(colnames(Y_l2), colnames(Y_l2), 1:floor(nsamp/odens))
        }

        # Begin imputation process for level 2 data + imputed level 1 data

        # Run through variables
        for (j in sample(1:p_l2)) {
            Sjc_l2 <- S_l2[j, -j] %*% solve(S_l2[-j, -j])
            sdj_l2 <- sqrt(S_l2[j, j] - S_l2[j, -j] %*% solve(S_l2[-j, -j]) %*% S_l2[-j, j])
            muj_l2 <- Z_l2[, -j] %*% t(Sjc_l2)
            if (!plugin.marginal_l2[j]) {
              for (r in 1:Rlevels_l2[j]) {
                ir_l2 <- (1:n_l2)[R_l2[, j] == r & !is.na(R_l2[, j])]
                lb_l2 <- suppressWarnings(max(Z_l2[R_l2[, j] == r - 1, j], na.rm = TRUE))
                ub_l2 <- suppressWarnings(min(Z_l2[R_l2[, j] == r + 1, j], na.rm = TRUE))
                Z_l2[ir_l2, j] <- qnorm(runif(length(ir_l2), pnorm(lb_l2, 
                  muj_l2[ir_l2], sdj_l2), pnorm(ub_l2, muj_l2[ir_l2], sdj_l2)), muj_l2[ir_l2], sdj_l2)
              }
            }
            ir_l2 <- (1:n_l2)[is.na(R_l2[, j])]
            Z_l2[ir_l2, j] <- rnorm(length(ir_l2), muj_l2[ir_l2], sdj_l2)
        }

        # Pull out S and save if at the right ns oden combination
        S_l2 <- solve(rwish(solve(S0_l2 * n0_l2 + t(Z_l2) %*% Z_l2), n0_l2 + n_l2))
        if (ns%%odens == 0) {
            C_l2 <- S_l2/(sqrt(diag(S_l2)) %*% t(sqrt(diag(S_l2))))
            lpc_l2 <- ldmvnorm(Z_l2 %*% diag(1/sqrt(diag(S_l2))), C_l2)
            LPC_l2 <- c(LPC_l2, lpc_l2)
            C.psamp_l2[, , ns/odens] <- C_l2
            Y.imp.s_l2 <- Y_l2
            for (j in 1:p_l2) {
                Y.imp.s_l2[is.na(Y_l2[, j]), j] <- quantile(Y_l2[, j], 
                    pnorm(Z_l2[is.na(Y_l2[, j]), j], 0, sqrt(S_l2[j,j])), na.rm = TRUE, type = 1) }
            Y.imp_l2[, , ns/odens] <- Y.imp.s_l2
        }

        # Print stuff 
        if (verb == TRUE & (ns%%(odens * 10)) == 0) {
            cat(round(100 * ns/nsamp), "percent done ", date(), "\n") }
    }

    # Organize and save
    # Y.impute = Y.imp, # Dont save level 1 imputations...redundant?
    G.ps <- list(
        C.psamp = C.psamp, LPC = LPC,
        C.psamp_l2 = C.psamp_l2, Y.impute_l2 = Y.imp_l2, LPC_l2 = LPC_l2
        )
    class(G.ps) <- "psgc"
    return(G.ps)
}
