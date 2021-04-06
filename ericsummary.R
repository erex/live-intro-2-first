ericsummary <- function (x, ...) 
{
    dht.obj <- x$dht
    model <- x$ddf
    x <- x$ds
    print.tables <- function(x, bysample) {
        cat("\nSummary statistics:\n")
        print(x$summary)
        if ("N" %in% names(x)) {
            cat("\nAbundance:\n")
            print(x$N)
        }
        cat("\nDensity:\n")
        print(x$D)
        if (bysample) {
            cat("\nEstimates by sample:\n")
            print(x$bysample)
        }
    }
    cat("\nSummary for distance analysis \n")
    cat("Number of observations : ", x$n, "\n")
    cat("Distance range         : ", x$left, " - ", 
        x$width, "\n")
    cat("\nModel :", Distance:::model.description(model), "\n")
    if (x$mono & x$mono.strict) {
        cat("\nStrict monotonicity constraints were enforced.\n")
    }
    else if (x$mono) {
        cat("\nMonotonicity constraints were enforced.\n")
    }
    cat("AIC   :", x$aic, "\n")
    cat("\nDetection function parameters\n")
    cat("Scale coefficient(s): ", "\n")
    cat(x$coeff$key.scale)
    if (x$key %in% c("gamma", "hr")) {
        cat("\nShape coefficient(s): ", "\n")
        cat(x$coeff$key.shape)
    }
    if (!is.null(x$coeff$adj.parm)) {
        cat("\nAdjustment term coefficient(s): ", "\n")
        cat(x$coeff$adj.parm)
    }
    cat("\n")
    if (!is.null(x$Nhat)) {
        parameters = data.frame(Estimate = c(x$average.p, x$Nhat))
        row.names(parameters) = c("Average p", "N in covered region")
        if (!is.null(x$average.p.se)) {
            parameters$SE = c(x$average.p.se, x$Nhat.se)
            parameters$CV = parameters$SE/parameters$Estimate
        }
    }
    else {
        parameters = data.frame(Estimate = c(x$average.p))
        row.names(parameters) = c("Average p")
        if (!is.null(x$average.p.se)) {
            parameters$SE = c(x$average.p.se)
            parameters$CV = parameters$SE/parameters$Estimate
        }
    }
    cat(parameters)
    x <- dht.obj
    if (!is.null(x)) {
        bysample <- FALSE
        if (is.null(x$clusters)) {
            print.tables(x$individuals, bysample)
        }
        else {
            cat("\nSummary for clusters\n")
            print.tables(x$clusters, bysample)
            cat("\nSummary for individuals\n")
            print.tables(x$individuals, bysample)
            cat("\nExpected cluster size\n")
            S <- x$Expected.S
            if (!is.null(S$se.Expected.S)) {
                S$cv.Expected.S <- S$se.Expected.S/S$Expected.S
                S$cv.Expected.S[S$Expected.S == 0] <- 0
            }
            print(S)
        }
    }
    invisible()
}