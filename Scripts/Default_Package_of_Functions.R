include <- function(pkgName) {
  if (!(pkgName %in% row.names(installed.packages()))) {
    install.packages(pkgName)
  }
}

include("mise")
include("matrixStats")

suppressWarnings(origpar <- par())

suppressWarnings(origpar$cin <- origpar$cra <- origpar$csi <- origpar$cxy <- origpar$din <- origpar$page <- NULL)

clear <- function(vars=FALSE,...) {
  mise::mise(vars = vars,...)
}

euclidean <- function(vector) {
  (sum(vector^2))^0.5
}

substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

PermuteMatrix <- function(matrix) {
  matrix(sample(matrix), nrow = nrow(matrix))
}

## Show the shapes of all possible scatter plot points.
show_pch <- function(){
  X <- rep(1:5, 5)
  Y <- c( rep(5,5), rep(4,5), rep(3,5), rep(2,5), rep(1,5))
  par( mar=c(1, 1, 3, 1))
  plot( X, Y, type='n', xlab="", ylab="", main="PCH 21-25 are fillable", yaxt='n', xaxt='n', ylim=c(0,5) )
  for (i in 1:25){
    points( X[i], Y[i], pch=i, cex=3 )
    text( X[i], Y[i]-0.5, as.character(i) )
  }
}

unstratified.jackknife.sample <- function(data, n) {
  k <- nrow(data)%/%n + 1
  r <- sample.int(n, 1)
  sys.sample <- seq(r, r+k*(n-1), k)
  jack.sample <- data[sys.sample, ]
  jack.sample
}

simulateDataset <- function(k, size) {
  simulatedDataset <- data.frame(X = rep(NA, k*size), Y = rep(NA, k*size))
  x.means <- runif(k)
  y.means <- runif(k)
  sdval <- 1/(k^2)
  for (i in 0:(k-1)) {
    simulatedDataset[(1+i*size):((i+1)*size), 1] <- rnorm(size, mean = x.means[i+1], sd = sdval)
    simulatedDataset[(1+i*size):((i+1)*size), 2] <- rnorm(size, mean = y.means[i+1], sd = sdval)
  }
  return(list(data = simulatedDataset, k = k, size = size))
}

napply <- function(X, MARGINname, FUN, ...) {
  if (MARGINname == "rows") {
    MARGINname <- 1
  }
  else if (MARGINname == "cols") {
    MARGINname <- 2
  }
  else {
    stop("Invalid value for MARGINname, must be 'rows' or 'cols'.")
  }
  apply(X, MARGIN = MARGINname, FUN, ...)
}

monstrsplit <- function(x, split, ...) {
  strsplit(x, split, ...)[[1]]
}

cleanTable <- function(x) {
  y = as.data.frame(cbind(names(x), unname(x)))
  y[ , 2] <- as.integer(y[ , 2]) 
  colnames(y) <- c("Word", "Frequency") 
  y
}

clear()
