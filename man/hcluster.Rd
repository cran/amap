\name{hcluster}
\title{Hierarchical Clustering}
\alias{hcluster}
\alias{hclusterpar}
\description{
  Hierarchical cluster analysis.
}
\usage{
hcluster(x, method = "euclidean", diag = FALSE, upper = FALSE,
         link = "complete", members = NULL, nbproc = 2,
         doubleprecision = TRUE)
}
\arguments{
  \item{x}{
    A numeric matrix of data, or an object that can be coerced to such a
    matrix (such as a numeric vector or a data frame with all numeric
    columns). Or an object of class "exprSet".
  }
  \item{method}{the distance measure to be used. This must be one of
    \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"},
    \code{"canberra"}, \code{"binary"}, \code{"pearson"},
     \code{"abspearson"},  \code{"correlation"},
    \code{"abscorrelation"}, \code{"spearman"} or \code{"kendall"}.
    Any unambiguous substring can be given.}
  \item{diag}{logical value indicating whether the diagonal of the
    distance matrix should be printed by \code{print.dist}.}
  \item{upper}{logical value indicating whether the upper triangle of the
    distance matrix should be printed by \code{print.dist}.}
  \item{link}{the agglomeration method to be used. This should
    be (an unambiguous abbreviation of) one of
    \code{"ward"}, \code{"single"}, \code{"complete"},
    \code{"average"}, \code{"mcquitty"}, \code{"median"} or
    \code{"centroid"},\code{"centroid2"}, \code{"ward.d2"}.}
  
  \item{members}{\code{NULL} or a vector with length size of \code{d}.}
  \item{nbproc}{integer, number of subprocess for parallelization [Linux
  & Mac only]}
  \item{doubleprecision}{True: use of double precision for distance
    matrix computation; False: use simple precision}
  
}
\value{
  An object of class \bold{hclust} which describes the
  tree produced by the clustering process.
  The object is a list with components:
  \item{merge}{an \eqn{n-1} by 2 matrix.
    Row \eqn{i} of \code{merge} describes the merging of clusters
    at step \eqn{i} of the clustering.
    If an element \eqn{j} in the row is negative,
    then observation \eqn{-j} was merged at this stage.
    If \eqn{j} is positive then the merge
    was with the cluster formed at the (earlier) stage \eqn{j}
    of the algorithm.
    Thus negative entries in \code{merge} indicate agglomerations
    of singletons, and positive entries indicate agglomerations
    of non-singletons.}

  \item{height}{a set of \eqn{n-1} non-decreasing real values.
    The clustering \emph{height}: that is, the value of
    the criterion associated with the clustering
    \code{method} for the particular agglomeration.}

  \item{order}{a vector giving the permutation of the original
    observations suitable for plotting, in the sense that a cluster
    plot using this ordering and matrix \code{merge} will not have
    crossings of the branches.}

  \item{labels}{labels for each of the objects being clustered.}

  \item{call}{the call which produced the result.}

  \item{method}{the cluster method that has been used.}

  \item{dist.method}{the distance that has been used to create \code{d}
    (only returned if the distance object has a \code{"method"}
    attribute).}

  There is a \code{\link{print}} and a \code{\link{plot}} method for
  \code{hclust} objects.
  The \code{plclust()} function is basically the same as the plot method,
  \code{plot.hclust}, primarily for back compatibility with S-plus. Its
  extra arguments are not yet implemented.
}
\details{
  This function is a mix of function \code{hclust} and function
   \code{dist}. \code{hcluster(x, method = "euclidean",link = "complete")
     = hclust(dist(x, method = "euclidean"),method = "complete"))}   
   It use twice less memory, as it doesn't store distance matrix.

   For more details, see documentation of \code{hclust} and \code{Dist}.

}

\note{Multi-thread (parallelisation) is disable on Windows.}

\author{
  The \code{hcluster} function is based on C code adapted from Cran
  Fortran routine
  by Antoine Lucas.
}
\seealso{
  \code{\link{Dist}},
  \code{\link[stats]{hclust}}, \code{\link[stats]{kmeans}}.
}

\references{
  Antoine Lucas and Sylvain Jasson, \emph{Using amap and ctc Packages
    for Huge Clustering}, R News, 2006, vol 6, issue 5 pages 58-60.
  }


\examples{

data(USArrests)
hc <- hcluster(USArrests,link = "ave")
plot(hc)
plot(hc, hang = -1)

## Do the same with centroid clustering and squared Euclidean distance,
## cut the tree into ten clusters and reconstruct the upper part of the
## tree from the cluster centers.
hc <- hclust(dist(USArrests)^2, "cen")
memb <- cutree(hc, k = 10)
cent <- NULL
for(k in 1:10){
  cent <- rbind(cent, colMeans(USArrests[memb == k, , drop = FALSE]))
}
hc1 <- hclust(dist(cent)^2, method = "cen", members = table(memb))
opar <- par(mfrow = c(1, 2))
plot(hc,  labels = FALSE, hang = -1, main = "Original Tree")
plot(hc1, labels = FALSE, hang = -1, main = "Re-start from 10 clusters")
par(opar)


## other combinaison are possible

hc <- hcluster(USArrests,method = "euc",link = "ward", nbproc= 1,
doubleprecision = TRUE)
hc <- hcluster(USArrests,method = "max",link = "single", nbproc= 2,
doubleprecision = TRUE)
hc <- hcluster(USArrests,method = "man",link = "complete", nbproc= 1,
doubleprecision = TRUE)
hc <- hcluster(USArrests,method = "can",link = "average", nbproc= 2,
doubleprecision = TRUE)
hc <- hcluster(USArrests,method = "bin",link = "mcquitty", nbproc= 1,
doubleprecision = FALSE)
hc <- hcluster(USArrests,method = "pea",link = "median", nbproc= 2,
doubleprecision = FALSE)
hc <- hcluster(USArrests,method = "abspea",link = "median", nbproc= 2,
doubleprecision = FALSE)
hc <- hcluster(USArrests,method = "cor",link = "centroid", nbproc= 1,
doubleprecision = FALSE)
hc <- hcluster(USArrests,method = "abscor",link = "centroid", nbproc= 1,
doubleprecision = FALSE)
hc <- hcluster(USArrests,method = "spe",link = "complete", nbproc= 2,
doubleprecision = FALSE)
hc <- hcluster(USArrests,method = "ken",link = "complete", nbproc= 2,
doubleprecision = FALSE)



}
\keyword{multivariate}
\keyword{cluster}
