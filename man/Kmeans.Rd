\name{Kmeans}
\alias{Kmeans}
\title{
K-Means Clustering
}
\description{
  Perform k-means clustering on a data matrix.
}
\usage{
Kmeans(x, centers, iter.max = 10, nstart = 1,
         method = "euclidean")
}
\arguments{
  \item{x}{
    A numeric matrix of data, or an object that can be coerced to such a
    matrix (such as a numeric vector or a data frame with all numeric columns).
  }
  \item{centers}{
    Either the number of clusters or a set of initial cluster centers.
    If the first, a random set of rows in \code{x} are chosen as the initial
    centers.
  }
  \item{iter.max}{
    The maximum number of iterations allowed.
  }
  \item{nstart}{If \code{centers} is a number, how many random sets
    should be chosen?}
  \item{method}{the distance measure to be used. This must be one of
    \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"},
    \code{"canberra"}, \code{"binary"}, \code{"pearson"} or
    \code{"correlation"}.
    Any unambiguous substring can be given.}
}
\details{
  The data given by \code{x} is clustered by the k-means algorithm.
  When this terminates, all cluster centres are at the mean of
  their Voronoi sets (the set of data points which are nearest to
  the cluster centre).

  The algorithm of Lloyd--Forgy is used; method="euclidean" should
  return same result as with function \link[stat]{kmeans}.

  An objective: to allow NA values.
}
\value{
  A list with components:

  \item{cluster}{
    A vector of integers indicating the cluster to which each point is
    allocated.
  }
  \item{centers}{A matrix of cluster centres.}
  \item{withinss}{The within-cluster sum of square distances for each cluster.}
  \item{size}{The number of points in each cluster.}
}

\seealso{
  \code{\link{hcluster}},\code{\link[stats]{kmeans}}.
}


\keyword{multivariate}
\keyword{cluster}