\name{Dist}
\title{Distance Matrix Computation}
\usage{
Dist(x, method = "euclidean", nbproc = 2, diag = FALSE, upper = FALSE)
}
\alias{Dist}

\arguments{
  \item{x}{numeric matrix or (data frame) or an object of class
    "exprSet".
    Distances between the rows of
    \code{x} will be computed.}
  \item{method}{the distance measure to be used. This must be one of
    \code{"euclidean"}, \code{"maximum"}, \code{"manhattan"},
    \code{"canberra"}, \code{"binary"}, \code{"pearson"},
     \code{"abspearson"},  \code{"correlation"},
    \code{"abscorrelation"}, \code{"spearman"} or \code{"kendall"}.
    Any unambiguous substring can be given.}
  \item{nbproc}{integer, Number of subprocess for parallelization}
  \item{diag}{logical value indicating whether the diagonal of the
    distance matrix should be printed by \code{print.dist}.}
  \item{upper}{logical value indicating whether the upper triangle of the
    distance matrix should be printed by \code{print.dist}.}
}
\description{
  This function computes and returns the distance matrix computed by
  using the specified distance measure to compute the distances between
  the rows of a data matrix.
}
\details{
  Available distance measures are (written for two vectors \eqn{x} and
  \eqn{y}):
  \describe{
    \item{\code{euclidean}:}{Usual square distance between the two
      vectors (2 norm).}

    \item{\code{maximum}:}{Maximum distance between two components of \eqn{x}
      and \eqn{y} (supremum norm)}

    \item{\code{manhattan}:}{Absolute distance between the two vectors
      (1 norm).}

    \item{\code{canberra}:}{\eqn{\sum_i |x_i - y_i| / |x_i + y_i|}{%
	sum(|x_i - y_i| / |x_i + y_i|)}.  Terms with zero numerator and
      denominator are omitted from the sum and treated as if the values
      were missing.
    }

    \item{\code{binary}:}{(aka \emph{asymmetric binary}): The vectors
      are regarded as binary bits, so non-zero elements are `on' and zero
      elements are `off'.  The distance is the \emph{proportion} of
      bits in which only one is on amongst those in which at least one is on.}

    \item{\code{pearson}:}{Also named "not centered Pearson"
      \eqn{1 - \frac{\sum_i x_i y_i}{\sqrt{\sum_i x_i^2 %
	   \sum_i y_i^2}}}{%
	1 - sum(x_i y_i) / sqrt [sum(x_i^2) sum(y_i^2)]}.
    }

    \item{\code{abspearson}:}{Absolute Pearson
      \eqn{1 - \left| \frac{\sum_i x_i y_i}{\sqrt{\sum_i x_i^2 %
	   \sum_i y_i^2}} \right| }{%
	1 - |sum(x_i y_i) / sqrt [sum(x_i^2) sum(y_i^2)] |}.
    }

    \item{\code{correlation}:}{Also named "Centered Pearson"
      \eqn{1 - corr(x,y)}.  
    }
    
    \item{\code{abscorrelation}:}{Absolute correlation
      \eqn{1 - | corr(x,y) |}
      with

      \eqn{ corr(x,y) = \frac{\sum_i x_i y_i -\frac1n \sum_i x_i \sum_i%
y_i}{% frac: 2nd part
\sqrt{\left(\sum_i x_i^2 -\frac1n \left( \sum_i x_i \right)^2 %
\right)%
\left( \sum_i  y_i^2 -\frac1n \left( \sum_i y_i \right)^2 %
\right)}  }}.  
    }

    \item{\code{spearman}:}{Compute a distance based on rank.
      \eqn{\sum(d_i^2)}{sum (d_i^2)} where \eqn{d_i} is the difference
      in rank between \eqn{x_i} and \eqn{y_i}.
      
      \code{Dist(x,method="spearman")[i,j] =}
	
      \code{cor.test(x[i,],x[j,],method="spearman")$statistic}
	
    }

    \item{\code{kendall}:}{Compute a distance based on rank.
      \eqn{\sum_{i,j} K_{i,j}(x,y)} with \eqn{K_{i,j}(x,y)}
      is 0 if \eqn{x_i, x_j} in same order as \eqn{y_i,y_j},
      1 if not.
    }
  }

  Missing values are allowed, and are excluded from all computations
  involving the rows within which they occur.  If some columns are
  excluded in calculating a Euclidean, Manhattan or Canberra distance,
  the sum is scaled up proportionally to the number of columns used.
  If all pairs are excluded when calculating a particular distance,
  the value is \code{NA}.

  The functions \code{as.matrix.dist()} and \code{as.dist()} can be used
  for conversion between objects of class \code{"dist"} and conventional
  distance matrices and vice versa.
}
\value{
  An object of class \code{"dist"}.

  The lower triangle of the distance matrix stored by columns in a
  vector, say \code{do}. If \code{n} is the number of
  observations, i.e., \code{n <- attr(do, "Size")}, then
  for \eqn{i < j <= n}, the dissimilarity between (row) i and j is
  \code{do[n*(i-1) - i*(i-1)/2 + j-i]}.
  The length of the vector is \eqn{n*(n-1)/2}, i.e., of order \eqn{n^2}.

  The object has the following attributes (besides \code{"class"} equal
  to \code{"dist"}):
  \item{Size}{integer, the number of observations in the dataset.}
  \item{Labels}{optionally, contains the labels, if any, of the
    observations of the dataset.}
  \item{Diag, Upper}{logicals corresponding to the arguments \code{diag}
    and \code{upper} above, specifying how the object should be printed.}
  \item{call}{optionally, the \code{\link{call}} used to create the
    object.}
  \item{methods}{optionally, the distance method used; resulting form
    \code{\link{dist}()}, the (\code{\link{match.arg}()}ed) \code{method}
    argument.}
}
\references{
  Mardia, K. V., Kent, J. T. and Bibby, J. M. (1979)
  \emph{Multivariate Analysis.} London: Academic Press.

  Wikipedia
  \url{https://en.wikipedia.org/wiki/Kendall_tau_distance}
  
}
\note{Multi-thread (parallelisation) is disable on Windows.}

\seealso{
  \code{\link[cluster]{daisy}} in the \file{cluster} package with more
  possibilities in the case of \emph{mixed} (contiuous / categorical)
  variables.
 \code{\link[stats]{dist}}  \code{\link{hcluster}}.
}
\examples{
x <- matrix(rnorm(100), nrow=5)
Dist(x)
Dist(x, diag = TRUE)
Dist(x, upper = TRUE)


## compute dist with 8 threads
Dist(x,nbproc=8)


Dist(x,method="abscorrelation")
Dist(x,method="kendall")

}
\keyword{multivariate}
\keyword{cluster}
