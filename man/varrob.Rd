\name{VarRob}
\alias{varrob}


\title{Robust variance}
\description{Compute a robust variance}

\usage{
varrob(x,h,D=NULL,kernel="gaussien")
}

\arguments{
\item{x}{Matrix  / data frame}
\item{h}{Scalar: bandwidth of the Kernel}
\item{kernel}{The kernel used. This must be one of '"gaussien"', 
'"quartic"', '"triweight"', '"epanechikov"' , 
'"cosinus"' or '"uniform"' } 
\item{D}{A product scalar matrix / une matrice de produit scalaire}
}

\details{

\code{U} compute robust variance. \eqn{U_n^{-1} = S_n^{-1} - 1/h V_n^{-1}}{1/Un
 = 1/Sn - 1 / (h Vn)}


\deqn{S_n=\frac{\sum_{i=1}^{n}K(||X_i||_{V_n^{-1}}/h)(X_i-\mu_n)(X_i-\mu_n)'}{\sum_{i=1}^nK(||X_i||_{V_n^{-1}}/h)}}{S: see latex doc}

with \eqn{\mu_n} estimator of the mean.

%\eqn{\delta=K(|| X_i-X_j||)} with K: a kernel.

\code{K} compute a kernel.

}


\value{
A matrix
}

\references{
  H. Caussinus, S. Hakam, A. Ruiz-Gazen
  Projections revelatrices controlees: groupements et structures
  diverses.
  2002, to appear in Rev. Statist. Appli.
}


\keyword{multivariate}

\author{Antoine Lucas}
\seealso{\link{acp} \link[stats]{princomp} }
