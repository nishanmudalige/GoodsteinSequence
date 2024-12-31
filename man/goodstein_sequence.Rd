\name{goodstein_sequence}
\alias{goodstein_sequence}
\title{A FUNCTION WHICH EVALUATES THE TERMS OF A GOODSTEIN SEQUENCE}
\description{
A function which evaluates the terms of a Goodstein sequence until convergence, or until a specific desired iteration.
The hereditary base representation and the number of digits of the evaluated term in the sequence can also be provided.
}
\usage{
goodstein_sequence(num, iter, hb = T, digits = T)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{num}{The starting number of the sequence}
  \item{iter}{The number of iterations to evaluate}
  \item{hb}{Whether the hereditary base representation is required.}
  \item{digits}{Whether the number of terms of the term is required.}
}
\details{}
\value{}
\references{}
\author{Nishan Mudalige}
\note{}
\seealso{}
\examples{
goodstein_sequence(5, 3, TRUE, TRUE)
}
