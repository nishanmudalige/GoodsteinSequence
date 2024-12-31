\name{hereditary_base_string}
\alias{hereditary_base_string}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
A FUNCTION TO EVALUATE THE HEREDITARY BASE REPRESENTATION OF A POSITIVE INTEGER GIVEN A BASE
}
\description{
A function which evaluates the hereditary base representation of a positive integer for a given base.
}
\usage{
hereditary_base_string(number, base)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{number}{A Positive integer.}
  \item{base}{A base}
}
\value{A string with the hereditary base representation for the given base}
\author{Nishan Mudalige}
\examples{
## hereditary_base_string(10, 2)
##  "2^(2 + 1) + 2"

## hereditary_base_string(2000, 5)
##  "3*5^(4) + 5^(3)"
}