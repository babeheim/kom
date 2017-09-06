
\name{create_risk_table}
\alias{create_risk_table}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Create a risk table}
\description{
  Create a table of those at risk for adoption of the trait and whether or not they actually adopted it. Creates the risk table used in the model fitting process, from the larger census records.
}
\usage{
create_risk_table( init_list, pop_data, quiet=TRUE, cognition="additive" )
}
\arguments{
    \item{init_list}{list of simulation parameters and conditions at initialization}
    \item{pop_data}{a database of individuals and households for the simulator to use}
    \item{history}{a data frame containing census records, the output of \code{diffuse}}
}
\details{
    This function prepares a data frame for analysis using binomial regression models, including logistic regression ("additive") and more exotic models.
}
%- maybe also 'usage' for other objects documented here.
\references{}
\author{Bret Beheim}
\examples{

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ }