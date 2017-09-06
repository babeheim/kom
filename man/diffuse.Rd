
\name{diffuse}
\alias{diffuse}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{Diffusion simulation}
\description{
  Simulates diffusion of a binary trait through a population.
}
\usage{
diffuse( init_list, pop_data, quiet=TRUE, cognition="additive" )
}
\arguments{
    \item{init_list}{list of simulation parameters and conditions at initialization}
    \item{pop_data}{a database of individuals and households for the simulator to use}
    \item{quiet}{if FALSE, will print a message every time a simulated census is collected}
    \item{cognition}{specifies the basic model form of the probabilistic decision rule for each agent who is able to adopt the trait}
}
\details{
    This function will run a simple agent-based simulation of the trait diffusing within a specified population, and collect data on that population at periodic intervals that is returned as output.
}
%- maybe also 'usage' for other objects documented here.
\references{}
\author{Bret Beheim}
\examples{

  data(hort_village)

  init_list <- list(
    n_seed_individuals = 5,
    seed_network_location = "random",
    simulation_duration_years = 1,
    phi_radius = 500,
    baseline_probability = 1e-05,
    kin_network_effect = 5,
    town_distance_effect = 0,
    neighbor_effect = 5,
    wealth_effect = 5,
    observation_rate = 30
  )

  diffuse( init_list, hort_village, cognition="additive" )

}
% Add one or more standard keywords, see file 'KEYWORDS' in the
% R documentation directory.
\keyword{ }