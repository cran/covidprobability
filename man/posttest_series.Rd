% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/functions.R
\name{posttest_series}
\alias{posttest_series}
\title{Calculate post-test probability if testing occurred on each day in a series}
\usage{
posttest_series(pre0, asympt, days = 14, mu = 1.63, sigma = 0.5, sens, spec)
}
\arguments{
\item{pre0}{The pretest probability on day 0 (at exposure)}

\item{asympt}{The proportion of infected patients expected to remain
asymptomatic throughout the course of infection}

\item{days}{Days since exposure for calculation range}

\item{mu}{The mean of a lognormal distribution that approximates the
incubation period for COVID-19. E.g. 1.63 (see reference).}

\item{sigma}{The standard deviation of a lognormal distribution that
approximates the incubation period for COVID-19. E.g. 0.5
(see reference).}

\item{sens}{A vector of sensitivities by day since exposure}

\item{spec}{The test specificity}
}
\value{
A vector of posttest probabilities
}
\description{
Given an initial pretest probability, and assuming symptoms never arise, with
each passing day the pretest probability will be lower, given the person did
not experience symptoms. This returns a vector of posttest probabilities
which takes all of the above into account, assuming a negative test on each
day. Note this is not a time series, and does not reflect if serial testing
were done each day and assumes testing was only done once.
}
