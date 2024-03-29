% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/lower_level_disprop_analysis.R
\name{prr}
\alias{prr}
\title{Proportional Reporting Rate}
\usage{
prr(
  obs = NULL,
  n_drug = NULL,
  n_event_prr = NULL,
  n_tot_prr = NULL,
  conf_lvl = 0.95
)
}
\arguments{
\item{obs}{Number of reports for the specific drug and event (i.e. the
observed count).}

\item{n_drug}{Number of reports with the drug of interest.}

\item{n_event_prr}{Number of reports with the event in the background.}

\item{n_tot_prr}{Number of reports in the background.}

\item{conf_lvl}{Confidence level of confidence or credibility intervals.
Default is 0.95 (i.e. 95 \% confidence interval).}
}
\value{
A tibble with three columns (point estimate and credibility bounds).
Number of rows equals length of inputs obs, n_drug, n_event_prr and n_tot_prr.
}
\description{
Calculates Proportional Reporting Rate ("PRR") with
confidence intervals, used in disproportionality analysis.
}
\details{
The PRR is the proportion of reports with an event in set of exposed
cases, divided with the proportion of reports with the event in a background
or comparator, which does not include the exposed.

The PRR is estimated from a observed-to-expected ratio, based on
similar to the RRR and IC, but excludes the exposure of interest from the
comparator.

\deqn{\hat{PRR} = \frac{\hat{O}}{\hat{E}}}

where \eqn{\hat{O}} is the observed number of reports, and expected \eqn{\hat{E}}
is estimated as

\deqn{\hat{E} = \frac{\hat{N}_{drug} \times (\hat{N}_{event} - \hat{O})}{\hat{N}_{TOT}-\hat{N}_{drug}}}

where \eqn{\hat{N}_{drug}}, \eqn{\hat{N}_{event}}, \eqn{\hat{O}} and \eqn{\hat{N}_{TOT}} are
the number of reports with the drug, the event, the drug and event, and
in the whole database respectively.

A confidence interval is derived in Gravel (2009) using the delta method:
\deqn{\hat{s} = \sqrt{ 1/\hat{O} - 1/(\hat{N}_{drug}) + 1/(\hat{N}_{event} - \hat{O}) - 1/(\hat{N}_{TOT} - \hat{N}_{drug})}}

and \deqn{[\hat{CI}_{\alpha/2}, \hat{CI}_{1-\alpha/2}] = }
\deqn{[\frac{\hat{O}}{\hat{E}} \times \exp(Q_{\alpha/2} \times \hat{s}),
\frac{\hat{O}}{\hat{E}} \times \exp(Q_{1-\alpha/2} \times \hat{s})]}

where \eqn{Q_{\alpha}} denotes the quantile function of a
standard Normal distribution at significance level \eqn{\alpha}.

Note: For historical reasons, another version of this standard deviation is sometimes used
where the last fraction under the square root is added rather than subtracted,
with negligible practical implications in large databases. This function uses the version
declared above, i.e. with subtraction.
}
\examples{
prr(
  obs = 5,
  n_drug = 10,
  n_event_prr = 20,
  n_tot_prr = 10000
)


# Note that input parameters can be vectors (of equal length, no recycling)
pvda::prr(
  obs = c(5, 10),
  n_drug = c(10, 20),
  n_event_prr = c(15, 30),
  n_tot_prr = c(10000, 10000)
)
}
\references{
\insertRef{Montastruc_2011}{pvda}

\insertRef{MscThesis}{pvda}
}
