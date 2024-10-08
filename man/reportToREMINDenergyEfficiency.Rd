% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/reportToREMINDenergyEfficiency.R
\name{reportToREMINDenergyEfficiency}
\alias{reportToREMINDenergyEfficiency}
\title{Report to REMIND f35_fe2es}
\usage{
reportToREMINDenergyEfficiency(
  fleetFEdemand,
  fleetESdemand,
  hybridElecShare,
  timeResReporting,
  demScen,
  SSPscen,
  transportPolScen,
  helpers
)
}
\arguments{
\item{fleetFEdemand}{final energy demand on fleet level}

\item{fleetESdemand}{energy service demand on fleet level}

\item{hybridElecShare}{share of electric driving for hybrid electric vehicles}

\item{timeResReporting}{time resolution reporting}

\item{demScen}{demand scenario}

\item{SSPscen}{SSP scenario}

\item{transportPolScen}{transport policy scenario}

\item{helpers}{list of helpers}
}
\value{
Energy efficiency of transport fuel technologies in [trn pkm/Twa or trn tkm/Twa]
}
\description{
Report to REMIND f35_fe2es
}
\author{
Johanna Hoppe
}
