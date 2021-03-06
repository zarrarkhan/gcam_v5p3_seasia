% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/zchunk_X201.socioeconomic_Subregions_Thailand.R
\name{module_breakout_X201.socioeconomics_Subregions_Thailand}
\alias{module_breakout_X201.socioeconomics_Subregions_Thailand}
\title{module_breakout_X201.socioeconomics_Subregions_Thailand}
\usage{
module_breakout_X201.socioeconomics_Subregions_Thailand(command, ...)
}
\arguments{
\item{command}{API command to execute}

\item{...}{other optional parameters, depending on command}
}
\value{
Depends on \code{command}: either a vector of required inputs,
a vector of output names, or (if \code{command} is "MAKE") all
the generated outputs: \code{L201.InterestRate}, \code{L201.LaborForceFillout}, \code{L201.PPPConvert}, \code{L201.BaseGDP_Scen}, \code{L201.Pop_gSSP1}, \code{L201.Pop_gSSP2}, \code{L201.Pop_gSSP3}, \code{L201.Pop_gSSP4}, \code{L201.Pop_gSSP5}, \code{L201.Pop_SSP1}, \code{L201.Pop_SSP2}, \code{L201.Pop_SSP3}, \code{L201.Pop_SSP4}, \code{L201.Pop_SSP5}, \code{L201.LaborProductivity_gSSP1}, \code{L201.LaborProductivity_gSSP2}, \code{L201.LaborProductivity_gSSP3}, \code{L201.LaborProductivity_gSSP4}, \code{L201.LaborProductivity_gSSP5}, \code{L201.LaborProductivity_SSP1}, \code{L201.LaborProductivity_SSP2}, \code{L201.LaborProductivity_SSP3}, \code{L201.LaborProductivity_SSP4}, and \code{L201.LaborProductivity_SSP5}. The corresponding file in the
original data system was \code{L201.Pop_GDP_scenarios.R} (socioeconomics level2).
}
\description{
Population and GDP of City Chosen and Rest of Region
}
\details{
Produces default interest rate by region, historical and future population by region and SSP scenario,
and uses per-capita GDP to calculate labor productivity by region and scenario.
}
\author{
PK, ZK & NB April 2021
}
