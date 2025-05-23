% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cp2sc.R
\name{cp2sc}
\alias{cp2sc}
\title{Implements the geometric spatial transfer of statistics from Spanish postal code areas to census sections}
\usage{
cp2sc(x, year, data.type = "counts", all.units = FALSE, na.rm = TRUE, ...)
}
\arguments{
\item{x}{A data frame of order N x K (with K > 1) with the statistics to be spatially transferred/imputed.
The first column must contains the codes of the postal code areas to which the statistics belong to. The statistical nature
of the data columns must be of the same type. See the argument \code{data.type}.}

\item{year}{An integer number. Reference year of the census sections to which the statistics are going to be transferred.
Only 2001 and 2003 to 2023 are allowed.}

\item{data.type}{A character string indicating the type of data to be transferred, either \code{"counts"} (aggregate statistics)
or \code{"averages"} (mean, proportion or rate statistics). Default \code{"counts"}.}

\item{all.units}{A \code{TRUE/FALSE} value indicating the census section units of the destination division to be included
in the output data frame. If \code{TRUE} all the units of the destination division are included. If \code{FALSE} only
those units for which a value is imputed are included. Default, \code{FALSE}.}

\item{na.rm}{A \code{TRUE/FALSE} logical value indicating whether \code{NA} values should be stripped before
the computations proceed. Default, \code{TRUE}.}

\item{...}{Other arguments to be passed to the function. Not currently used.}
}
\value{
A list with the following components
\item{df}{ A data frame with the statistics spatially transferred to the census sections corresponding to the \code{year.sscc.dest} division.}
\item{missing}{ A vector with the codes of the postal code areas included in \code{x} that are not available in the shp file of postal code area division.}
}
\description{
Transfers the statistics available in a set of Spanish postal codes to the corresponding
spatial set of Spanish official census sections into force in a given year.
}
\note{
The data that allows to transfer statistics among census sections
and/or postal code areas has been own elaboration by the authors using (i)
the Spanish Digital Cartography Files available in http://www.ine.es
that contain the digitalisation of the georeferenced polygons of the
census sections, according to UTM coordinates 28, 29, 30 and 31, and (ii)
the Cartography File of postal code areas developed by Goerlich (2022).

Neither The Spanish Statistical Office (Instituto Nacional de Estadística) nor
Professor Goerlich had any involvement in preparing this package. They bear no responsibility on the results derived from using this package.

Postal code areas have 2019 as reference year. It must be noted, however,
that they can be considered as almost time stationary. Spanish postal code areas are quite
stable over time.
}
\examples{
data <- structure(list(CCPP = c(1120L, 1160L, 1250L, 1212L, 1213L),
                       income = c(15000L, 12000L, 11500L,
                       13000L, 12500L)),
                  class = "data.frame", row.names = c(NA, -5L))
example <- cp2sc(x = data, year = 2014, data.type = "averages")
}
\references{
Goerlich, FJ (2022). Elaboracion de un mapa de codigos postales de Espana con recursos libres. Como evitar pagar a Correos 6000 euros por informacion de referencia. Working Papers Ivie n. 2022-3. Valencia: Ivie. \doi{10.12842/WPIVIE_0322}

Pavia, JM and Cantarino, I (2017a). Can dasymetric mapping significantly improve population data reallocation in a dense urban area? \emph{Geographical Analysis}, 49(2), 155-174. \doi{10.1111/gean.12112}

Pavia, JM and Cantarino, I (2017b). Dasymetric distribution of votes in a dense city. \emph{Applied Geography}, 86, 22-31. \doi{10.1016/j.apgeog.2017.06.021}

Perez, V and Pavia, JM (2024a). Improving Accuracy in Geospatial Information Transfer: A Population Density-Based Approach, in \emph{6th International Conference on Advanced Research Methods and Analytics (CARMA 2024)}, Editorial Universitat Politecnica de Valencia, pp. 326-333. \doi{10.4995/CARMA2024.2024.17796}

Perez, V and Pavia, JM  (2024b) Automating the transfer of data between census sections and postal codes areas over time. An application to Spain. \emph{Investigaciones Regionales - Journal of Regional Research}, forthcoming. \doi{10.38191/iirr-jorr.24.057}
}
\seealso{
\code{\link{sc2cp}} \code{\link{sc2sc}}
}
\author{
Jose M. Pavia, \email{pavia@uv.es}

Virgilio Perez \email{virgilio.perez@uv.es}
}
