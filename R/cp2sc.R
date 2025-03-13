#' Implements the geometric spatial transfer of statistics from Spanish postal code areas to census sections
#'
#' @description Transfers the statistics available in a set of Spanish postal codes to the corresponding
#'              spatial set of Spanish official census sections into force in a given year.
#'
#' @author Jose M. Pavia, \email{pavia@@uv.es}
#' @author Virgilio Perez \email{virgilio.perez@@uv.es}
#' @references Goerlich, FJ (2022). Elaboracion de un mapa de codigos postales de Espana con recursos libres. Como evitar pagar a Correos 6000 euros por informacion de referencia. Working Papers Ivie n. 2022-3. Valencia: Ivie. \doi{10.12842/WPIVIE_0322}
#' @references Pavia, JM and Cantarino, I (2017a). Can dasymetric mapping significantly improve population data reallocation in a dense urban area? *Geographical Analysis*, 49(2), 155-174. \doi{10.1111/gean.12112}
#' @references Pavia, JM and Cantarino, I (2017b). Dasymetric distribution of votes in a dense city. *Applied Geography*, 86, 22-31. \doi{10.1016/j.apgeog.2017.06.021}
#' @references Perez, V and Pavia, JM (2024a). Improving Accuracy in Geospatial Information Transfer: A Population Density-Based Approach, in *6th International Conference on Advanced Research Methods and Analytics (CARMA 2024)*, Editorial Universitat Politecnica de Valencia, pp. 326-333. \doi{10.4995/CARMA2024.2024.17796}
#' @references Perez, V and Pavia, JM  (2024b) Automating the transfer of data between census sections and postal codes areas over time. An application to Spain. *Investigaciones Regionales - Journal of Regional Research*, forthcoming. \doi{10.38191/iirr-jorr.24.057}
#'
#' @param x A data frame of order N x K (with K > 1) with the statistics to be spatially transferred/imputed.
#'          The first column must contains the codes of the postal code areas to which the statistics belong to. The statistical nature
#'          of the data columns must be of the same type. See the argument `data.type`.
#'
#' @param year An integer number. Reference year of the census sections to which the statistics are going to be transferred.
#'             Only 2001 and 2003 to 2023 are allowed.
#'
#' @param data.type A character string indicating the type of data to be transferred, either `"counts"` (aggregate statistics)
#'                  or `"averages"` (mean, proportion or rate statistics). Default `"counts"`.
#'
#' @param all.units A `TRUE/FALSE` value indicating the census section units of the destination division to be included
#'                  in the output data frame. If `TRUE` all the units of the destination division are included. If `FALSE` only
#'                  those units for which a value is imputed are included. Default, `FALSE`.
#'
#' @param na.rm A `TRUE/FALSE` logical value indicating whether `NA` values should be stripped before
#'             the computations proceed. Default, `TRUE`.
#'
#' @param ... Other arguments to be passed to the function. Not currently used.
#'
#' @note The data that allows to transfer statistics among census sections
#'       and/or postal code areas has been own elaboration by the authors using (i)
#'       the Spanish Digital Cartography Files available in http://www.ine.es
#'       that contain the digitalisation of the georeferenced polygons of the
#'       census sections, according to UTM coordinates 28, 29, 30 and 31, and (ii)
#'       the Cartography File of postal code areas developed by Goerlich (2022).
#' @note Neither The Spanish Statistical Office (Instituto Nacional de Estadística) nor
#'       Professor Goerlich had any involvement in preparing this package. They bear no responsibility on the results derived from using this package.
#' @note Postal code areas have 2019 as reference year. It must be noted, however,
#'       that they can be considered as almost time stationary. Spanish postal code areas are quite
#'       stable over time.
#'
#' @return
#' A list with the following components
#'  \item{df}{ A data frame with the statistics spatially transferred to the census sections corresponding to the `year.sscc.dest` division.}
#'  \item{missing}{ A vector with the codes of the postal code areas included in `x` that are not available in the shp file of postal code area division.}
#'
#' @export
#'
#' @seealso \code{\link{sc2cp}} \code{\link{sc2sc}}
#' @importFrom stats aggregate
#'
#' @examples
#' data <- structure(list(CCPP = c(1120L, 1160L, 1250L, 1212L, 1213L),
#'                        income = c(15000L, 12000L, 11500L,
#'                        13000L, 12500L)),
#'                   class = "data.frame", row.names = c(NA, -5L))
#' example <- cp2sc(x = data, year = 2014, data.type = "averages")

cp2sc <- function(x,
                  year,
                  data.type = "counts",
                  all.units = FALSE,
                  na.rm = TRUE,
                  ...){

  if (!is.data.frame(x)){
    stop("ERROR: 'x' must be an object of class data.frame")
  } else {
    x <- as.data.frame(x)
  }

  #inputs <- c(as.list(environment()), list(...))
  if (!(year %in% c(2001L, 2003L:2023L)))
    stop("ERROR: The reference year for the census sections is not allowed. Only 2001 or 2003 to 2023 is allowed.")
  if (!(data.type %in% c("counts", "averages")))
    stop("ERROR: The argument 'data.type' is improper. Only 'counts' and 'averages' are allowed.")
  testeo <- test_ccpp_codes(bbdd  = x)
  bbdd <- testeo$bbdd

  # CCPP to SSCC 2019
  if (data.type == "counts"){
    transfer_function <- ccpp2sscc_total
  } else {
    transfer_function <- ccpp2sscc_average
  }
  bbdd <- transfer_function(bbdd = bbdd,
                            all.units = all.units,
                            na.rm = na.rm)

  # SSCC 2019 to SSCC year
  if (year != 2019L){
    years <- 2019L:year
    years <- years[years != 2002]

    if (data.type == "counts"){
      transfer_function <- impute_total
    } else {
      transfer_function <- impute_average
    }

    for (aa in 1L:(length(years) - 1L)){
      bbdd <- transfer_function(bbdd = bbdd,
                                y.origin = years[aa],
                                y.dest = years[aa + 1L],
                                all.units = all.units,
                                na.rm = na.rm)
    }
  }
  #return(list("df" = bbdd, "missing" = testeo$missing, "inputs" = inputs))
  return(list("df" = bbdd, "missing" = testeo$missing))
}
