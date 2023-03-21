#' Implements the geometric spatial transfer of statistics among Spanish census sections corresponding to two different spatial divisions
#'
#' @description  Spatially transfers the statistics available in a set of Spanish census sections corresponding to
#'               the division into force in a given year to the census sections of another division
#'               with reference in another year.
#'
#' @author Jose M. Pavia, \email{pavia@@uv.es}
#' @author Virgilio Perez \email{virgilio.perez@@uv.es}
#' @references Pavia, JM and Cantarino, I (2017a). Can dasymetric mapping significantly improve population data reallocation in a dense urban area? *Geographical Analysis*, 49(2), 155-174. \doi{10.1111/gean.12112}
#' @references Pavia, JM and Cantarino, I (2017b). Dasymetric distribution of votes in a dense city. *Applied Geography*, 86, 22-31. \doi{10.1016/j.apgeog.2017.06.021}
#'
#' @param x A data frame of order N x K (with K > 1) with the statistics to be spatially transferred/imputed.
#'          The first column must contains the codes of the census sections to which the statistics belong to. The statistical nature
#'          of the data columns must be of the same type. See the argument `data.type'. `
#'
#' @param year.sscc.origin An integer number. Reference year of the census sections included in the first column of `x`.
#'                         Only 2001 and 2003 to 2022 are allowed.
#'
#' @param year.sscc.dest An integer number. Reference year of the census sections to which the statistics are going to be transferred.
#'                       Only 2001 and 2003 to 2022 are allowed and it must be different than `year.sscc.origin`.
#'
#' @param data.type A character string indicating the type of data to be transferred, either `"counts"` (aggregate statistics)
#'                  or `"averages"` (mean, proportion or rate statistics). Default `"counts"`.
#'
#' @param all.units A `TRUE/FALSE` value indicating the census section units of the destination division to be included
#'                  in the output data frame. If `TRUE` all the units of the destination division are included. If `FALSE` only
#'                  those units for which a value is imputed are included. Default, `FALSE`.
#'
#' @param ... Other arguments to be passed to the function. Not currently used.
#'

#'
#' @return
#' A list with the following components
#'  \item{df}{ A data frame with the statistics spatially transferred to the census sections corresponding to the `year.sscc.dest` division.}
#'  \item{missing}{ A vector with the codes of the census sections included in `x` that are not available in the shp file of census sections corresponding to the `year.sscc.origin` division.}
#'
#' @note The data that allows to transfer throughout time statistics among census sections
#'       has been own elaboration by the authors using the Spanish Digital Cartography Files
#'        in http://www.ine.es that contain the digitalisation of the georeferenced polygons
#'        of the census sections, according to UTM coordinates 28, 29, 30 and 31.
#' @note  The Spanish Statistical Office (Instituto Nacional de Estadistica) had any
#'        involvement in preparing this package. They bear no responsibility on the results
#'        derived from using this package.
#'
#' @export
#'
#' @seealso \code{\link{sc2cp}} \code{\link{cp2sc}}
#' @importFrom stats aggregate
#'
#' @examples
#' data <- structure(list(SSCC = c(3403601001, 3403701001, 3403801001, 3403901001,
#'                                 3404101001, 3404201001, 3404501001, 3404601001,
#'                                 3404701001, 3404701002, 3404801001),
#'                        X15.19 = c(4L, 7L, 13L, 0L, 0L, 13L, 1L, 5L, 30L, 48L, 1L),
#'                        X20.24 = c(5L, 5L, 9L, 0L, 2L, 12L, 2L, 1L, 34L, 61L, 3L)),
#'                        row.names = 1:11, class = "data.frame")
#' example <- sc2sc(x = data, year.sscc.origin = 2020, year.sscc.dest = 2014)

sc2sc <- function(x,
                  year.sscc.origin,
                  year.sscc.dest,
                  data.type = "counts",
                  all.units = FALSE,
                  ...){

 # inputs <- c(as.list(environment()), list(...))
  test_years(year.sscc.origin, year.sscc.dest)
  if (!(data.type %in% c("counts", "averages")))
    stop("ERROR: The argument 'data.type' is improper. Only 'counts' and 'averages' are allowed.")

    years <- year.sscc.origin:year.sscc.dest
    years <- years[years != 2002]

  testeo <- test_sscc_codes(bbdd  = x,
                            y.origin = year.sscc.origin,
                            y.dest = years[2L])

  bbdd <- testeo$bbdd
  if (data.type == "counts"){
    transfer_function <- impute_total
  } else {
    transfer_function <- impute_average
  }

  for (aa in 1L:(length(years) - 1L)){
    bbdd <- transfer_function(bbdd = bbdd,
                               y.origin = years[aa],
                               y.dest = years[aa + 1L],
                               all.units = all.units)
  }

  # return(list("df" = bbdd, "missing" = testeo$missing, "inputs" = inputs))
  return(list("df" = bbdd, "missing" = testeo$missing))

}
