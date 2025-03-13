# Testea si los años introducidos son correctos

test_years <- function(y.origin, y.dest){
   if ((y.origin - y.dest) == 0L)
     stop("ERROR: The origin and destination reference years must be different.")
   if (!all(c(y.origin, y.dest) %in% c(2001L, 2003L:2023L)))
     stop("ERROR: The origin and destination reference years allowed are 2001 and 2003 to 2023.")
}

# --------------------------------------
# Deshace la reducción de tamanyo de las correspondencias entre sscc
reverse_reduce_size <- function(lista){
  temp <- mapply(function(a, b) data.frame(sscc = a, w = 1),
                 lista[[1L]][, 2L], 1, SIMPLIFY = FALSE)
  names(temp) <- lista[[1L]][, 1L]
  lista <- append(temp, lista[[2L]])
#  lista <- temp[order(names(temp))]
  return(lista)
}

# --------------------------------------
# Testea si los codigos de sscc son correctos y si existen

test_sscc_codes <- function(bbdd, y.origin, y.dest){
  # SSCC codes are of length 10
  names(bbdd)[1L] <- "SSCC"
  codes.sc <- bbdd$SSCC
  l.codes <- sapply(codes.sc, nchar)
  sum.no.10 <- sum(l.codes != 10L)
  sum.9 <- sum(l.codes == 9L)
  codes.sc[l.codes == 9L] <- paste0("0", codes.sc[l.codes == 9L])
  nombre.lista <- paste0("sscc_", substr(y.origin, 3L, 4L), "_to_", substr(y.dest, 3L, 4L))
  lista.origen <- get(nombre.lista)
  existen <- codes.sc %in% c(lista.origen[[1L]][, 1L], names(lista.origen[[2L]]))
  sum.no.existen <- sum(!existen)

  if (sum.no.10 != 0L){
    message(paste0('\n *********************INFO MESSAGE************************\n',
                   " A Spanish census section code has 10 digits.\n",
                   " The code of ", sum.no.10, " rows in the dataset has a length different from 10.\n",
                   " From them ", sum.9, " codes has a length of 9, to which a leading zero has been added.\n"))
  bbdd$SSCC <- codes.sc
  }

  missing <- NULL
  if (sum.no.existen != 0L){
    message(paste0('\n *********************WARNING MESSAGE*********************\n',
                   " A total of ", sum.no.existen, " census section codes included in the dataset are not",
                   " among the codes of the Spanish polygons of the ", y.origin, " census section partition.",
                   " They are not considered in the transfer computations.\n",
                   " The detail of the non-existing codes is available in the component 'missing' of the output of the function.\n"))
    missing <- codes.sc[!existen]
    bbdd <- bbdd[existen, ]
  }

  unicos <- unique(bbdd$SSCC)
  if (length(unicos) < nrow(bbdd))
    stop("ERROR: The dataset contains at least two rows with the same census section code.")

  bbdd <- bbdd[order(bbdd$SSCC), ]

  return(list("bbdd" = bbdd, "missing" = missing))
}


# --------------------------------------
# Testea si los codigos de ccpp son correctos y si existen

test_ccpp_codes <- function(bbdd){
  # Postal codes are of length 5
  names(bbdd)[1L] <- "CCPP"
  codes.cp <- bbdd$CCPP
  l.codes <- sapply(codes.cp, nchar)
  sum.no.5 <- sum(l.codes != 5L)
  sum.4 <- sum(l.codes == 4L)
  codes.cp[l.codes == 4L] <- paste0("0", codes.cp[l.codes == 4L])
  ccpp_to_sscc <- get("ccpp_to_sscc")
  existen <- codes.cp %in% names(ccpp_to_sscc)
  sum.no.existen <- sum(!existen)

  if (sum.no.5 != 0L){
    message(paste0('\n *********************INFO MESSAGE************************\n',
                   " A Spanish postal code has 5 digits.\n",
                   " The code of ", sum.no.5, " rows in the dataset has a length different from 5.\n",
                   " From them ", sum.4, " codes has a length of 4, to which a leading zero has been added.\n"))
    bbdd$CCPP <- codes.cp
  }

  missing <- NULL
  if (sum.no.existen != 0L){
    message(paste0('\n *********************WARNING MESSAGE*********************\n',
                   " A total of ", sum.no.existen, " postal codes included in the dataset are not available",
                   " among the polygons of the (2019) postal code area partition.",
                   " They are not considered in the transfer computations.\n",
                   " The detail of the non-existing codes is available in the component 'missing' of the output of the function.\n"))
    missing <- codes.cp[!existen]
    bbdd <- bbdd[existen, ]
  }

  unicos <- unique(bbdd$CCPP)
  if (length(unicos) < nrow(bbdd))
    stop("ERROR: The dataset contains at least two rows with the same postal code.")

  bbdd <- bbdd[order(bbdd$CCPP), ]

  return(list("bbdd" = bbdd, "missing" = missing))
}


# --------------------------------------
# This function transfer statistics of count variables available in a bbdd from year
# y.origin to year y.dest

impute_total <- function(bbdd, y.origin, y.dest, all.units, na.rm){
  nombre.lista <- paste0("sscc_", substr(y.origin, 3L, 4L), "_to_", substr(y.dest, 3L, 4L))
  lista.origen <- get(nombre.lista)
  nombre.lista <- paste0("sscc_", substr(y.dest, 3L, 4L), "_to_", substr(y.origin, 3L, 4L))
  lista.destino <- get(nombre.lista)
  variables <- names(bbdd)[-1L]
  lista.origen1 <- lista.origen[[1L]][lista.origen[[1L]][, 1L] %in% bbdd[, 1L], ]
  lista.origen2 <- lista.origen[[2L]][names(lista.origen[[2L]]) %in% bbdd[, 1L]]
#  salida <- data.frame(names(lista.destino),
#                       matrix(0L, length(lista.destino), length(variables)))
#  names(salida) <- c("SSCC.Codes", variables)
  if (all.units){
    salida <- data.frame(SSCC.Codes = c(lista.destino[[1L]][, 1L], names(lista.destino[[2L]])))
  } else {
    salida <- data.frame(SSCC.Codes = sort(unique(c(lista.origen1[, 2L],
      as.vector(unlist(sapply(lista.origen2, function(x) x$sscc)))))))
  }

  # correspondencias unitarias
  n1 <- bbdd[, 1L] %in% lista.origen1[, 1L]
  if (sum(n1) > 0L){
    sscc.destino <- lista.origen1
    names( sscc.destino) <- c("SSCC", "SSCC.Codes")
    temp <- merge(bbdd, sscc.destino, all.x = TRUE)
    # temp[is.na(temp$SSCC.Codes), 2L:ncol(bbdd)] <- 0L
    temp <- temp[, -1L]
    salida <- merge(salida, temp, all.x = TRUE)
    # salida[is.na(salida)] <- 0L
  } else {
    salida <- as.data.frame(cbind(salida, matrix(0L, nrow(salida), length(variables))))
    names(salida)[-1L] <- variables
  }
  salida <-  stats::aggregate(salida[, -1], by = list(SSCC.Codes = salida$SSCC.Codes),
                              sum, na.rm = na.rm)
  salida[is.na(salida[ , 2L]) & !(salida$SSCC.Codes %in% unique(sscc.destino$SSCC.Codes)), -1L] <- 0L

  # correspondencias no unitarias
  no.n1 <- !n1
  if (sum(no.n1) > 0L){
    bbdd.no <- bbdd[no.n1, ]
    destinos.visitados <- c()
    for (rr in 1L:nrow(bbdd.no)){
      destino <- lista.origen2[[ which(names(lista.origen2) == bbdd.no[rr, 1L]) ]]
      destinos.visitados <- c(destinos.visitados, destino$sscc)
      reparto <- as.matrix(destino$w) %*% as.matrix(bbdd.no[rr, -1L])
      for (ii in 1L:nrow(reparto)){
        fila <- which(destino$sscc[ii] == salida[, 1L])
        salida[fila, -1L] <- sum(salida[fila, -1L], reparto[ii, ], na.rm = na.rm)
      }
    }
    destinos.visitados <- c(unique(destinos.visitados), unique(sscc.destino$SSCC.Codes))
    salida[!(salida$SSCC.Codes %in% destinos.visitados), -1L] <- NA
  }
  names(salida)[1] <- "SSCC"
  names(salida)[-1L] <- variables
  salida <- salida[order(salida$SSCC), ]
  return(salida)
}


# --------------------------------------
# This function transfer average statistics available in a bbdd from year
# y.origin to year y.dest

impute_average <- function(bbdd, y.origin, y.dest, all.units, na.rm){
  nombre.lista <- paste0("sscc_", substr(y.origin, 3L, 4L), "_to_", substr(y.dest, 3L, 4L))
  lista.origen <- get(nombre.lista)
  nombre.lista <- paste0("sscc_", substr(y.dest, 3L, 4L), "_to_", substr(y.origin, 3L, 4L))
  lista.destino <- get(nombre.lista)
  variables <- names(bbdd)[-1L]
  lista.origen1 <- lista.origen[[1L]][lista.origen[[1L]][, 1L] %in% bbdd[, 1L], ]
  lista.origen2 <- lista.origen[[2L]][names(lista.origen[[2L]]) %in% bbdd[, 1L]]

  if (all.units){
    salida <- data.frame(SSCC.Codes = c(lista.destino[[1L]][, 1L], names(lista.destino[[2L]])))
  } else {
    salida <- data.frame(SSCC.Codes = sort(unique(c(lista.origen1[, 2L],
                                                    as.vector(unlist(sapply(lista.origen2, function(x) x$sscc)))))))
  }

  # correspondencias unitarias
  destinos <- sort(unique(c(lista.origen1[, 2L],
                            as.vector(unlist(sapply(lista.origen2, function(x) x$sscc))))))
  # n.filas <- sapply(lista.destino[names(lista.destino) %in% destinos], nrow)
  # n1 <- destinos[n.filas == 1L]
  n.filas <- lista.destino[[1L]][, 1L] %in% destinos
  n1 <- lista.destino[[1L]][n.filas, 1L]
  temp <- data.frame("SSCC.Codes" = rep(NA, nrow(salida)))
  if(length(n1) > 0L){
    #sscc.destino <- data.frame("SSCC" = unlist(sapply(lista.destino[names(lista.destino) %in% n1],
    #                                                  function(x) x$sscc)), "SSCC.Codes" = n1)
    sscc.destino <- data.frame("SSCC" = lista.destino[[1L]][lista.destino[[1L]][, 1L] %in% n1, 2L],
                               "SSCC.Codes" = n1)
    temp <- merge(bbdd, sscc.destino, all.y = TRUE)
    salida <- merge(salida, temp[, -1L], all.x = TRUE)
    salida[is.na(salida[, 2L]) & !(salida$SSCC.Codes %in% n1), -1L] <- 0L
  } else {
    salida <- as.data.frame(cbind(salida, matrix(0L, nrow(salida), length(variables))))
    names(salida)[-1L] <- variables
  }

  # correspondencias no unitarias
  # no.n1 <- destinos[n.filas != 1L]
  no.n1 <- destinos[!(destinos %in% n1)]
  lista.destino2 <- lista.destino[[2L]]
  if (length(no.n1) > 0L){
    for (rr in 1L:length(no.n1)){
      donantes <- lista.destino2[[ which(names(lista.destino2) == no.n1[rr]) ]]
      pesos <- donantes$w
      pesos.acum <- 0
      for (ii in 1L:length(pesos)){
        fila <- which(bbdd$SSCC == donantes$sscc[ii])
        if (length(fila) > 0L){
          if( !na.rm ){
            pesos.acum <- pesos.acum + pesos[ii]
            salida[salida$SSCC.Codes == no.n1[rr], -1L] <-
              salida[salida$SSCC.Codes == no.n1[rr], -1L] + bbdd[fila, -1L] * pesos[ii]
          } else {
            if(!is.na(bbdd[fila, -1L])){
              pesos.acum <- pesos.acum + pesos[ii]
              salida[salida$SSCC.Codes == no.n1[rr], -1L] <-
                salida[salida$SSCC.Codes == no.n1[rr], -1L] + bbdd[fila, -1L] * pesos[ii]
            }
            if (pesos.acum  == 0) {
              salida[salida$SSCCPP.Codes == no.n1[rr], -1L] <- NA
            }
          }
        }
      }
      salida[salida$SSCC.Codes == no.n1[rr], -1L] <-
        salida[salida$SSCC.Codes == no.n1[rr], -1L]/pesos.acum
    }
    salida[!(salida$SSCC.Codes %in% c(n1, no.n1)), -1L] <- NA
  }
  names(salida)[1] <- "SSCC"
  names(salida)[-1L] <- variables
  salida <- salida[order(salida$SSCC), ]
  return(salida)

}


# --------------------------------------
# This function transfer statistics of count variables from 2019 sscc to 2019 ccpp

sscc2ccpp_total <- function(bbdd, all.units, na.rm){
  lista.origen <- get("sscc_to_ccpp")
  lista.destino <- get("ccpp_to_sscc")
  variables <- names(bbdd)[-1L]
  lista.origen <- lista.origen[names(lista.origen) %in% bbdd[, 1L]]
  #  salida <- data.frame(names(lista.destino),
  #                       matrix(0L, length(lista.destino), length(variables)))
  #  names(salida) <- c("SSCC.Codes", variables)
  if (all.units){
    salida <- data.frame(CCPP.Codes = names(lista.destino))
  } else {
    salida <- data.frame(CCPP.Codes = sort(unique(as.vector(unlist(sapply(lista.origen, function(x) x$ccpp))))))
  }

  # correspondencias unitarias
  n.filas <- sapply(lista.origen, nrow)
  n1 <- n.filas == 1L
  if (sum(n1) > 0L){
    ccpp.destino <- data.frame("SSCC" = bbdd[n1, 1L],
                               "CCPP.Codes" = unlist(sapply(lista.origen[n1], function(x) x$ccpp)))
    temp <- merge(bbdd, ccpp.destino, all.x = TRUE)
    # temp[is.na(temp$CCPP.Codes), 2L:ncol(bbdd)] <- 0L
    temp <- temp[, -1L]
    salida <- merge(salida, temp, all.x = TRUE)
    salida <-  stats::aggregate(salida[, -1], by = list(CCPP.Codes = salida$CCPP.Codes),
                                sum, na.rm = na.rm)
    salida[is.na(salida[ , 2L]) & !(salida$CCPP.Codes %in% unique(ccpp.destino$CCPP.Codes)), -1L] <- 0L
    # salida[is.na(salida)] <- 0L
  } else {
    salida <- as.data.frame(cbind(salida, matrix(0, nrow(salida), length(variables))))
    names(salida)[-1L] <- variables
    ccpp.destino <- NULL
  }
  # salida <-  stats::aggregate(. ~ CCPP.Codes, salida, sum, na.rm = na.rm)

  # correspondencias no unitarias
  no.n1 <- n.filas != 1L
  if (sum(no.n1) > 0L){
    bbdd.no <- bbdd[no.n1, ]
    destinos.visitados <- c()
    for (rr in 1L:nrow(bbdd.no)){
      destino <- lista.origen[[ which(names(lista.origen) == bbdd.no[rr, 1L]) ]]
      destinos.visitados <- c(destinos.visitados, destino$ccpp)
      reparto <- as.matrix(destino$w) %*% as.matrix(bbdd.no[rr, -1L])
      for (ii in 1L:nrow(reparto)){
        fila <- which(destino$ccpp[ii] == salida[, 1L])
        salida[fila, -1L] <- sum(c(salida[fila, -1L], reparto[ii, ]), na.rm = na.rm)
      }
    }
    destinos.visitados <- c(unique(destinos.visitados), unique(ccpp.destino$CCPP.Codes))
    salida[!(salida$CCPP.Codes %in% destinos.visitados), -1L] <- NA
  }
  names(salida)[1] <- "CCPP"
  names(salida)[-1L] <- variables
  salida <- salida[order(salida$CCPP), ]
  return(salida)
}


# --------------------------------------
# This function transfer average statistics from 2019 sscc to 2019 ccpp

sscc2ccpp_average <- function(bbdd, all.units, na.rm){
  lista.origen <- get("sscc_to_ccpp")
  lista.destino <- get("ccpp_to_sscc")
  variables <- names(bbdd)[-1L]
  lista.origen <- lista.origen[names(lista.origen) %in% bbdd[, 1L]]
  if (all.units){
    salida <- data.frame(CCPP.Codes = names(lista.destino))
  } else {
    salida <- data.frame(CCPP.Codes = sort(unique(as.vector(unlist(sapply(lista.origen, function(x) x$ccpp))))))
  }

  # correspondencias unitarias
  destinos <- sort(unique(as.vector(unlist(sapply(lista.origen, function(x) x$ccpp)))))
  n.filas <- sapply(lista.destino[names(lista.destino) %in% destinos], nrow)
  n1 <- destinos[n.filas == 1L]
  temp <- data.frame("CCPP.Codes" = rep(NA, nrow(salida)))
  if(length(n1) > 0L){
    sscc.destino <- data.frame("SSCC" = unlist(sapply(lista.destino[names(lista.destino) %in% n1],
                                                      function(x) x$sscc)), "CCPP.Codes" = n1)
    temp <- merge(bbdd, sscc.destino, all.y = TRUE)
    salida <- merge(salida, temp[, -1L], all.x = TRUE)
    salida[is.na(salida[, 2L]) & !(names(lista.destino) %in% n1), -1L] <- 0L
  } else {
    salida <- as.data.frame(cbind(salida, matrix(0L, nrow(salida), length(variables))))
    names(salida)[-1L] <- variables
  }

  # correspondencias no unitarias
  no.n1 <- destinos[n.filas != 1L]
  if (length(no.n1) > 0L){
    for (rr in 1L:length(no.n1)){
      donantes <- lista.destino[[ which(names(lista.destino) == no.n1[rr]) ]]
      pesos <- donantes$w
      pesos.acum <- 0
      for (ii in 1L:length(pesos)){
        fila <- which(bbdd$SSCC == donantes$sscc[ii])
        if (length(fila) > 0L){
          if( !na.rm ){
            pesos.acum <- pesos.acum + pesos[ii]
            salida[salida$CCPP.Codes == no.n1[rr], -1L] <-
            salida[salida$CCPP.Codes == no.n1[rr], -1L] + bbdd[fila, -1L] * pesos[ii]
          } else {
            if(!is.na(bbdd[fila, -1L])){
              pesos.acum <- pesos.acum + pesos[ii]
              salida[salida$CCPP.Codes == no.n1[rr], -1L] <-
              salida[salida$CCPP.Codes == no.n1[rr], -1L] + bbdd[fila, -1L] * pesos[ii]
            }
            if (pesos.acum  == 0) {
              salida[salida$CCPP.Codes == no.n1[rr], -1L] <- NA
            }
          }
        }
      }
      salida[salida$CCPP.Codes == no.n1[rr], -1L] <-
        salida[salida$CCPP.Codes == no.n1[rr], -1L]/pesos.acum
    }
     salida[!(salida$CCPP.Codes %in% c(n1, no.n1)), -1L] <- NA
  }
  names(salida)[1] <- "CCPP"
  names(salida)[-1L] <- variables
  salida <- salida[order(salida$CCPP), ]
  return(salida)

}


# --------------------------------------
# This function transfer statistics of count variables from 2019 ccpp to 2019 sscc

ccpp2sscc_total <- function(bbdd, all.units, na.rm = na.rm){
  lista.origen <- get("ccpp_to_sscc")
  lista.destino <- get("sscc_to_ccpp")
  variables <- names(bbdd)[-1L]
  lista.origen <- lista.origen[names(lista.origen) %in% bbdd[, 1L]]
  #  salida <- data.frame(names(lista.destino),
  #                       matrix(0L, length(lista.destino), length(variables)))
  #  names(salida) <- c("SSCC.Codes", variables)
  if (all.units){
    salida <- data.frame(SSCC.Codes = names(lista.destino))
  } else {
    salida <- data.frame(SSCC.Codes = sort(unique(as.vector(unlist(sapply(lista.origen, function(x) x$sscc))))))
  }

  # correspondencias unitarias
  n.filas <- sapply(lista.origen, nrow)
  n1 <- n.filas == 1L
  if (sum(n1) > 0L){
    sscc.destino <- data.frame("CCPP" = bbdd[n1, 1L],
                               "SSCC.Codes" = unlist(sapply(lista.origen[n1], function(x) x$sscc)))
    temp <- merge(bbdd, sscc.destino, all.x = TRUE)
    # temp[is.na(temp$SSCC.Codes), 2L:ncol(bbdd)] <- 0L
    temp <- temp[, -1L]
    salida <- merge(salida, temp, all.x = TRUE)
    # salida[is.na(salida)] <- 0L
  } else {
    salida <- as.data.frame(cbind(salida, matrix(0L, nrow(salida), length(variables))))
    names(salida)[-1L] <- variables
  }
  # salida <-  stats::aggregate(. ~ SSCC.Codes, salida, sum, na.rm = na.rm)
  salida <-  stats::aggregate(salida[, -1], by = list(SSCC.Codes = salida$SSCC.Codes),
                              sum, na.rm = na.rm)
  salida[is.na(salida[ , 2L]) & !(salida$SSCC.Codes %in% unique(sscc.destino$SSCC.Codes)), -1L] <- 0L

  # correspondencias no unitarias
  no.n1 <- n.filas != 1L
  if (sum(no.n1) > 0L){
    bbdd.no <- bbdd[no.n1, ]
    destinos.visitados <- c()
    for (rr in 1L:nrow(bbdd.no)){
      destino <- lista.origen[[ which(names(lista.origen) == bbdd.no[rr, 1L]) ]]
      destinos.visitados <- c(destinos.visitados, destino$sscc)
      reparto <- as.matrix(destino$w) %*% as.matrix(bbdd.no[rr, -1L])
      for (ii in 1L:nrow(reparto)){
        fila <- which(destino$sscc[ii] == salida[, 1L])
        salida[fila, -1L] <- sum(c(salida[fila, -1L], reparto[ii, ]), na.rm = na.rm)
      }
    }
    destinos.visitados <- c(unique(destinos.visitados), unique(sscc.destino$SSCC.Codes))
    salida[!(salida$SSCC.Codes %in% destinos.visitados), -1L] <- NA
  }
  names(salida)[1] <- "SSCC"
  names(salida)[-1L] <- variables
  salida <- salida[order(salida$SSCC), ]
  return(salida)
}


# --------------------------------------
# This function transfer average statistics from 2019 ccpp to 2019 sscc

ccpp2sscc_average <- function(bbdd, all.units, na.rm){
  lista.origen <- get("ccpp_to_sscc")
  lista.destino <- get("sscc_to_ccpp")
  variables <- names(bbdd)[-1L]
  lista.origen <- lista.origen[names(lista.origen) %in% bbdd[, 1L]]
  if (all.units){
    salida <- data.frame(SSCC.Codes = names(lista.destino))
  } else {
    salida <- data.frame(SSCC.Codes = sort(unique(as.vector(unlist(sapply(lista.origen, function(x) x$sscc))))))
  }

  # correspondencias unitarias
  destinos <- sort(unique(as.vector(unlist(sapply(lista.origen, function(x) x$sscc)))))
  n.filas <- sapply(lista.destino[names(lista.destino) %in% destinos], nrow)
  n1 <- destinos[n.filas == 1L]
  temp <- data.frame("SSCC.Codes" = rep(NA, nrow(salida)))
  if(length(n1) > 0L){
    ccpp.destino <- data.frame("CCPP" = unlist(sapply(lista.destino[names(lista.destino) %in% n1],
                                                      function(x) x$ccpp)), "SSCC.Codes" = n1)
    temp <- merge(bbdd, ccpp.destino, all.y = TRUE)
    salida <- merge(salida, temp[, -1L], all.x = TRUE)
    salida[is.na(salida) & !(names(lista.destino) %in% n1)] <- 0L
  } else {
    salida <- as.data.frame(cbind(salida, matrix(0L, nrow(salida), length(variables))))
    names(salida)[-1L] <- variables
  }

  # correspondencias no unitarias
  no.n1 <- destinos[n.filas != 1L]
  if (length(no.n1) > 0L){
    for (rr in 1L:length(no.n1)){
      donantes <- lista.destino[[ which(names(lista.destino) == no.n1[rr]) ]]
      pesos <- donantes$w
      pesos.acum <- 0
      for (ii in 1L:length(pesos)){
        fila <- which(bbdd$CCPP == donantes$ccpp[ii])
        if (length(fila) > 0L){
          if( !na.rm ){
            pesos.acum <- pesos.acum + pesos[ii]
            salida[salida$SSCC.Codes == no.n1[rr], -1L] <-
              salida[salida$SSCC.Codes == no.n1[rr], -1L] + bbdd[fila, -1L] * pesos[ii]
          } else {
            if(!is.na(bbdd[fila, -1L])){
              pesos.acum <- pesos.acum + pesos[ii]
              salida[salida$SSCC.Codes == no.n1[rr], -1L] <-
                salida[salida$SSCC.Codes == no.n1[rr], -1L] + bbdd[fila, -1L] * pesos[ii]
            }
            if (pesos.acum  == 0) {
              salida[salida$SSCC.Codes == no.n1[rr], -1L] <- NA
            }
          }
        }
      }
      salida[salida$SSCC.Codes == no.n1[rr], -1L] <-
        salida[salida$SSCC.Codes == no.n1[rr], -1L]/pesos.acum
    }
    salida[!(salida$SSCC.Codes %in% c(n1, no.n1)), -1L] <- NA
  }
  names(salida)[1] <- "SSCC"
  names(salida)[-1L] <- variables
  salida <- salida[order(salida$SSCC), ]
  return(salida)

}
