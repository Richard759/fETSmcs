#' Calculate features from Talagala, Hyndman, Athanaspoulos & Lubba et al.
#' and add them to the dataset
#'.
#' Output is a features \code{frame}.
#' @param dataset A list the elements having a \code{ts} object with the
#' name \code{x}
#' @param n.cores The number of cores to be used. \code{n.cores > 1} means
#' parallel processing.
#'
#' @examples
#' features <- get_features(Mcomp::M3[c(1:3)], n.cores=1)
#' features
#' @importFrom parallel makeCluster
#' @importFrom parallel clusterExport
#' @importFrom parallel clusterCall
#' @importFrom parallel parSapplyLB
#' @importFrom tibble add_column
#' @importFrom Rcatch22 catch22_all
#' @importFrom tsfeatures heterogeneity
#' @importFrom tsfeatures tsfeatures
#' @export


get_features <-
  function(dataset, n.cores=1) {
    list_process_fun <- sapply
    cl = -1
    if (n.cores > 1) {
      cl <- makeCluster(n.cores)
      clusterExport(cl, varlist=ls(), envir=environment())
      clusterExport(cl, varlist=ls(envir=environment(get_features)),
                    envir = environment(get_features))
      list_process_fun <- function(my_list, ...) {
        parSapplyLB(cl, my_list, ...)
      }
    }

    features <- list_process_fun(
      dataset,
      function (serdat) {
        tryCatch({
          #additional features from Talagala, Hyndman, Athanasopoulos 2018
          featrow <-tsfeatures(
            serdat$x,
            features = c(
              "acf_features",
              "arch_stat",
              "crossing_points",
              "entropy",
              "flat_spots",
              "heterogeneity_tsfeat_workaround",
              "hurst",
              "lumpiness",
              "nonlinearity",
              "pacf_features",
              "stl_features",
              "stability",
              "unitroot_kpss",
              "unitroot_pp"
            )
          )

          #additional features
          series_length <- length(serdat$x)

          featrow <- add_column(
            featrow,
            "series_length" = series_length)

          featrow[is.na(featrow)] <- 0  #SET NAs TO 0


          #adding dummy variables for non seasonal series
          #that are not output by tsfeatures
          if (length(featrow) == 32) {
            featrow <- add_column(featrow, seas_acf1 = 0,
                                  .before = 7)
            featrow <- add_column(featrow, seas_pacf = 0,
                                  .before = 22)
            featrow = add_column(featrow, seasonal_strength = 0,
                                 peak = 0, trough = 0, .before = 28)
          }
          featrow<-data.frame(featrow)
          catch22_out <- catch22_all(serdat$x)
          for(j in 1:22){
            featrow[1,catch22_out[j,1]]<-catch22_out[j,2]
          }
          featrow
        }, error = function(e) {
          print(e)
          return(e)
        })
      })

    if (n.cores > 1) {
      parallel::stopCluster(cl)
    }

    data.frame(t(features))
  }

#' @export
heterogeneity_tsfeat_workaround <- function(x) {
  output <- c(arch_acf =0, garch_acf=0, arch_r2=0, garch_r2=0)
  try( output <- heterogeneity(x) )
  output
}
