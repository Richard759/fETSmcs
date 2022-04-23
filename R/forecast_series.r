#' Forecast coming time series.
#'
#' @param dataset A list the elements having a \code{ts} object with the
#' name \code{x}.
#' @param features Can be NULL if you just use the model package provides.
#' @param n.cores The number of cores to be used. \code{n.cores > 1} means
#' parallel processing.
#'
#' @return A list with the elements having the following structure
#'
#' @importFrom lightgbm lgb.load
#' @importFrom stats frequency
#' @importFrom stats predict
#' @importFrom forecast ets
#' @importFrom forecast forecast
#' @export
forecast_series <- function(dataset, features=NULL, n.cores=1) {
  error_list<-c("A","M")
  trend_list<-c("A","Ad","N")
  seasonality_list<-c("A","M","N")
  non_strict_list<-c('ANM','AAM','AAdM')
  file<-system.file("extdata", "lgb_error.txt", package = "fbcsETS")
  lgb_error<-lgb.load(file)
  file<-system.file("extdata", "lgb_trend.txt", package = "fbcsETS")
  lgb_trend<-lgb.load(file)
  lgb_seasonality<-lgb.load(system.file("extdata",
                                        "lgb_seasonality.txt",
                                        package = "fbcsETS"))
  if(is.null(features)){
    features<-get_features(dataset, n.cores=n.cores)
  }
  pred_error <- predict(lgb_error,data.matrix(features), reshape = TRUE)
  pred_error<-cbind(1-pred_error,pred_error)
  pred_trend <- predict(lgb_trend,data.matrix(features), reshape = TRUE)
  pred_seasonality <- predict(lgb_seasonality,data.matrix(features),
                              reshape = TRUE)
  for (i in 1:length(dataset)){
    seasonal_period <- features[i,'seasonal_period']
    series_length <- features[i,'series_length']
    # Yearly series should be None seasonality component
    if(seasonal_period==1){
      ets_method<-paste0(error_list[nloc(pred_error[i,])],
                         trend_list[nloc(pred_trend[i,])],
                         "N")
    }else{
      ets_method <- paste0(error_list[nloc(pred_error[i,])],
                           trend_list[nloc(pred_trend[i,])],
                           seasonality_list[nloc(pred_seasonality[i,])])
    }
    # Check if the method is applicable
    if(ets_method %in% non_strict_list){
      if(nloc(pred_error[i,])*nloc(pred_seasonality[i,],2) >
         nloc(pred_error[i,],2)*nloc(pred_seasonality[i,])){
        ets_method<-paste0(error_list[nloc(pred_error[i,])],
                           trend_list[nloc(pred_trend[i,])],
                           seasonality_list[nloc(pred_seasonality[i,],2)])
      }

      else{
        ets_method<-paste0(error_list[nloc(pred_error[i,],2)],
                           trend_list[nloc(pred_trend[i,])],
                           seasonality_list[nloc(pred_seasonality[i,])])
      }
    }

    # Non-positive data shouldn't be multiplicative error component
    if(min(dataset[[i]]$x)<=0 & substring(ets_method,1,1) == 'M'){
      ets_method<-paste0('A',
                         trend_list[nloc(pred_trend[i,])],
                         seasonality_list[nloc(pred_seasonality[i,])])
      if(ets_method %in% non_strict_list){
        ets_method<-paste0('A',
                           trend_list[nloc(pred_trend[i,])],
                           seasonality_list[nloc(pred_seasonality[i,],2)])
      }
    }
    # Damped component need enough observation
    if (substring(ets_method,3,3)=="d"){
      npars <- 9L
      if (substring(ets_method,4,4) != "N") {
        npars <- npars + seasonal_period
      }
      if (npars>=series_length){
        ets_method<-paste0(substring(ets_method,1,1),
                           trend_list[nloc(pred_trend[i,],2)],
                           substring(ets_method,4,4))
      }
    }
    dataset[[i]]$method <- ets_method
  }

  list_process_fun <- lapply
  cl = -1
  if (n.cores > 1) {
    cl <- parallel::makeCluster(n.cores)
    parallel::clusterExport(cl, varlist=ls(), envir=environment())
    list_process_fun <- function(my_list, ...) {
      parallel::parLapplyLB(cl, my_list, ...)
    }
  }

  forecast_model <- list_process_fun(dataset,function (series) {
    if (substring(series$method,3,3)=="d"){
      m<-paste0(substring(series$method,1,2),substring(series$method,4,4))
      ts_model<-try(forecast::ets(series$x,model = m, damped =TRUE))
      f_result<-forecast::forecast(ts_model,level=c(95),series$h)
      if(max(f_result$upper - f_result$lower) > max(abs(series$x))*50){
        ts_model<-try(forecast::ets(series$x,model = 'ANN', damped =FALSE))
      }

    }else{
      ts_model<-try(forecast::ets(series$x,model = series$method,damped = FALSE))
      f_result<-forecast::forecast(ts_model,level=c(95),series$h)
      if(max(f_result$upper - f_result$lower) > max(abs(series$x))*50){
        ts_model<-try(forecast::ets(series$x,model = 'ANN', damped =FALSE))
      }

    }
    ts_model
  })

  if (n.cores > 1) {
    parallel::stopCluster(cl)
  }

  forecast_model
}

nloc<-function(x,n=1){
  x<-order(x,decreasing=TRUE)
  return(x[n])
}
