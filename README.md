`fETSmcs`
========

An ETS model component selection approach that employs machine learning methodology to predict model component forms using time series features.
The R package `fETSmcs` provides the implementations of ETS model selection for a given time series, see our [paper](https://github.com/Richard/fbcsETS) for more details.

Installation
------------

You can install the package `fbcsETS` from [GitHub Repository](https://github.com/Richard/fbcsETS) with:

``` r
devtools::install_github("Richard759/fETSmcs")
```

Usage
-----

This part introduces how to use the model in our working paper to predict an appropriate ETS model for targeted series.

``` r
# Packages we need.
library(fETSmcs)
library(M4comp2018)
library(forecast)

# Extract features over time series.
data <- M4[1:10]
features <- get_features(data, n.cores=4)

# Use time series features to predict an appropriate ETS model.
model <- model_selection(data, features,n.cores = 4)

# You can also directly put targeted series in function "model_selection" without extracting features separately.
model <- model_selection(data, n.cores = 4)

# The output of "model_selection" function is a list of ETS model.
model[[1]]

# ETS(A,Ad,N) 
#
#   Smoothing parameters:
#     alpha = 0.9999 
#     beta  = 1e-04 
#     phi   = 0.9619 
#
#   Initial states:
#     l = 4836.8895 
#     b = 135.1124 
#
#   sigma:  122.7106
#
#      AIC     AICc      BIC 
# 411.2104 414.7104 419.8143 

# Forecast series with feature-based ETS model component selection.
forecast::forecast(model[[1]],level=c(95),data[[1]]$h)

#      Point Forecast    Lo 95    Hi 95
# 2010       7300.097 7059.589 7540.606
# 2011       7337.579 6997.449 7677.708
# 2012       7373.632 6957.048 7790.217
# 2013       7408.312 6927.264 7889.361
# 2014       7441.671 6903.821 7979.521
# 2015       7473.759 6884.550 8062.968
```

