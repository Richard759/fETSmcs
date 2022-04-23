`fETSmcs`
========

An ETS model component selection approach that employs machine learning methodology to predict model component forms using time series features.
The R package `fETSmcs` provides the implementations of ETS model selection for a given time series, and you can also use our simulated data to build your own classifier, see our [paper](https://github.com/Richard/fbcsETS) for more details.

Installation
------------

You can install the package `fbcsETS` from [GitHub Repository](https://github.com/Richard/fbcsETS) with:

``` r
devtools::install_github("Richard/fbcsETS")
```

Usage
-----

This part introduces how to use the model in our working paper to predict an appropriate ETS model for targeted series.

``` r
# Packages we need.
library(fETSmcs)
library(M4comp2018)

# Extract features over time series.
data <- M4[1:10]
features <- get_features(data, n.cores=4)

# Forecast time series using feature-based ETS model in our working paper.
model <- forecast_series(data, features,n.cores = 4)

# You can also directly put targeted series in function "forecast_series" without extracting features separately.
model <- forecast_series(data, n.cores = 4)

# Output of the forecast function is ETS model whose method is predicted by our feature-based classifer.
part_model[[1]]

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
```

