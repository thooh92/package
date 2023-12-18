# EvAppleWine
This package is used to estimate and predict evapotranspiration in vineyards and apple trees. The model is based on a radiation partitioning approach by 
Hofmann et al. (2014). EvAppleWine comes with a library of orchard or vineyard geometries.
At the moment this library only includes rows, as they are typical for vineyards and most widespread in commercial apple orchards.

## Initializing EvAppleWine
EvAppleWine is not (yet) available on CRAN. It can, however, be downloaded from github using the package devtools. Thus, for EvAppleWine initialization run this code
```
#install.packages("devtools")
library(devtools)
install_github("thooh92/EvAppleWine")
library(EvAppleWine)
```

## Download Orchard/Vineyard Geometry
The data with the outputs of the radiation partitioning model are found hosted on ufz.minio.de. They can be accessed using the 
function `download_fun()`. Therefore, the distance between rows, the height of the rows and the width of the orchard must be specified. Therefore, the 
geometry must be measured in fully developed vegetation. The radiation partitioning of the closest available geometry is always downloaded.
Additionally, a filepath can be specified where the download will be stored. For more information on this function, type `?download_fun`

For example, to download the closest geometry to an apple orchard with a row distance of 1.2 m, a row height of 1.7 m and a row width of 0.41 m the
following code chunk would be used
```
download_fun(r_distance = 1.2, r_height = 1.7, r_width = 0.41)
```

In a warning message it will be specified that, in this example, the closest geometry r_distance = 1.25, r_height = 1.5 and r_width = 0.45 was
downloaded. 


## Forecast
EvAppleWine allows to implement a 5-day weather forecast to simulate water demand for this period. This is based on ICON-EU data run by the German 
Weather Service (DWD). Therefore, these forecast data are limited to 23.5°W to 45.0°E, and 29.5°N to 70.5°N, i.e. Europe. These data were downloaded and preprocessed and are again hosted on ufz.minio.de. With `forecast()`, forecast data are extracted for a location (can be a SpatVector or geographic 
coordinates in decimal degrees), and a data.frame and two plots are returned. The forecast data are updated daily and are available ca. at noon CET. `forecast()` defaults to downloading the respective data from ufz.minio.de, which downloads ca. 1.4 GB. If the function must be rerun for any reason but the data was downloaded before, `download = F` can be defined. 

The location data, if provided in coordinates, must be a data.frame with the columns "lon" and "lat" and it should provide a third column "Name", defining the 
name of the location. Similarly, if provided as SpatVector a column "Name" should be provided. The function will not fail without a "Name" column, but
the plots returned will be less informative. `forecast()` will finally return a list where the first position is the data.frame with the forecast data,
the 2nd position a plot illustrating all data for a visual check and the 3rd position a dashboard-style plot illustrating temperature and precipitation 
development. 

The following example downloads and extracts data for a point SpatVector with several locations, and in the end the dashboard plot is accessed:

```
locs    <- vect("Standorte.shp")  # data for three locations
fc      <- forecast(locations = locs, download = T)
fc[[3]]
```
