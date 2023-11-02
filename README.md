# package
This package is used to estimate and predict evapotranspiration in vineyards and apple trees. The model is based on a radiation partitioning approach by 
Hofmann et al. (2014). The package comes with a library of orchard or vineyard geometries which can be downloaded on the local machine using download_fun().
At the moment this library only includes rows, as they are typical for vineyards and most widespread in commercial apple orchards.

## Initializing Package
This package is not (yet) available on CRAN. It can, however, be downloaded from github using the package devtools. Thus, for package initialization run this code
```
library(devtools)
install_github("thooh92/package")
library(package)
```

## Download Orchard/Vineyard Geometry
The data with the outputs of the radiation partitioning model are found hosted on ufz.minio.de. They can be accessed using an Access Key specified in the 
function download_fun(). Therefore, the distance between rows, the height of the rows and the width of the orchard must be specified. Therefore, the 
geometry must be measured in fully developed vegetation. The radiation partitioning of the closest available geometry is always downloaded.

For example, to download the closest geometry to an apple orchard with a row distance of 1.2 m, a row height of 1.7 m and a row width of 0.41 m the
following code chunk would be used
```
download_fun(r_distance = 1.2, r_height = 1.7, r_width = 0.41)
```

In a warning message it will be specified that, in this example, the closest geometry r_distance = 1.25, r_height = 1.5 and r_width = 0.45 was
downloaded. Additionally, a filepath can be specified where the download will be stored. 
