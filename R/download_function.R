#' download_fun
#'
#' allows to download radiation partitioning for closest vineyard or orchard geometry from database for 16 growth states during a vegetation period
#' These data can then be translated to a given location and given time period using [processing_function]
#'
#' Radiation Partitioning was modelled for row distances between 1 and 3.5 m, row heights between 1 and 3.5 m and row widths between 0.35 and 0.55 m.
#' If other geometries are given to the function, the closest geometry is downloaded. If these geometries are far off the closest modelled geometry a
#' a warning is created.
#'
#' @param r_width Enter width of a vineyard or orchard row in m in fully developed vegetation
#' @param r_height Enter height of a vineyard or orchard row in m in fully developed vegetation
#' @param r_distance Enter distance between vineyard or orchard rows in m in fully developed vegetation
#' @param filepath Enter path where downloaded files should be stored. Defaults to working directory. Trailing / required.
#'
#' @return A list with all different vegetation state files for the associated geometry
#' @export
#'
#' @examples
#'

download_fun <- function(r_width, r_height, r_distance, filepath = paste0(getwd(),"/")){
  library(devtools)
  library(minio.s3)

  # Define available geometries
  widths    <- c(0.35, 0.45, 0.55)
  heights   <- seq(1,3.5,0.5)
  distances <- seq(1,3.5,0.25)

  # Choose closest geometry from available ones
  width_a   <- widths[which.min(widths - r_width)]
  height_a  <- heights[which.min(abs(heights - r_height))]
  distance_a<- distances[which.min(abs(distances - r_distance))]

  # Warnings if geometry is far off available ones
  if(r_width < 0.2 | r_width > 0.7){
    print("WARNING: r_width far off available geometries - results may not be accurate")
  }
  if(r_height < 0.5 | r_height > 4){
    print("WARNING: r_height far off available geometries - results may not be accurate")
  }
  if(r_distance < 0.5 | r_distance > 4){
    print("WARNING: r_distance far off available geometries - results may not be accurate")
  }

  # Get closest available geometry file with all vegetation states
  suffices      <- c("_CF006%_P0.95_tab", "_CF012%_P0.91_tab", "_CF019%_P0.86_tab",
                     "_CF025%_P0.81_tab", "_CF031%_P0.77_tab", "_CF038%_P0.72_tab",
                     "_CF044%_P0.67_tab", "_CF050%_P0.62_tab", "_CF056%_P0.58_tab",
                     "_CF062%_P0.53_tab", "_CF069%_P0.48_tab", "_CF075%_P0.44_tab",
                     "_CF081%_P0.39_tab", "_CF088%_P0.34_tab", "_CF094%_P0.30_tab",
                     "_CF100%_P0.25_tab")
  monte_list    <- paste0("D",distance_a,"_W",width_a,"_H",height_a,suffices)


  # Download from minio
  Sys.setenv("AWS_ACCESS_KEY_ID" = 'newtest', # enter your credentials
             "AWS_SECRET_ACCESS_KEY" = 'mFvTQdedNNHdP3PucRgTMf1HIm1QWfTGcTzOQ0a1', # enter your credentials
             "AWS_DEFAULT_REGION" = 'test',
             "AWS_S3_ENDPOINT" = 'minio.ufz.de:443')

  for(i in 1:16){
      save_object(bucket = 'met-ohnemus-miro',
              object = paste0("radiation_model/results_monte/",monte_list[i],".rdata"),
              file = paste0(filepath,monte_list[i],".rdata"),
              use_https = T)
  }
}
