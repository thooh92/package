#' forecast
#'
#' implements a 5-day forecast based on data from the German Weather Service (DWD)
#' applicable for European locations. The forecast function allows download of forecast
#' data and creates a table and two plots checking these data as outputs.
#'
#' @name forecast
#' @param locations SpatVector or data.frame. Locations for which forecast information shall be extracted.
#' If data.frame geographic coordinates must be provided in columns named "lon", "lat".
#' Optionally, the location names can be provided in a column "Name".
#' @param filepath Character. Enter path where downloaded files should be stored. Defaults to working directory.
#' @param download Logical. Indicates whether forecast data stored as .tif shall be downloaded. If TRUE, ca.
#' 1.8 GB of data are downloaded. Defaults to TRUE.
#'
#' @return A list with a data table storing the forecast data, a plot allowing
#' checking these data visually, and a dashboard plot visualizing temperature
#' and precipitation.
#' @import tidyverse
#' @import terra
#' @import scales
#' @import curl
#' @import stringr
#' @import R.utils
#'
#' @examples
#' # Using a point SpatVector
#' locs    <- vect("C:/Docs/MIRO/GIS/Standorte/Standorte.shp")
#' fc      <- forecast(locations = locs, download = T)
#'
#' # Using a data.frame
#' x <- data.frame(
#' lon = c(11:15),
#' lat = c(51:55)
#' )
#' fc      <- forecast(locations = x, download = F)
#'

#' @rdname forecast
#' @export
forecast  <- function(locations, filepath = getwd(), download = T){
  pats    <- c("RELHUM_2M.tif", "T_2M.tif", "TMAX_2M.tif", "TMIN_2M.tif", "TOT_PREC.tif",
               "GLOBRAD.tif", "WINDSPEED_10M.tif") # patterns of forecast data

  if(download == T){
    # Minio Access Key
    Sys.setenv("AWS_ACCESS_KEY_ID" = 'h5OlQlKs05NICoKjrWhC',
               "AWS_SECRET_ACCESS_KEY" = '6dwoCb9XKhH0QE4RX5Gj63HnsYXUewv5tHp0jaVh',
               "AWS_DEFAULT_REGION" = 'test',
               "AWS_S3_ENDPOINT" = 'minio.ufz.de:443')



    # If filepath was provided without trailing backslash, a trailing backslash is added
    if(substr(filepath, nchar(filepath), nchar(filepath)) != "/"){
      filepath = paste0(filepath, "/")
    }

    # Minio Download
    obj <- c()

    for(i in 1:length(pats)){
      obj[i] <- minio.s3::save_object(bucket = 'met-ohnemus-miro',
                                      object = paste0("dwd_forecast/",pats[i]),
                                      file = paste0(filepath,pats[i]),
                                      use_https = T)
    }
  } else { # If data was downloaded before, names are assign for later data access
    obj <- pats
  }

  # Assign information of model start
  last_time <- minio.s3::save_object(bucket = 'met-ohnemus-miro',
                           object = "dwd_forecast/time.rds",
                           file = paste0(filepath,"time.rds"),
                           use_https = T)
  last_time <- readRDS(last_time)

  # Preparing value extraction
  hours   <- c(0:78, seq(81,120,3))  # hours for which forecast is available
  h       <- 0:120  # hours to interpolate forecast data to
  empty_v <- c()


  if(is.data.frame(locations)){  # transforms data.frame to SpatVector
    locations <- vect(locations, geom=c("lon", "lat"), crs="EPSG:4326", keepgeom=FALSE)
  }


  for(i in 1:7){  # Extracting data from .tifs, prodcing data.frame
    ras     <- rast(obj[i])
    if(i == 1){ locations    <- terra::project(locations, crs(ras)) }

    ext_df  <- terra::extract(ras, locations, fun = "mean")
    colnames(ext_df)[2:94] <- hours
    ext_df  <- pivot_longer(ext_df, cols = 2:94, names_to = "hours", values_to = "value")

    # Interpolation of 3 h time intervals from 78h+
    for(k in 1:length(unique(ext_df$ID))){
      sub_df   <- ext_df[ext_df$ID == k,]

      if(i == 5){
        for(j in 2:length(sub_df$value)){
          empty_v[j] <- sub_df$value[j] - sub_df$value[j-1]
        }
        sub_df$value[2:length(sub_df$value)] <- empty_v[2:length(sub_df$value)]
      }

      # Interpolation of data to hourly intervals; DWD data from 78 h in the future
      # onwards provided in 3 h intervals
      interpol <- data.frame(approx(as.numeric(sub_df$hours), sub_df$value, xout = h,
                                    method = "linear", rule = 2, ties = mean))
      interpol$ID <- rep(unique(ext_df$ID)[k], length(interpol$x))
      if(k == 1){ interpol_df <- interpol }
      if(k > 1){ interpol_df <- rbind(interpol_df, interpol)}


    }

    if(i == 1){ fill  <- interpol_df }
    if(i > 1){ fill   <- cbind(fill, interpol_df)}

  }

  pats <- sub(".tif", "", pats)  # Naming for data.frame and plots

  colnames(fill)[c(2,5,8,11,14,17,20)] <- pats[1:7]
  fill    <- fill[,c(1:3,5,8,11,14,17,20)]
  fill    <- pivot_longer(fill, cols = c(2,4:9), names_to = "variable", values_to = "value")

  if(!is.null(locations$Name)){  # If provided, names are extracted for plotting
    locations$ID <- as.numeric(row.names(data.frame(locations)))
    fill <- full_join(fill, data.frame(locations))
  } else {
    fill$Name <- fill$ID
  }

  fill$time <- last_time + fill$x*60*60  # Transforming hours to datetime

  # Temperature ribbon to plot around mean temperature
  ribbon  <- fill[fill$variable == "T_2M" | fill$variable ==  "TMAX_2M" |
                    fill$variable == "TMIN_2M",] %>% group_by(Name, time) %>%
    summarize(max = max(value), min = min(value))


  # Assign human-readable names for plots
  fill$variable <- ifelse(fill$variable == "RELHUM_2M", "Relative Humidity [%]",
                          ifelse(fill$variable == "T_2M", "Mean Air Temperature [°C]",
                                 ifelse(fill$variable == "TMAX_2M", "Maximum Air Temperature [°C]",
                                        ifelse(fill$variable == "TMIN_2M", "Minimum Air Temperature [°C]",
                                               ifelse(fill$variable == "TOT_PREC", "Precipitation [mm]",
                                                      ifelse(fill$variable == "GLOBRAD", "Global Radiation [W/m²]", "Wind Speed [m/s]"))))))



  # Plotting all variables as visual check
  vars_p <-
    ggplot(fill, aes(x = time, y = value, color = as.character(Name))) + theme_bw() +
    geom_line() + facet_wrap(~variable, scales = "free", ncol = 1) +
    labs(x = "", y = "", color = "")


  # Introducing Scale for dashboard-style plot
  scale <- round(max(fill$value[fill$variable == "Precipitation [mm]"])/
                   max(fill$value[fill$variable == "Mean Air Temperature [°C]"]),2)


  # Producing dashboard-style forecast plot
  forecast_p <-
    ggplot() + geom_bar(data = fill[fill$variable == "Precipitation [mm]",],
                        aes(x = time, y = value/scale), stat = "identity", fill = "blue") +
    geom_line(data = fill[fill$variable == "Mean Air Temperature [°C]",],
              aes(x = time, y = value, colour = variable), size = 1.1) +
    theme_bw() + facet_wrap(~Name, nrow = 3) +
    labs(y = "Air Temperature [°C]", x = "", color = "") +
    geom_ribbon(data = ribbon, aes(x = time, ymin = min, ymax = max),
                alpha = 0.5, fill = "grey50") +
    scale_y_continuous(
      sec.axis = sec_axis(trans = ~ .x*scale,
                          name = "Precipitation [mm]")) +
    theme(legend.position = "NONE")

  ret_list <- list(fill, vars_p, forecast_p)
  return(ret_list)
}

