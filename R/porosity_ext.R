#' Porosity Extraction
#'
#' extracts the porosity of green vegetation regarding light interception.
#' Based on Sonnentag et al. (2012, 10.1016/j.agrformet.2011.09.009) a
#' classification is run separating "plant" and "background". On this account,
#' the threshold procedure introduced by Otsu (1979, https://doi.org/10.1109/TSMC.1979.4310076)
#' was implemented.
#'
#'
#' @return A list consisting of the porosity (0-1), the number of pixels, the number
#' of pixels classified as non-green and the threshold value for this classification
#'
#' @param picture A character vector with the file path of the RGB image to process.
#' @param mode character. Mode used for classification. Default is 'excess_blue', other options are 'blue',
#' 'green' and 'red'. The mode refers to the color channel used for classification.
#' @param resultpath Character. Enter path where downloaded files should be stored.
#' @param filename Character. Enter name that the saved file should get.
#' @param classification Character. Defaults to the Otsu method. Other options are "click_threshold",
#' then the threshold to classify the image has to be set manually by clicking on the density plot, and
#' "set_threshold" where a numeric threshold is typed in as argument and used for classification. The latter
#' is particularly useful when an Otsu classification did not directly deliver perfect results, thus the Otsu
#' threshold can be adjusted. For set_threshold, the threshold value must be supplied in the function body.
#'
#' @export

porosity_ext <- function(picture, mode = "excess_blue", resultpath, filename, classification = "Otsu",
                         threshold = NA){
  cat("Please wait, analyzing image \n")

  # read rgb channels as vector
  red   <- as.vector(picture[,,1])
  green <- as.vector(picture[,,2])
  blue  <- as.vector(picture[,,3])

  # Sum of individual colour values
  bright <- (red + green + blue)

  # Share of each colour
  rn <- red / bright
  gn <- green / bright
  bn <- blue / bright


  # Grabbing density based on chosen mode
  if (mode == "blue"){
    d_channel <- density(bn)
    channel <- bn

  } else if (mode == "green"){
    d_channel <- density(gn)
    channel <- gn

  } else if (mode == "red"){
    d_channel <- density(rn)
    channel <- rn

  } else if (mode == "excess_blue"){
    # calculation excess blue value
    e_blue <- (2*bn - (rn+gn))

    d_channel <- density(e_blue[!is.na(e_blue)])
    channel <- e_blue

  } else stop("mode has to be 'blue', 'green', 'red' or 'excess_blue")

  if(classification == "Otsu"){
    threshold <- otsu_threshold(channel)
    cat("Threshold value: ", threshold)

    # Produces vector with T and F indicating value above or below threshold
    if (mode == "blue" | mode == "excess_blue"){
      logi <- channel < threshold        # value < threshold = F
      logi[is.na(logi)] <- 0
    } else logi <- channel >= threshold  # value >= threshold = T

    # New array, if logi = F, value becomes 0, logi = T values kept
    pic <- array(data = c(red*logi, green *logi, blue * logi), dim = dim(picture) )
    pic.raster <- as.raster(pic)
    plot(pic.raster)   # plot Raster to interactive console
  }

  if(classification == "set_threshold"){
    threshold <- threshold
    cat("Threshold value: ", threshold)

    # Produces vector with T and F indicating value above or below threshold
    if (mode == "blue" | mode == "excess_blue"){
      logi <- channel < threshold        # value < threshold = F
      logi[is.na(logi)] <- 0
    } else logi <- channel >= threshold  # value >= threshold = T

    # New array, if logi = F, value becomes 0, logi = T values kept
    pic <- array(data = c(red*logi, green *logi, blue * logi), dim = dim(picture) )
    pic.raster <- as.raster(pic)
    plot(pic.raster)   # plot Raster to interactive console
  }

  if(classification == "click_threshold"){
    input <- "repeat"
    while(input == "repeat"){  # While loop to allow for correction of classification
      cat("Graphics: Device is created or updated \n")
      cat("\n", "Please choose threshold in plot \n")
      cat("\n", "1. Left click on threshold position \n")
      cat("\n", "2. If satisfied, press esc or click finish in plot panel \n")

      # plot density & identify threshold position
      plot(d_channel)
      d_rank <- identify(d_channel)
      cat(paste("\n", "The",d_rank,"th value in the density distribution is chosen as threshold for classification \n"))
      dev.off() # closes the plot

      # identify threshold value
      threshold <- d_channel$x[d_rank]
      cat("Threshold value: ", threshold)

      # Produces vector with T and F indicating value above or below threshold
      if (mode == "blue" | mode == "excess_blue"){
        logi <- channel < threshold        # value < threshold = F
        logi[is.na(logi)] <- 0
      } else logi <- channel >= threshold  # value >= threshold = T

      # New array, if logi = F, value becomes 0, logi = T values kept
      pic <- array(data = c(red*logi, green *logi, blue * logi), dim = dim(picture) )
      pic.raster <- as.raster(pic)
      plot(pic.raster)   # plot Raster to interactive console

      cat("\n", "Control Quality of Classification \n")
      cat("\n", "Enter 'repeat' to repeat analysis or 'accept' if result is fine \n")

      # Create output of insertion ("Enter" or "ja")
      input <- scan(n=1, what = character())
      cat(input, " was chosen \n")
    }
  }

    # Add trailing slash to resultpath if none exists yet
    resultpath <- ifelse(substr(resultpath, nchar(resultpath),
                                nchar(resultpath)) == "/", resultpath,
                         paste0(resultpath, "/"))

    # Save classified picture
    writeJPEG(target = paste0(resultpath,filename,"_",mode,".jpg"), pic)

  # Calculating Porosity by subtracting amount of cells with T from all cells
  poro <- (length(logi) - sum(logi)) / length(logi)
  cat("\n Porosity: ", poro, "\n")

  # Extract measurement values for output list
  count_true <- sum(logi)
  count_pixel <- length(logi)

  # Create output list
  list(poro = poro, count_pixel = count_pixel, count_true = count_true, threshold = threshold)
}


otsu_threshold <- function(channel){
  histogram <- hist(channel, breaks = 500)

  # extracting counts and edges of each histogram bin
  counts <- histogram$counts # frequency of each element / n_i
  edges <- histogram$breaks # width of each bar


  # calculating bin centers
  bin_centers <- edges[-length(edges)] + diff(edges) / 2

  # Function to summarize squared deviations
  ssd <- function(counts, centers) {
    n <- sum(counts)
    mu = sum(centers * counts) / n # class mean levels

    return(sum(counts * ((centers - mu) ^ 2)))
  }

  # Calculate left and right squared deviations and summarize them
  total_ssds <- c()
  for (bin_no in 1:(length(counts) - 1)) {
    left_ssd <- ssd(counts[1:bin_no], bin_centers[1:bin_no])
    right_ssd <- ssd(counts[(bin_no + 1):length(counts)], bin_centers[(bin_no + 1):length(bin_centers)])
    total_ssds <- c(total_ssds, left_ssd + right_ssd)
  }

  # Extract otsu bin with the lowest sum of squared deviations
  z <- which.min(total_ssds)

  # Extract otsu treshhold, i.e. threshold with min squared deviation
  t <- bin_centers[z]

  return(t)
}
