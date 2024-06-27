#' Porosity Extraction
#'
#' extracts the porosity of green vegetation regarding light interception.
#' Based on Sonnentag et al. (2012) - 10.1016/j.agrformet.2011.09.009 a
#' classification is run separating "plant" and "background".
#'
#' @return A list consisting of the porosity (0-1), the number of pixels, the number
#' of pixels classified as non-green and the threshold value for this classification
#'
#' @param picture RGB picture.
#' @param mode character. Mode used for classification. Default is 'excess_blue', other options are 'blue',
#' 'green' and 'red'. The mode refers to the color channel used for classification.
#' @param resultpath Character. Enter path where downloaded files should be stored.
#'
#' @export

porosity_ext <- function(picture, mode = "excess_blue", resultpath, filename){
  cat("Please wait, analyzing picure \n")

  # read rgb channels as vector
  rot   <- as.vector(picture[,,1])
  gruen <- as.vector(picture[,,2])
  blau  <- as.vector(picture[,,3])

  # Sum of individual colour values
  hell <- (rot + gruen + blau) #+ 0.0005

  # Share of each colour
  rn <- rot / hell
  gn <- gruen / hell
  bn <- blau / hell

  # calculation excess blue value
  e_blau <- (2*bn - (rn+gn))

  # Grabbing density based on chosen mode
  if (mode == "blue"){
    d_kanal <- density(bn)
    kanal <- bn

  } else if (mode == "green"){
    d_kanal <- density(gn)
    kanal <- gn

  } else if (mode == "red"){
    d_kanal <- density(rn)
    kanal <- rn

  } else if (mode == "excess_blue"){
    d_kanal <- density(e_blau[!is.na(e_blau)])
    kanal <- e_blau

  } else stop("mode has to be 'blue', 'green', 'red' or 'excess_blue")

  eingabe <- "repeat"
  while(eingabe == "repeat"){  # While loop to allow for correction of classification
    cat("Graphics: Device is created or updated \n")
    cat("\n", "Please choose threshold in plot \n")
    cat("\n", "1. Left click on threshold position \n")
    cat("\n", "2. If satisfied, press esc, click finish in plot panel or insert threshold value in Console  \n")

    # plot density & identify threshold position
    plot(d_kanal)
    d_rank <- identify(d_kanal)
    cat(paste("\n", "The",d_rank,"th value in the density distribution is chosen as threshold for classification \n"))
    dev.off() # closes the plot

    # identify threshold value
    grenze <<- d_kanal$x[d_rank]
    cat("Threshold value: ", grenze)

    # Produces vector with T and F indicating value above or below threshold
    if (mode == "blue" | mode == "excess_blue"){
      logi <- kanal < grenze        # value < threshold = F
      logi[is.na(logi)] <- 0
    } else logi <- kanal >= grenze  # value >= threshold = T

    # New array, if logi = F, value becomes 0, logi = T values kept
    bild <- array(data = c(rot*logi, gruen *logi, blau * logi), dim = dim(picture) )
    bild.raster <- as.raster(bild)
    plot(bild.raster)   # plot Raster to interactive console

    # Save classified picture
    writeJPEG(target = paste0(resultpath,filename,"_",mode,".jpg"), bild)

    cat("\n", "Control Quality of Classification \n")
    cat("\n", "Enter 'repeat' to repeat analysis or 'accept' if result is fine \n")

    # Create output of insertion ("Enter" or "ja")
    eingabe <- scan(n=1, what = character())
    cat(eingabe, " was chosen \n")
  }

  # Calculating Porosity by subtracting amount of cells with T from all cells
  poro <- (length(logi) - sum(logi)) / length(logi)
  cat("Porosity: ", poro, "\n")

  # Extract measurement values for output list
  anzahl_blau <- sum(logi)
  anzahl_pixel <- length(logi)

  # Create output list
  list(poro = poro, anzahl_pixel = anzahl_pixel, anzahl_blau = anzahl_blau, threshold = grenze)
}

