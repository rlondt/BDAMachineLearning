# Neural Network

# Resize images and convert to grayscale

install.packages("BiocManager")
BiocManager::install("EBImage")

rm(list=ls())
require(EBImage)

# Set wd where images are located
setwd("C://dogs_images")
# Set d where to save images
save_in <- "C://dogs_images_resized"


procesFiles <- function(dir, updir){
  # Load images names
  images <- list.files(dir, recursive = TRUE)
  # Set width
  w <- 64
  # Set height
  h <- 64
  
  # Main loop resize images and set them to greyscale
  for(i in 1:length(images))
  {
    # Try-catch is necessary since some images
    # may not work.
    result <- tryCatch({
      # Image name
      imgname <- images[i]
      # Read image
      img <- readImage(imgname)
      # Resize image 28x28
      img_resized <- resize(img, w = w, h = h)
      # Set to grayscale
      grayimg <- channel(img_resized,"gray")
      # Path to file
      path <- paste(updir, imgname, sep = "")
      # Save image
      writeImage(grayimg, path, quality = 70)
      # Print status
      print(paste("Done",i,sep = " "))},
      # Error function
      error = function(e){print(e)})
  }
}


procesDir <- function(dir, updir){
  dirs <- list.dirs()
  for (i in 1:length(dirs)){
    procesDir(dirs[i], updir+"/"+dirs[i])
  }
  procesFiles(dir, updir)
}

procesFiles("~/BDAMachineLearning/opdrachten/fotoset", "~/BDAMachineLearning/opdrachten/fotosetbewerkt")
