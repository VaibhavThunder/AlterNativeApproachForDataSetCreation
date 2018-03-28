# Resize images and convert to grayscale

rm(list=ls())
require(EBImage)

# Set wd where images are located
setwd("/Users/vaibhavmishra/Documents/CSC 540/Image classification/Image")
# Set d where to save images
save_in <- "/Users/vaibhavmishra/Documents/CSC 540/Image classification/reSizedDog"
# Load images names
images <- list.files()
# Set width
w <- 256
# Set height
h <- 256

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
    #grayimg <- channel(img_resized,"gray")
    # Path to file
    path <- paste(save_in, imgname, sep = "")
    # Save image
    #writeImage(grayimg, path, quality = 70)
    writeImage(img_resized, path, quality = 70)
    # Print status
    print(paste("Done",i,sep = " "))},
    # Error function
    error = function(e){print(e)})
}

library(raster)
library(sp)
library(rgdal)
# Generate a train-test datasetDEM <- raster("reSizedDogCatLatest.jpg")
DEM <- raster("reSizedDogDog1.jpg")

SplitRas <- function(raster,ppside,save,plot){
  h        <- ceiling(ncol(raster)/ppside)
  v        <- ceiling(nrow(raster)/ppside)
  agg      <- aggregate(raster,fact=c(h,v))
  agg[]    <- 1:ncell(agg)
  agg_poly <- rasterToPolygons(agg)
  names(agg_poly) <- "polis"
  r_list <- list()
  for(i in 1:ncell(agg)){
    e1          <- extent(agg_poly[agg_poly$polis==i,])
    r_list[[i]] <- crop(raster,e1)
  }
  if(save==T){
    for(i in 1:length(r_list)){
      writeRaster(r_list[[i]],filename=paste("SplitRas",i,sep=""),
                  format="GTiff",overwrite=TRUE)
    }
  }
  if(plot==T){
    par(mfrow=c(ppside,ppside))
    for(i in 1:length(r_list)){
      plot(r_list[[i]],axes=F,legend=F,bty="n",box=FALSE)
    }
  }
  return(r_list)
}
SplitRas(raster=DEM,ppside=32,save=TRUE,plot=TRUE)



# Clean environment and load required packages
rm(list=ls())
require(EBImage)

# Set wd where resized greyscale images are located
setwd("/Users/vaibhavmishra/Documents/CSC 540/Image classification/reSizedCat")

# Out file
out_file <- "/Users/vaibhavmishra/Documents/CSC 540/Image classification/CSV file/cats.csv"

# List images in path
images <- list.files()

# Set up df
df <- data.frame()

# Set image size. In this case 64*64
img_size <- 8*8

# Set label
label <- 0

# Main loop. Loop over each image
for(i in 1:length(images))
{
  # Read image
  img <- readImage(images[i])
  # Get the image as a matrix
  img_matrix <- img@.Data
  # Coerce to a vector
  img_vector <- as.vector(t(img_matrix))
  # Add label
  vec <- c(label, img_vector)
  # Bind rows
  df <- rbind(df,vec)
  # Print status info
  print(paste("Done ", i, sep = ""))
}

# Set names
names(df) <- c("label", paste("pixel", c(1:img_size)))

# Write out dataset
write.csv(df, out_file, row.names = FALSE)

#-------------------------------------------------------------------------------
# Test and train split and shuffle

# Load datasets

setwd("/Users/vaibhavmishra/Documents/CSC 540/Image classification/CSV file")
cats <- read.csv("cats.csv")
dogs <- read.csv("dogs.csv")

# Bind rows in a single dataset
new <- rbind(cats, dogs)

# Shuffle new dataset
shuffled <- new[sample(1:10),]

# Train-test split
#train_28 <- shuffled[1:1200,]
#test_28 <- shuffled[1201:1512,]

# Save train-test datasets
write.csv(shuffled, "shuffled_l.csv",row.names = FALSE)
