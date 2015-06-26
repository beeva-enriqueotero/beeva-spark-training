##
## Exercise source:
## http://www.r-bloggers.com/r-k-means-clustering-on-an-image/
##


# k-means clustering is a method of vector quantization, originally from signal processing,
# that is popular for cluster analysis in data mining. k-means clustering aims to partition n
# observations into k clusters in which each observation belongs to the cluster with the nearest mean,
# serving as a prototype of the cluster. (Wikipedia, Ref 1.) 


# We will utilize the following packages for input and output:
#  
#  jpeg – Read and write JPEG images; and,
#  ggplot2 – An implementation of the Grammar of Graphics.




#Load the jpeg library
library(jpeg)
url <- "http://www.wall321.com/thumbnails/detail/20120304/colorful%20birds%20tropical%20head%203888x2558%20wallpaper_www.wall321.com_40.jpg"

# Download the file and save it as "Image.jpg" in the directory
dFile <- download.file(url, "Image.jpg")

# "Hey R! this is a jpg image!
img <- readJPEG("Image.jpg") # Read the image

# Obtain the dimension
imgDm <- dim(img)

# Assign RGB channels to data frame
imgRGB <- data.frame(
  x = rep(1:imgDm[2], each = imgDm[1]),
  y = rep(imgDm[1]:1, imgDm[2]),
  R = as.vector(img[,,1]),
  G = as.vector(img[,,2]),
  B = as.vector(img[,,3])
)

#That doesn't say much, what just happend?
View(imgRGB)

#So each pixel is now represented by its Red, Green and Blue values.


library(ggplot2)

# ggplot theme to be used. This is an aux function to plot thing in a 
# "pretty" way. Quotes intended.

plotTheme <- function() {
  theme(
    panel.background = element_rect(
      size = 3,
      colour = "black",
      fill = "white"),
    axis.ticks = element_line(
      size = 2),
    panel.grid.major = element_line(
      colour = "gray80",
      linetype = "dotted"),
    panel.grid.minor = element_line(
      colour = "gray90",
      linetype = "dashed"),
    axis.title.x = element_text(
      size = rel(1.2),
      face = "bold"),
    axis.title.y = element_text(
      size = rel(1.2),
      face = "bold"),
    plot.title = element_text(
      size = 20,
      face = "bold",
      vjust = 1.5)
  )
}

# Plot the image
ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = rgb(imgRGB[c("R", "G", "B")])) +
  labs(title = "Original Image: Colorful Bird") +
  xlab("x") +
  ylab("y") +
  plotTheme()

###############################################################################
## Now the interesting part! Let's use the k-means algorithm to create groups #
## based on their RGB values.                                                 #
###############################################################################

# Let's try 3 clusters! You could change this value and see what happens.
kClusters <- 3
kMeans <- kmeans(imgRGB[, c("R", "G", "B")], centers = kClusters)
kColours <- rgb(kMeans$centers[kMeans$cluster,])


# Ok now finally, lets SEE the groups.
ggplot(data = imgRGB, aes(x = x, y = y)) + 
  geom_point(colour = kColours) +
  labs(title = paste("k-Means Clustering of", kClusters, "Colours")) +
  xlab("x") +
  ylab("y") + 
  plotTheme()


