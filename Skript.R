library(keras)
library(tensorflow)
library(tfdatasets)
library(purrr)
library(ggplot2)
library(rsample)
library(stars)
library(raster)
library(reticulate)
library(mapview)

setwd("./")

dl_prepare_data <- function(files=NULL, train, predict=FALSE, subsets_path=NULL, model_input_shape = image_size, batch_size = 1L) {
  
  if (!predict){
    
    #function for random change of saturation,brightness and hue, 
    #will be used as part of the augmentation
    spectral_augmentation <- function(img) {
      img <- tf$image$random_brightness(img, max_delta = 0.3)
      img <- tf$image$random_contrast(img, lower = 0.8, upper = 1.1)
      img <- tf$image$random_saturation(img, lower = 0.8, upper = 1.1)
      # make sure we still are between 0 and 1
      img <- tf$clip_by_value(img, 0, 1)
    }
    
    
    #create a tf_dataset from the input data.frame 
    #right now still containing only paths to images 
    dataset <- tensor_slices_dataset(files)
    
    #use dataset_map to apply function on each record of the dataset 
    #(each record being a list with two items: img and mask), the 
    #function is list_modify, which modifies the list items
    #'img' and 'mask' by using the results of applying decode_jpg on the img and the mask   
    #-> i.e. jpgs are loaded and placed where the paths to the files were (for each record in dataset)
    dataset <- 
      dataset_map(dataset, function(.x) 
        list_modify(.x,img = tf$image$decode_jpeg(tf$io$read_file(.x$img)),
                    mask = tf$image$decode_jpeg(tf$io$read_file(.x$mask)))) 
    
    #convert to float32:
    #for each record in dataset, both its list items are modyfied 
    #by the result of applying convert_image_dtype to them
    dataset <- 
      dataset_map(dataset, function(.x) 
        list_modify(.x, img = tf$image$convert_image_dtype(.x$img, dtype = tf$float32),
                    mask = tf$image$convert_image_dtype(.x$mask, dtype = tf$float32))) 
    
    #resize:
    #for each record in dataset, both its list items are modified 
    #by the results of applying resize to them 
    dataset <- 
      dataset_map(dataset, function(.x) 
        list_modify(.x, img = tf$image$resize(.x$img, size = shape(model_input_shape[1], model_input_shape[2])),
                    mask = tf$image$resize(.x$mask, size = shape(model_input_shape[1], model_input_shape[2]))))
    
    
    # data augmentation performed on training set only
    if (train) {
      
      #augmentation 1: flip left right, including random change of 
      #saturation, brightness and contrast
      
      #for each record in dataset, only the img item is modified by the result 
      #of applying spectral_augmentation to it
      augmentation <- 
        dataset_map(dataset, function(.x) 
          list_modify(.x, img = spectral_augmentation(.x$img)))
      
      #...as opposed to this, flipping is applied to img and mask of each record
      augmentation <- 
        dataset_map(augmentation, function(.x) 
          list_modify(.x, img = tf$image$flip_left_right(.x$img),
                      mask = tf$image$flip_left_right(.x$mask)))
      
      dataset_augmented <- dataset_concatenate(dataset,augmentation)
      
      #augmentation 2: flip up down, 
      #including random change of saturation, brightness and contrast
      augmentation <- 
        dataset_map(dataset, function(.x) 
          list_modify(.x, img = spectral_augmentation(.x$img)))
      
      augmentation <- 
        dataset_map(augmentation, function(.x) 
          list_modify(.x, img = tf$image$flip_up_down(.x$img),
                      mask = tf$image$flip_up_down(.x$mask)))
      
      dataset_augmented <- dataset_concatenate(dataset_augmented,augmentation)
      
      #augmentation 3: flip left right AND up down, 
      #including random change of saturation, brightness and contrast
      
      augmentation <- 
        dataset_map(dataset, function(.x) 
          list_modify(.x, img = spectral_augmentation(.x$img)))
      
      augmentation <- 
        dataset_map(augmentation, function(.x) 
          list_modify(.x, img = tf$image$flip_left_right(.x$img),
                      mask = tf$image$flip_left_right(.x$mask)))
      
      augmentation <- 
        dataset_map(augmentation, function(.x) 
          list_modify(.x, img = tf$image$flip_up_down(.x$img),
                      mask = tf$image$flip_up_down(.x$mask)))
      
      dataset_augmented <- dataset_concatenate(dataset_augmented,augmentation)
      
    }
    
    # shuffling on training set only
    if (train) {
      dataset <- dataset_shuffle(dataset_augmented, buffer_size = batch_size*128)
    }
    
    # train in batches; batch size might need to be adapted depending on
    # available memory
    dataset <- dataset_batch(dataset, batch_size)
    
    # output needs to be unnamed
    dataset <-  dataset_map(dataset, unname) 
    
  }else{
    #make sure subsets are read in in correct order 
    #so that they can later be reassembled correctly
    #needs files to be named accordingly (only number)
    o <- order(as.numeric(tools::file_path_sans_ext(basename(list.files(subsets_path)))))
    subset_list <- list.files(subsets_path, full.names = T)[o]
    
    dataset <- tensor_slices_dataset(subset_list)
    
    dataset <- 
      dataset_map(dataset, function(.x) 
        tf$image$decode_jpeg(tf$io$read_file(.x))) 
    
    dataset <- 
      dataset_map(dataset, function(.x) 
        tf$image$convert_image_dtype(.x, dtype = tf$float32)) 
    
    dataset <- 
      dataset_map(dataset, function(.x) 
        tf$image$resize(.x, size = shape(model_input_shape[1], model_input_shape[2]))) 
    
    dataset <- dataset_batch(dataset, batch_size)
    dataset <-  dataset_map(dataset, unname)
    
  }
  
}
dl_subsets <- function(inputrst, targetsize, targetdir, targetname="", img_info_only = FALSE, is_mask = FALSE){
  require(jpeg)
  require(raster)
  
  #determine next number of quadrats in x and y direction, by simple rounding
  targetsizeX <- targetsize[1]
  targetsizeY <- targetsize[2]
  inputX <- ncol(inputrst)
  inputY <- nrow(inputrst)
  
  #determine dimensions of raster so that 
  #it can be split by whole number of subsets (by shrinking it)
  while(inputX%%targetsizeX!=0){
    inputX = inputX-1  
  }
  while(inputY%%targetsizeY!=0){
    inputY = inputY-1    
  }
  
  #determine difference
  diffX <- ncol(inputrst)-inputX
  diffY <- nrow(inputrst)-inputY
  
  #determine new dimensions of raster and crop, 
  #cutting evenly on all sides if possible
  newXmin <- floor(diffX/2)
  newXmax <- ncol(inputrst)-ceiling(diffX/2)-1
  newYmin <- floor(diffY/2)
  newYmax <- nrow(inputrst)-ceiling(diffY/2)-1
  rst_cropped <- suppressMessages(crop(inputrst, extent(inputrst,newYmin,newYmax,newXmin,newXmax)))
  #writeRaster(rst_cropped,filename = target_dir_crop, overwrite=TRUE)
  
  #return (list(ssizeX = ssizeX, ssizeY = ssizeY, nsx = nsx, nsy =nsy))
  agg <- suppressMessages(aggregate(rst_cropped[[1]],c(targetsizeX,targetsizeY)))
  agg[]    <- suppressMessages(1:ncell(agg))
  agg_poly <- suppressMessages(rasterToPolygons(agg))
  names(agg_poly) <- "polis"
  
  pb <- txtProgressBar(min = 0, max = ncell(agg), style = 3)
  for(i in 1:ncell(agg)) {
    
    # rasterOptions(tmpdir=tmpdir)
    setTxtProgressBar(pb, i)
    e1  <- extent(agg_poly[agg_poly$polis==i,])
    
    subs <- suppressMessages(crop(rst_cropped,e1))
    #rescale to 0-1, for jpeg export
    if(is_mask==FALSE){
      
      subs <- suppressMessages((subs-cellStats(subs,"min"))/(cellStats(subs,"max")-cellStats(subs,"min")))
    } 
    #write jpg
    
    
    writeJPEG(as.array(subs),target = paste0(targetdir,targetname,i,".jpg"),quality = 1)
    
    #writeRaster(subs,filename=paste0(targetdir,"SplitRas_",i,".tif"),overwrite=TRUE) 
    #return(c(extent(rst_cropped),crs(rst_cropped)))
  }
  close(pb)
  #img_info <- list("tiles_rows"=nrow(rst_cropped)/targetsizeY, "tiles_cols"=ncol(rst_cropped)/targetsizeX,"crs"= crs(rst_cropped),"extent"=extent(rst_cropped))
  #writeRaster(rst_cropped,filename = paste0(targetdir,"input_rst_cropped.tif"))
  rm(subs,agg,agg_poly)
  gc()
  return(rst_cropped)
  
}
rebuild_img <- function(pred_subsets,out_path,target_rst){
  require(raster)
  require(gdalUtils)
  require(stars)
  
  
  subset_pixels_x <- ncol(pred_subsets[1,,,])
  subset_pixels_y <- nrow(pred_subsets[1,,,])
  tiles_rows <- nrow(target_rst)/subset_pixels_y
  tiles_cols <- ncol(target_rst)/subset_pixels_x
  
  # load target image to determine dimensions
  target_stars <- st_as_stars(target_rst,proxy=F)
  #prepare subfolder for output
  result_folder <- paste0(out_path,"out")
  if(dir.exists(result_folder)){
    unlink(result_folder,recursive = T)
  }
  dir.create(path = result_folder)
  
  #for each tile, create a stars from corresponding predictions, 
  #assign dimensions using original/target image, and save as tif: 
  for (crow in 1:tiles_rows){
    for (ccol in 1:tiles_cols){
      i <- (crow-1)*tiles_cols + (ccol-1) +1 
      
      dimx <- c(((ccol-1)*subset_pixels_x+1),(ccol*subset_pixels_x))
      dimy <- c(((crow-1)*subset_pixels_y+1),(crow*subset_pixels_y))
      cstars <- st_as_stars(t(pred_subsets[i,,,1]))
      attr(cstars,"dimensions")[[2]]$delta=-1
      #set dimensions using original raster
      st_dimensions(cstars) <- st_dimensions(target_stars[,dimx[1]:dimx[2],dimy[1]:dimy[2]])[1:2]
      
      write_stars(cstars,dsn = paste0(result_folder,"/_out_",i,".tif")) 
    }
  }
  
  starstiles <- as.vector(list.files(result_folder,full.names = T),mode = "character")
  gdalbuildvrt(starstiles,paste0(result_folder,"/mosaic.vrt"))
  gdalwarp(paste0(result_folder,"/mosaic.vrt"), paste0(result_folder,"/mosaic.tif"))
}

# Size and bands of the masks, imgs and subsets
image_size <- c(416,416)
bands <- 3

#----------------------------------------------------------------------------------
# Train the model

## load pretrained vgg16 and use part of it as contracting path (feature extraction) ##
vgg16_feat_extr <- application_vgg16(weights = "imagenet", include_top = FALSE, input_shape = append(image_size,bands))

# optionally freeze first layers to prevent changing of their weights, either whole convbase or only certain layers
# freeze_weights(vgg16_feat_extr) #or:
# freeze_weights(vgg16_feat_extr, to = "block1_pool") 

# we'll not use the whole model but only up to layer 15
unet_tensor <- vgg16_feat_extr$layers[[15]]$output 


## add the second part of 'U' for segemntation ##

# "bottom curve" of U-net
unet_tensor <- layer_conv_2d(unet_tensor, filters = 1024, kernel_size = 3, padding = "same", activation = "relu")
unet_tensor <- layer_conv_2d(unet_tensor, filters = 1024, kernel_size = 3, padding = "same", activation = "relu")

# upsampling block 1
unet_tensor <- layer_conv_2d_transpose(unet_tensor, filters = 512, kernel_size = 2, strides = 2, padding = "same")
unet_tensor <- layer_concatenate(list(vgg16_feat_extr$layers[[14]]$output, unet_tensor))
unet_tensor <- layer_conv_2d(unet_tensor, filters = 512, kernel_size = 3, padding = "same", activation = "relu")
unet_tensor <- layer_conv_2d(unet_tensor, filters = 512, kernel_size = 3, padding = "same", activation = "relu")

# upsampling block 2
unet_tensor <- layer_conv_2d_transpose(unet_tensor, filters = 256, kernel_size = 2, strides = 2, padding = "same")
unet_tensor <- layer_concatenate(list(vgg16_feat_extr$layers[[10]]$output, unet_tensor))
unet_tensor <- layer_conv_2d(unet_tensor,filters = 256, kernel_size = 3, padding = "same", activation = "relu")
unet_tensor <- layer_conv_2d(unet_tensor,filters = 256, kernel_size = 3, padding = "same", activation = "relu")

# upsampling block 3
unet_tensor <- layer_conv_2d_transpose(unet_tensor, filters = 128, kernel_size = 2, strides = 2, padding = "same")
unet_tensor <- layer_concatenate(list(vgg16_feat_extr$layers[[6]]$output, unet_tensor))
unet_tensor <- layer_conv_2d(unet_tensor, filters = 128, kernel_size = 3, padding = "same", activation = "relu")
unet_tensor <- layer_conv_2d(unet_tensor, filters = 128, kernel_size = 3, padding = "same", activation = "relu")

# upsampling block 4
unet_tensor <- layer_conv_2d_transpose(unet_tensor, filters = 64, kernel_size = 2, strides = 2, padding = "same")
unet_tensor <- layer_concatenate(list(vgg16_feat_extr$layers[[3]]$output, unet_tensor))
unet_tensor <- layer_conv_2d(unet_tensor, filters = 64, kernel_size = 3, padding = "same", activation = "relu")
unet_tensor <- layer_conv_2d(unet_tensor, filters = 64, kernel_size = 3, padding = "same", activation = "relu")

# final output 
unet_tensor <- layer_conv_2d(unet_tensor, filters = 1, kernel_size = 1, activation = "sigmoid")

# create model from tensors
pretrained_unet <- keras_model(inputs = vgg16_feat_extr$input, outputs = unet_tensor)


#get paths 
files <- data.frame(
  img = list.files("./training_data/imgs_mehr/", full.names = TRUE, pattern = "*.jpg"),
  mask = list.files("./training_data/masks_mehr/", full.names = TRUE, pattern = "*.jpg")
)

# split the data into training and validation datasets. 

files <- initial_split(files, prop = 0.8)

# prepare data for training
training_dataset <- dl_prepare_data(training(files),train = TRUE,model_input_shape = image_size,batch_size = 1L)
validation_dataset <- dl_prepare_data(testing(files),train = FALSE,model_input_shape = image_size,batch_size = 1L)

# get all tensors through the python iterator
training_tensors <- training_dataset%>%as_iterator()%>%iterate()

#how many tensors?
length(training_tensors)

compile(
  pretrained_unet,
  optimizer = optimizer_rmsprop(lr = 1e-5),
  loss = "binary_crossentropy",
  metrics = c(metric_binary_accuracy)
)

#Early Stopping
early_stopping <- callback_early_stopping(
  monitor = 'val_loss',
  patience = 3,
  restore_best_weights = TRUE 
)

diagnostics <- fit(pretrained_unet,
                   training_dataset,
                   epochs = 7,
                   validation_data = validation_dataset,
                   callbacks = list(early_stopping))

plot(diagnostics)
diagnostics$metrics

png(filename="model_results/diagnostics.png")
plot(diagnostics)
dev.off()

save_model_hdf5(pretrained_unet,filepath = "model_results/pretrained_unet.h5")
#-----------------------------------------------------------------------------------
# Predict on a raster layer

pretrained_unet <- load_model_hdf5("model_results/pretrained_unet.h5")

predictions <- predict(pretrained_unet,validation_dataset)
head(predictions)
tail(predictions)

sample <- floor(runif(n = 1,min = 1,max = 4))
img_path <- as.character(testing(files)[[sample,1]])
mask_path <- as.character(testing(files)[[sample,2]])
img <- magick::image_read(img_path)
mask <- magick::image_read(mask_path)
pred <- magick::image_read(as.raster(predict(object = pretrained_unet,validation_dataset)[sample,,,]))

out <- magick::image_append(c(
  magick::image_append(mask, stack = TRUE),
  magick::image_append(img, stack = TRUE), 
  magick::image_append(pred, stack = TRUE)
)
)

plot(out)

# subsets

# Load image and make sure it only has 3 layers
testarea <- stack("./prediction/geo_and_surroundings.tif")
nlayers(testarea)
testarea_layer <- stack(testarea[[1]], testarea[[2]], testarea[[3]])
testarea <- testarea_layer
nlayers(testarea)
plotRGB(testarea)

# Create subsets
cropped_img <- dl_subsets(testarea, image_size, "./prediction/subsets/", img_info_only = FALSE, is_mask = FALSE)
# Predict on the subsets
test_dataset <- dl_prepare_data(train = F,predict = T,subsets_path="./prediction/subsets/",model_input_shape = image_size,batch_size = 5L)
system.time(predictions <- predict(pretrained_unet,test_dataset))

# Put the mosaic back together
rebuild_img(predictions,"./prediction/", cropped_img)

mosaic <- stack("./prediction/out/mosaic.tif")

#----------------------------------------------------------------------------------
# Visualization

plot(mosaic,col = gray.colors(256))
# Reclassify of the raster
reclass_matrix <- matrix(c(0, 0.25, 0,
                           0.25, 1, 1),
                         ncol = 3, byrow = TRUE)

mosaic_reclassified <- reclassify(mosaic, reclass_matrix)
plot(mosaic_reclassified,col = gray.colors(256))

library("leaflet")

extent <- extent(mosaic_reclassified)
xmin <- extent@xmin
xmax <- extent@xmax
ymin <- extent@ymin
ymax <- extent@ymax

mosaic_pal <- colorNumeric(c("white", "black"), values(mosaic_reclassified), na.color = "transparent")

map <- leaflet() %>%
  addTiles() %>%
  addRasterImage(mosaic_reclassified, colors = mosaic_pal, opacity = 0.8, group="Prediction") %>%
  addLayersControl(
    baseGroups = c("Base Map"),
    overlayGroups = c("Prediction"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  fitBounds(xmin, ymin, xmax, ymax)

map
