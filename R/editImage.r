#' @title edit.image
#' @description Adjust color, brightness or contrast of the image
#' @details By using an x-y-3(rgb)-representation of an image, you can
#' change the color, brightness or contrast of that image. For adjusting
#' brightness or contrast use a number between -1 and 1 as parameter.
#' To get more intense colors use a number between 0 and 1 for each color
#' parameter. By entering grey.mode = TRUE one will get a normal grey image.
#' Other options for grey.mode are "luminosity.grey", red.grey",
#' "green.grey" and "blue.grey".
#' @aliases edit.image image.edit
#' @author Kai Budde
#' @export edit.image
#' @param image An array of numbers between 0 and 1
#' @param file.name A character (location of the image)
#' @param grey.mode A character
#' @param brightness A number
#' @param contrast A character
#' @param red A character
#' @param green A character
#' @param blue A character

edit.image <- function(image = NULL,
                       file.name = NULL,
                       grey.mode = NULL,
                       brightness = NULL,
                       contrast = NULL,
                       red = NULL,
                       green = NULL,
                       blue = NULL){

    # Default values for missing arguments ---------------------------------
    if(is.null(brightness)){
        brightness <- -2
    }
    if(missing(contrast)){
        contrast <- -2
    }
    if(missing(red)){
        red <- -1
    }
    if(missing(green)){
        green <- -1
    }
    if(missing(blue)){
        blue <- -1
    }
    
    if(is.null(image) & is.null(file.name)){
        print(paste("Please enter either an x,y,3-array or a file ",
                    "name (image path).", sep=""))
        return()
    }
    
    if(!is.null(file.name)){
        #options (don't show warnings)
        .old.options <- options()
        on.exit(options(.old.options))
        options(stringsAsFactors = FALSE, warn=-1)
        
        image <- tiff::readTIFF(source = file.name, info = FALSE)
    }
    
    
    
    # Create a grey image --------------------------------------------------
    if(!is.null(grey.mode)){
        
        if(grey.mode == TRUE){
            grey.mode <- "normal.grey"
        }
        
        if(!grey.mode  %in% c("normal.grey", "luminosity.grey", "red.grey",
                              "green.grey", "blue.grey")){
            grey.mode <- "normal.grey"
        }
        
        mat.R <- image[,,1]
        mat.G <- image[,,2]
        mat.B <- image[,,3]
        
        # Normal grey
        if(grey.mode == "normal.grey"){
            image.grey <- (mat.R + mat.G + mat.B)/3
        }
        
        # Luminosity method
        if(grey.mode == "luminosity.grey"){
            image.grey <- 0.21 * mat.R + 0.72 * mat.G + 0.07 * mat.B
        }
        
        # Grey from red layer
        if(grey.mode == "red.grey"){
            image.grey <- mat.R
        }
        
        # Grey from green layer
        if(grey.mode == "green.grey"){
            image.grey <- mat.G
        }
        
        # Grey from blue layer
        if(grey.mode == "blue.grey"){
            image.grey <- mat.B
        }
        
        rm(mat.R, mat.G, mat.B)
        
        return(image.grey)
        
    }else{
        
        # Brightness -------------------------------------------------------
        # We add a number between (+-)0 and 1 to every pixel. If the result
        # is greater (smaller) than 1 (0) then take the maximum 1
        # (minimum 0).
        
        if(brightness >= -1 & brightness <= 1){
            image[,,1] <- image[,,1] + brightness
            image[,,2] <- image[,,2] + brightness
            image[,,3] <- image[,,3] + brightness
            
            image[,,1][image[,,1] > 1] <- 1
            image[,,1][image[,,1] < 0] <- 0
            
            
            image[,,2][image[,,2] > 1] <- 1
            image[,,2][image[,,2] < 0] <- 0
            
            
            image[,,3][image[,,3] > 1] <- 1
            image[,,3][image[,,3] < 0] <- 0
        }
        
        
        
        # Contrast ---------------------------------------------------------
        # The contrast can be changed from -1 (less) to 1 (more contrast).
        # Nothing happens for contrast == 0.
        
        if(contrast >= -1 & contrast <= 1){
            
            # Correction factor for contrast
            contrast.factor <-
                259 * (contrast + 1) / ( 255 *( 259/255 - contrast))
            
            image[,,1] <- contrast.factor * (image[,,1] - 0.5) + 0.5
            image[,,2] <- contrast.factor * (image[,,2] - 0.5) + 0.5
            image[,,3] <- contrast.factor * (image[,,3] - 0.5) + 0.5
            
            image[,,1][image[,,1] > 1] <- 1
            image[,,1][image[,,1] < 0] <- 0
            
            
            image[,,2][image[,,2] > 1] <- 1
            image[,,2][image[,,2] < 0] <- 0
            
            
            image[,,3][image[,,3] > 1] <- 1
            image[,,3][image[,,3] < 0] <- 0
        }
        
        # Change intensity of colors ---------------------------------------
        # For every color layer (red, green, blue) one may choose a value
        # between 0 and 1. This value will be multiplied with the maximum
        # value of the color. (Use green == 0 & blue == 0 to get an image
        # with only the red layer.)
        
        
        if(0 <= red & red <= 1){
            image[,,1] <- red * image[,,1] / max(image[,,1])
        }
        if(0 <= green & green <= 1){
            image[,,2] <- green*image[,,2]/max(image[,,2])
        }
        if(0 <= blue & blue <= 1){
            image[,,3] <- blue*image[,,3]/max(image[,,3])
        }
        
        if(!is.null(file.name)){
            
            # Save image
            save.file.name <- gsub("\\.tif+", "", file.name)
            save.file.name <- paste(save.file.name, "_edited.tif", sep="")
            
            tiff::writeTIFF(what = image,
                            where = save.file.name,
                            bits.per.sample = 8L, compression = "none",
                            reduce = TRUE)
            
            return(print(paste("The edited image has been save as ",
                               save.file.name, ".", sep="")))
        }else{
            return(image)
        }
        
    }
    
}
