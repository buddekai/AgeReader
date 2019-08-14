#' @title find.midpoint
#' @description Find midpoint of otolith close to intersection of two lines
#' @details We will add more later...To be continued
#' @aliases findmidpoint find.Midpoint
#' @author Kai Budde
#' @export find.midpoint
#' @param image A matrix of numbers: Contains the information of the otolith
#' image
#' @param image.grey A matrix of numbers: Grey values of the otolith image
#' (values between 0 and 1)
#' @param center.point A number: Coordinates of the starting point for the
#' box to look for the darkest point
#' @param number.of.blocks.in.row A number: Number of blocks in a row or
#' column (the search field will then be number x number big)
#' @param search.length A number: Pixel length of the entire search field.

find.midpoint <- function(image = NULL,
                          image.grey = NULL,
                          center.point = NULL,
                          number.of.blocks.in.row = NULL,
                          search.length = 180,
                          image.path){
    
    #image <- tiff::readTIFF(source = input.file, info = FALSE)
    #image.grey <- edit.image(image = image, grey.mode = "normal.grey")
    
    if(is.null(center.point)){
        center.point <- dim(image.grey)%/%2
    }
    if(is.null(number.of.blocks.in.row)){
        number.of.blocks.in.row <-  3
    }
    
    image.information <- array(data = 0, dim = dim(image))
    
    
    upper.left.corner.0 <- center.point - search.length %/% 2
    upper.left.corner <- c(0,0)
    lower.right.corner <- c(0,0)
    #lower.right.corner <- center + search.length %/% 2
    
    block.length <- search.length %/% number.of.blocks.in.row
    
    for(i in 1:number.of.blocks.in.row){
        for(j in 1:number.of.blocks.in.row){
            
            # Calculate the upper left and lower right corner of block (i,j)
            # (i goes down in parallel to x and j to the right, parralel
            # to y)
            upper.left.corner[1] <- upper.left.corner.0[1] + (j-1)*block.length
            upper.left.corner[2] <- upper.left.corner.0[2] + (i-1)*block.length
            lower.right.corner[1] <- upper.left.corner.0[1] + j*block.length
            lower.right.corner[2] <- upper.left.corner.0[2] + i*block.length
            
            #print(paste("Block ", (i-1)*number.of.blocks.in.row+j))
            #print(paste("upper left: ", paste(upper.left.corner, collapse = " ")))
            #print(paste("lower right: ", paste(lower.right.corner, collapse = " ")))
            
            blockPixels <- getBlockPixels(upper.left.corner,
                                          lower.right.corner)
            
            cornerPixels <- getCornerPixels(upper.left.corner,
                                            lower.right.corner)
            
            # Mark borders of boxes in red
            image.information[cbind(cornerPixels,1)] <- 1
            image.information[cbind(cornerPixels,2)] <- -1
            image.information[cbind(cornerPixels,3)] <- -1
            
            if(j == 1 & i == 1){
                meanPixelValue <- mean(image.grey[blockPixels])
                min.meanPixelValue <- meanPixelValue
                min.i <- i
                min.j <- j
                coordinates.of.midpoint <- (lower.right.corner + upper.left.corner) %/%2
            }else{
                meanPixelValue <- mean(image.grey[blockPixels])
                #print("there")
                if(meanPixelValue < min.meanPixelValue){
                    min.meanPixelValue <- meanPixelValue
                    min.i <- i
                    min.j <- j
                    coordinates.of.midpoint <- (lower.right.corner + upper.left.corner) %/%2
                }
            }
        }
    }
    
    
    # Mark center points blue
    image.information[coordinates.of.midpoint[1], coordinates.of.midpoint[2], 1] <- 1
    image.information[coordinates.of.midpoint[1], coordinates.of.midpoint[2], 2] <- 1
    image.information[coordinates.of.midpoint[1], coordinates.of.midpoint[2], 3] <- 1

    # Write image
    # Add image.information to image
    image.information <- image + image.information
    
    image.information <-
        ifelse(image.information < 0, 0, image.information)
    image.information <-
        ifelse(image.information > 1, 1, image.information)
    
    # Save image
    save.input.file <- gsub("\\.tif+", "", image.path)
    save.input.file <- paste(save.input.file, "_blocks_", search.length
                             ,".tif", sep="")
    
    tiff::writeTIFF(what = image.information,
                    where = save.input.file,
                    bits.per.sample = 8L, compression = "none",
                    reduce = TRUE)
    
    print(paste("Schwarze Mitte: ", paste(coordinates.of.midpoint, collapse = " ")))
    
    return(coordinates.of.midpoint)
    
}


