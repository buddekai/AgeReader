#' @title find.midpoint
#' @description Find midpoint of otolith close to intersection of two lines
#' @details We will add more later...To be continued
#' @aliases findmidpoint find.Midpoint
#' @author Kai Budde
#' @export find.midpoint
#' @param input.file A character

input.file <- "./testdata/21801305_007.tif"
source("./R/editImage.r")


find.midpoint <- function(input.file = NULL){
    
    image <- tiff::readTIFF(source = input.file, info = FALSE)
    image.grey <- edit.image(image = image, grey.mode = "normal.grey")
    image.information <- array(data = 0, dim = dim(image))
    
    center <- dim(image.grey)%/%2
    
    # First few blocks ###
    
    # Block size
    start.size <- 180
    
    upper.left.corner <- center - start.size %/% 2
    lower.right.corner <- center + start.size %/% 2
    
    three.times.three.blocks <- getBlocks(upper.left.corner,
                                          lower.right.corner,
                                          number.of.bocks = 3)
    
}

getBlocks <- function(upper.left.corner, lower.right.corner,
                      number.of.bocks = 3){
    
    # vertical distance of lines
    vertical.distance <- ((lower.right.corner[2]-upper.left.corner[2]) %/%
                              number.of.bocks)
    
    # horizontal distance of lines
    horizontal.distance <- ((lower.right.corner[1]-upper.left.corner[1]) %/%
                                number.of.bocks)
    
    if(number.of.bocks > 0){
        # first vertical line
        lines <- upper.left.corner[1] : (upper.left.corner[1] + number.of.bocks*horizontal.distance)
        lines <- matrix(data = lines, nrow = length(lines), ncol = 2)
        lines[,2] <- upper.left.corner[2]
        vertical.lines <- lines
        vertical.lines.copy <- vertical.lines
        
        # other vertical lines
        for(i in 1:number.of.bocks){
            vertical.lines <- rbind(vertical.lines, vertical.lines.copy[,2] + i*vertical.distance)
        }
        
        # first horizontal line
        lines <- upper.left.corner[2] : (upper.left.corner[2] + number.of.bocks*horizontal.distance)
        lines <- matrix(data = lines, nrow = length(lines), ncol = 2)
        lines[,1] <- upper.left.corner[1]
        horizontal.lines <- lines
        
    }
    
}