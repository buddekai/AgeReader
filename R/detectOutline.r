#' @title detect.outline
#' @description Detect the outline of the otolith
#' @details We will add more later...To be continued
#' @aliases detect.outline outline.detect
#' @author Kai Budde
#' @export detect.outline
#' @param image.grey An array (x,y) of values between 0 and 1
#' @param image.information An array (x,y) of values between 0 and 1
#' @param distance A number (Lines for finding the outline of the otolith
#' are this many pixels apart on the x- and y-axes.)
#' @param parameter.for.end A number (The edge is being found by calculating
#' a floating mean. The function marks an edge, if the next point differs
#' more than floating mean divided by parameter.for.end.)
#' @param parameter.distance.deviation A number (Outliers get detected by
#' multiplying the parameter with the mean distance between points.)
#' @param file.name A character
#' @param grey.mode A character

detect.outline <- function(image.grey = NULL,
                           image.information = NULL,
                           distance = 20,
                           parameter.for.end = NULL,
                           parameter.distance.deviation = 2,
                           file.name = NULL,
                           grey.mode = "normal.grey"){
    
    
    # Import image (if function is used by itself) -------------------------
    if(!is.null(file.name)){
        #options (don't show warnings)
        .old.options <- options()
        on.exit(options(.old.options))
        options(stringsAsFactors = FALSE, warn=-1)
        
        # read image
        image <- tiff::readTIFF(source = file.name, info = FALSE)
        image.grey <- edit.image(image = image, grey.mode = grey.mode)
        image.information <- array(data = 0, dim = dim(image))
    }
    
   
    
    # Start working with the image -----------------------------------------
    
    # Find the mid-point of the image
    start <- dim(image.grey)/2
    
    # Give information on middle and upper right part
    end <- dim(image.grey)
    
    middle.mean <- mean(
        image.grey[(start[1]-100):(start[1]+100),
                   (start[2]-100):(start[2]+100)])
    
    upper.right.corner.mean <- mean(
        image.grey[1:200, (end[2]-200):end[2]])
    lower.left.corner.mean <- mean(
        image.grey[(end[1]-200):end[1], 1:200])
    
    end.mean <- mean(upper.right.corner.mean, lower.left.corner.mean)
    
    ratio.mean <- middle.mean / end.mean
    
    # Mean value of the grey.image
    image.grey.mean <- mean(image.grey)
    image.grey.sd <- stats::sd(image.grey)
    
    if(is.null(parameter.for.end)){
        if(ratio.mean < 3){
            parameter.for.end <- 1.2
        }else if(image.grey.sd > 0.2){
            parameter.for.end <- 1.2
        }else if(image.grey.mean / image.grey.sd < 0.9){
            parameter.for.end <- 1.6
        }else{
            #print("The image color and brightness is unusual.")
            parameter.for.end <- 1.6
        }
    }
    
    
    # Find the edge going along every side of the image --------------------
    
    # initialize a vector which contains all points of the outline (as well
    # as deviations). edge contains (x,y)-pairs. (For x-y-axes of our
    # coordinate system.)
    edge <- c(0,0)
    
    # Move alongside upper border from left to right (x = 0, y = 0 to max)
    i <- 1
    
    while(i <= dim(image.grey)[2]){
        
        edge <- rbind(edge, findEdge(image.grey = image.grey,
                                     start.pixel = start,
                                     end.pixel = c(1,i),
                                     parameter.for.end))
        
        i <- i + distance
    }
    
    # Move alongside right border from top to bottom (x = 0 to max, y = max)
    i <- 1
    last.pixel.right <- dim(image.grey)[2]
    
    while(i <= dim(image.grey)[1]){
        
        edge <- rbind(edge, findEdge(image.grey = image.grey,
                                     start.pixel = start,
                                     end.pixel = c(i,last.pixel.right),
                                     parameter.for.end))
        
        i <- i + distance
    }
    
    # Move alongside lower border from right to left (x = max, y = max to 0)
    i <- last.pixel.right
    last.pixel.bottom <- dim(image.grey)[1]
    
    while(i >= 1){
        
        edge <- rbind(edge, findEdge(image.grey = image.grey,
                                     start.pixel = start,
                                     end.pixel = c(last.pixel.bottom,i),
                                     parameter.for.end))
        
        i <- i - distance
    }
    
    # Move alongside left border from bottom to top (x = max to 0, y = 0)
    i <- last.pixel.bottom
    
    while(i > 1){
        
        edge <- rbind(edge, findEdge(image.grey = image.grey,
                                     start.pixel = start,
                                     end.pixel = c(i,1),
                                     parameter.for.end))
        
        i <- i - distance
    }
    
    
    # Clean the outline vector and delete all outliers ---------------------
    
    # Remove first empty line and unname the vector
    edge <- edge[-1,]
    edge <- unname(edge)
    
    
    # Delete all points that lie on the borders of the image
    
    # top
    lines.to.delete <- which(edge[,2] == 1)
    if(length(lines.to.delete) > 0){
        edge <- edge[-lines.to.delete,]
    }
    
    # right
    lines.to.delete <- which(edge[,2] == ncol(image.grey))
    if(length(lines.to.delete) > 0){
        edge <- edge[-lines.to.delete,]
    }
    
    # bottom
    lines.to.delete <- which(edge[,1] == nrow(image.grey))
    if(length(lines.to.delete) > 0){
        edge <- edge[-lines.to.delete,]
    }
    
    # left
    lines.to.delete <- which(edge[,1] == 1)
    if(length(lines.to.delete) > 0){
        edge <- edge[-lines.to.delete,]
    }
    
    rm(lines.to.delete)
    
    # Delete all outliers
    
    # Calculate the mean distances between points
    
    number.of.points <- dim(edge)[1]
    
    df.edge <- data.frame(edge)
    # y1 and x1 are now the coordinates of a "normal" coordinate system.
    names(df.edge) <- c("y1", "x1")
    
    df.edge$y2 <- c(df.edge$y1[2:number.of.points], df.edge$y1[1])
    df.edge$x2 <- c(df.edge$x1[2:number.of.points], df.edge$x1[1])
    
    df.edge$dist <- (df.edge$x2-df.edge$x1)^2+(df.edge$y2-df.edge$y1)^2
    
    mean.distance <- mean(df.edge$dist)
    
    df.edge$outlier <- ifelse(
        test = df.edge$dist > parameter.distance.deviation * mean.distance,
        1, 0)
    
    outliers <- which(
        df.edge$dist > parameter.distance.deviation * mean.distance)
    
    edge.copy <- edge
    
    edge <- edge[-outliers,]
    outliers <- edge.copy[outliers,]
    
    rm(edge.copy)
    
    # Add outline, outliers and borders to image.information ---------------
    
    # Mark outline and outliers
    image.information[cbind(edge,1)] <- 1
    image.information[cbind(edge,2)] <- -1
    image.information[cbind(edge,3)] <- -1
    
    image.information[cbind(outliers, 1)] <- -1
    image.information[cbind(outliers, 2)] <- -1
    image.information[cbind(outliers, 3)] <- 1
    
    # Add to the image.information the frame of the otolith
    top.y    <- min(df.edge$y1[df.edge$outlier==0])
    right.x  <- max(df.edge$x1[df.edge$outlier==0])
    bottom.y <- max(df.edge$y1[df.edge$outlier==0])
    left.x   <- min(df.edge$x1[df.edge$outlier==0])
    
    
    top.border    <- getLineIndices(start.x = left.x, start.y = top.y,
                                    end.x = right.x, end.y = top.y)
    right.border  <- getLineIndices(start.x = right.x, start.y = top.y,
                                    end.x = right.x, end.y = bottom.y)
    bottom.border <- getLineIndices(start.x = right.x, start.y = bottom.y,
                                    end.x = left.x, end.y = bottom.y)
    left.border   <- getLineIndices(start.x = left.x, start.y = bottom.y,
                                    end.x = left.x, end.y = top.y)
    
    border <- rbind(top.border, right.border, bottom.border, left.border)
    
    image.information[cbind(border[,2], border[,1], 3)] <- -1
    image.information[cbind(border[,2], border[,1], 2)] <- 1
    image.information[cbind(border[,2], border[,1], 1)] <- -1
    
    # Save values of the border
    image.border <- c(top.y, right.x, bottom.y, left.x)
    # Save values of the left and right point for ring detection
    # (Be aware that x & y are the typical x & y)
    left.point  <- c(left.x , df.edge$y1[df.edge$x1 == left.x][1])
    right.point <- c(right.x, df.edge$y1[df.edge$x1 == right.x][1])
    
    # Export image (if function is used by itself) -------------------------
    if(!is.null(file.name)){
        
        # Add image.information to image
        image.information <- image + image.information
        
        image.information <-
            ifelse(image.information < 0, 0, image.information)
        image.information <-
            ifelse(image.information > 1, 1, image.information)
        
        # Save image
        save.file.name <- gsub("\\.tif+", "", file.name)
        save.file.name <- paste(save.file.name, "_outline.tif", sep="")
        
        tiff::writeTIFF(what = image.information,
                        where = save.file.name,
                        bits.per.sample = 8L, compression = "none",
                        reduce = TRUE)
        return(print(paste("The edited image has been saved as ",
                           save.file.name, ".", sep="")))
        
    }else{
        
        # If function is used by read.age()
        return(list(image.information, image.border,
                    left.point, right.point))
    }
    
}
