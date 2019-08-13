#' @title detect.outline
#' @description Detect the outline of the otolith
#' @details We will add more later...To be continued
#' @aliases detect.outline outline.detect
#' @author Kai Budde
#' @export detect.outline
#' @param file.name A character:Full path of an image that shall be used for
#' outline detection
#' @param image.grey A matrix of numbers: Grey values of the otolith image
#' (values between 0 and 1)
#' @param grey.mode A character: Determines how the grey image shall be
#' calculated
#' @param parameter.for.end A number: The edge is being found by calculating
#' a floating mean. The function marks an edge if the next point differs
#' more than floating mean divided by parameter.for.end
#' @param distance A number: Lines for finding the outline of the otolith
#' are this many pixels apart on the x- or y-axes
#' @param parameter.distance.deviation A number: Outliers get detected by
#' multiplying the parameter with the mean distance between points
#' @param image.information A matrix of numbers: Contains additional pixel
#' information of the image (values between 0 and 1)

detect.outline <- function(
    file.name = NULL,
    image.grey = NULL,
    grey.mode = "normal.grey",
    parameter.for.end = NULL,
    distance = 20,
    parameter.distance.deviation = 2,
    image.information = NULL){
    
    # Parameters of the function that shall not be changed -----------------
    
    # Range for mean calculation of parts of the image 
    pixel.length <- 100
    
    # Parameter for edge detection depending on the ratio of the
    # brightness in the middle and at the corners of the image
    parameter.for.end.low <- 1.2
    parameter.for.end.high <- 1.6
    
    # Minimum distance (percentage of max distance) of edge point from the
    # midpoint
    min.dist.from.midpoint <- 0.2
    
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
    
    
    # Calculate summary statistics of the image --------------------------------
    
    # Save the coordinates of the mid-point of the image
    mid.point <- dim(image.grey)/2
    
    # Save the coordinates of the lower right corner of the image
    # ([x=0,y=0] is at the top left corner)
    end.point <- dim(image.grey)

    # Mean value of the grey.image
    image.grey.mean <- mean(image.grey)
    image.grey.sd <- stats::sd(image.grey)
    
    # Calculate the parameter.for.end if not given as input parameter ------
    
    if(is.null(parameter.for.end)){
        # Calculate the mean value of the pixels in the middle of the image
        # within a box of size (2*pixel.length) * (2*pixel.length)
        middle.mean <- mean(
            image.grey[
                (mid.point[1]-pixel.length):(mid.point[1]+pixel.length),
                (mid.point[2]-pixel.length):(mid.point[2]+pixel.length)])
        
        # Calculate the mean value of the pixels in two corners
        upper.right.corner.mean <- mean(
            image.grey[
                1:(2*pixel.length),
                (end.point[2]-(2*pixel.length)):end.point[2]])
        
        lower.left.corner.mean <- mean(
            image.grey[
                (end.point[1]-(2*pixel.length)):end.point[1],
                1:(2*pixel.length)])

        # Calculate the ratio of the brightness within the middle and two
        # corners
        corner.mean <- mean(upper.right.corner.mean, lower.left.corner.mean)
        ratio.mean <- middle.mean / corner.mean
        
        # Choose the parameter.for.end value
        if(ratio.mean < 3){
            parameter.for.end <- parameter.for.end.low
        }else if(image.grey.sd > 0.2){
            parameter.for.end <- parameter.for.end.low
        }else if(image.grey.mean / image.grey.sd < 0.9){
            parameter.for.end <- parameter.for.end.high
        }else{
            #print("The image color and brightness is unusual.")
            parameter.for.end <- parameter.for.end.high
        }
    }
    
    
    # Find the edge of the otolith going along every side of the image -----
    
    # initialize a vector which contains all points of the outline (as well
    # as deviations). edge contains (x,y)-pairs. (For x-y-axes of our
    # coordinate system.)
    edge <- c(0,0)
    
    # Move alongside upper border from left to right
    # (x = 0, y = from 0 to max)
    i <- 1
    
    while(i <= dim(image.grey)[2]){
        
        edge <- rbind(edge, findEdge(image.grey = image.grey,
                                     start.pixel = mid.point,
                                     end.pixel = c(1,i),
                                     parameter.for.end))
        i <- i + distance
    }
    
    # Move alongside right border from top to bottom
    # (x = from 0 to max, y = max)
    i <- 1
    last.pixel.right <- dim(image.grey)[2]
    
    while(i <= dim(image.grey)[1]){
        
        edge <- rbind(edge, findEdge(image.grey = image.grey,
                                     start.pixel = mid.point,
                                     end.pixel = c(i,last.pixel.right),
                                     parameter.for.end))
        i <- i + distance
    }
    
    # Move alongside lower border from right to left
    # (x = max, y = from max to 0)
    i <- last.pixel.right
    last.pixel.bottom <- dim(image.grey)[1]
    
    while(i >= 1){
        
        edge <- rbind(edge, findEdge(image.grey = image.grey,
                                     start.pixel = mid.point,
                                     end.pixel = c(last.pixel.bottom,i),
                                     parameter.for.end))
        i <- i - distance
    }
    
    # Move alongside left border from bottom to top
    # (x = from max to 0, y = 0)
    i <- last.pixel.bottom
    
    while(i > 1){
        
        edge <- rbind(edge, findEdge(image.grey = image.grey,
                                     start.pixel = mid.point,
                                     end.pixel = c(i,1),
                                     parameter.for.end))
        i <- i - distance
    }
    
    
    # Delete all points of the outline vector at borders of the image ------
    
    # Remove first empty line and unname the vector
    edge <- edge[-1,]
    edge <- unname(edge)
    
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
    
    
    # Delete all outliers --------------------------------------------------
    
    # Number of edge points found
    number.of.edge.points <- dim(edge)[1]
    
    # Copy the vector with the edge positions into a data.frame
    df.edge <- data.frame(edge)
    
    # y1 and x1 are now the coordinates of a "normal" coordinate system.
    names(df.edge) <- c("y1", "x1")
    
    # Add another column to the df with the shifted x/y.coordinates
    # (the next neighbors of all points are now in the next column)
    df.edge$y2 <- c(df.edge$y1[2:number.of.edge.points], df.edge$y1[1])
    df.edge$x2 <- c(df.edge$x1[2:number.of.edge.points], df.edge$x1[1])
    
    # Calculate the Euclidean distance of each edge point to the next one
    df.edge$dist <- sqrt((df.edge$x2-df.edge$x1)^2 +
                             (df.edge$y2-df.edge$y1)^2)
    
    # Calculate the mean distance between two edge points
    mean.distance <- mean(df.edge$dist)
    
    # Calculate distance from each edge point to midpoint
    df.edge$distToMidpoint <-
        sqrt((df.edge$x1-mid.point[2])^2+(df.edge$y1-mid.point[1])^2)
    
    # Remove all outlier that are more away than
    # min.dist.from.midpoint * max(df.edge$distToMidpoint)
    
    df.edge$outlier <- ifelse(
        test = (df.edge$distToMidpoint <
                    (min.dist.from.midpoint * max(df.edge$distToMidpoint))),
        1, 0)
    
    # Remove all outlier that are more away than
    # parameter.distance.deviation * mean.distance
    
    # Mark the outliers in the data frame
    df.edge$outlier2 <- ifelse(
        test = df.edge$dist > parameter.distance.deviation * mean.distance,
        1, 0)
    
    # Combine both outlier columns
    df.edge$outlier <- as.numeric(df.edge$outlier | df.edge$outlier2)
    
    # Get the indeces of the outliers
    outliers <- which( df.edge$outlier == 1 )
    
    # edge contains only points that are not outliers
    edge.copy <- edge
    edge <- edge[-outliers,]
    
    # outlier contains all outliers
    outliers <- edge.copy[outliers,]
    
    rm(edge.copy)
    rm(number.of.edge.points)
    
    
    # Calculate the bordering frame of the otolith -------------------------
    
    # Calculate the outermost points of the edge
    top.y    <- min(df.edge$y1[df.edge$outlier==0])
    right.x  <- max(df.edge$x1[df.edge$outlier==0])
    bottom.y <- max(df.edge$y1[df.edge$outlier==0])
    left.x   <- min(df.edge$x1[df.edge$outlier==0])
    
    # Save values of the border points in vector
    image.border.points <- c(top.y, right.x, bottom.y, left.x)
    
    # Save values of the left and right points for ring detection
    # (Be aware that x & y are the typical x & y)
    left.point  <- c(left.x , df.edge$y1[df.edge$x1 == left.x][1])
    right.point <- c(right.x, df.edge$y1[df.edge$x1 == right.x][1])
    
    # Calculate the lines of the frame using the HelperFunction
    # getLineIndices()
    top.border    <- getLineIndices(start.x = left.x, start.y = top.y,
                                    end.x = right.x, end.y = top.y)
    right.border  <- getLineIndices(start.x = right.x, start.y = top.y,
                                    end.x = right.x, end.y = bottom.y)
    bottom.border <- getLineIndices(start.x = right.x, start.y = bottom.y,
                                    end.x = left.x, end.y = bottom.y)
    left.border   <- getLineIndices(start.x = left.x, start.y = bottom.y,
                                    end.x = left.x, end.y = top.y)
    
    # Connect all points of the lines to the varible border
    border <- rbind(top.border, right.border, bottom.border, left.border)
    
    
    # Add outline, outliers and broder frame to image.information ----------
    
    # Mark outline points in red
    image.information[cbind(edge,1)] <- 1
    image.information[cbind(edge,2)] <- -1
    image.information[cbind(edge,3)] <- -1
    
    # Mark outliers blue 
    image.information[cbind(outliers, 1)] <- -1
    image.information[cbind(outliers, 2)] <- -1
    image.information[cbind(outliers, 3)] <- 1
    
    # Mark border lines in green
    image.information[cbind(border[,2], border[,1], 3)] <- -1
    image.information[cbind(border[,2], border[,1], 2)] <- 1
    image.information[cbind(border[,2], border[,1], 1)] <- -1
    
    
    # Export image if function is used by itself ---------------------------
    if(!is.null(file.name)){
        
        # Add image.information to image
        image.information <- image + image.information
        
        image.information <-
            ifelse(image.information < 0, 0, image.information)
        image.information <-
            ifelse(image.information > 1, 1, image.information)
        
        # Save image
        save.file.name <- gsub("\\.tif+", "", file.name)
        save.file.name <- paste(save.file.name, "_edge.tif", sep="")
        
        tiff::writeTIFF(what = image.information,
                        where = save.file.name,
                        bits.per.sample = 8L, compression = "none",
                        reduce = TRUE)
        return(print(paste("The edited image has been saved as ",
                           save.file.name, ".", sep="")))
        
    }
    
    # Return image.information, image.border.points, left.point, right.point
    # if function is used by read.age() ------------------------------------
    return(list(image.information, image.border.points,
                left.point, right.point))
    
}
