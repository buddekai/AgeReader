
############################################################################
# Helper Functions                                                         #
############################################################################

# Add columns to df.user.file ----------------------------------------------

add.col.to.user.file <- function(df.user.file){
    
    columns.of.df <- names(df.user.file)
    
    if(sum(grepl("age", columns.of.df)) == 0){
        df.user.file$age             <- NA
    }
    if(sum(grepl("cohort", columns.of.df)) == 0){
        df.user.file$cohort          <- NA
    }
    if(sum(grepl("outer_zone", columns.of.df)) == 0){
        df.user.file$outer_zone      <- NA
    }
    if(sum(grepl("readability", columns.of.df)) == 0){
        df.user.file$readability     <- NA
    }
    if(sum(grepl("number_of_rings_left", columns.of.df)) == 0){
        df.user.file$number_of_rings_left   <- NA
    }
    if(sum(grepl("number_of_rings_right", columns.of.df)) == 0){
        df.user.file$number_of_rings_right   <- NA
    }
    
    # Old columns
    #if(sum(grepl("PROCESSED", columns.of.df)) == 0){
    #    df.user.file$PROCESSED       <- "No"
    #}
    #if(sum(grepl("SCALE_mperpix", columns.of.df)) == 0){
    #    df.user.file$SCALE_mperpix   <- NA
    #}
    #if(sum(grepl("top_border", columns.of.df)) == 0){
    #    df.user.file$top_border      <- NA
    #}
    #if(sum(grepl("right_border", columns.of.df)) == 0){
    #    df.user.file$right_border    <- NA
    #}
    #if(sum(grepl("lower_border", columns.of.df)) == 0){
    #    df.user.file$lower_border    <- NA
    #}
    #if(sum(grepl("left_border", columns.of.df)) == 0){
    #    df.user.file$left_border     <- NA
    #}
    #if(sum(grepl("left_point_x", columns.of.df)) == 0){
    #    df.user.file$left_point_x    <- NA
    #}
    #if(sum(grepl("left_point_y", columns.of.df)) == 0){
    #    df.user.file$left_point_y    <- NA
    #}
    #if(sum(grepl("right_point_x", columns.of.df)) == 0){
    #    df.user.file$right_point_x   <- NA
    #}
    #if(sum(grepl("right_point_y", columns.of.df)) == 0){
    #    df.user.file$right_point_y   <- NA
    #}
    #if(sum(grepl("equal_ring_number", columns.of.df)) == 0){
    #    df.user.file$equal_ring_number   <- NA
    #}
    #if(sum(grepl("diameter_opaque_ring_1", columns.of.df)) == 0){
    #    df.user.file$diameter_opaque_ring_1   <- NA
    #}
    #if(sum(grepl("diameter_opaque_ring_2", columns.of.df)) == 0){
    #    df.user.file$diameter_opaque_ring_2   <- NA
    #}
    #if(sum(grepl("diameter_opaque_ring_3", columns.of.df)) == 0){
    #    df.user.file$diameter_opaque_ring_3   <- NA
    #}
    #if(sum(grepl("diameter_hyaline_ring_1", columns.of.df)) == 0){
    #    df.user.file$diameter_hyaline_ring_1  <- NA
    #}
    #if(sum(grepl("diameter_hyaline_ring_2", columns.of.df)) == 0){
    #    df.user.file$diameter_hyaline_ring_2  <- NA
    #}
    #if(sum(grepl("diameter_hyaline_ring_3", columns.of.df)) == 0){
    #    df.user.file$diameter_hyaline_ring_3  <- NA
    #}
    #if(sum(grepl("otolith_diameter", columns.of.df)) == 0){
    #    df.user.file$otolith_diameter  <- NA
    #}
    
    return(df.user.file)
}

## Read in czi-file in order to get the scale of the image ------------------
#
#get.scale <- function(file.name){
#    
#    # Save all meta data
#    metadata <- paste(readLines(file.name, n = 461),
#                      collapse = " ")
#    
#    # Find and keep the information about the scale
#    metadata <- gsub(".*<Metadata>", "<Metadata>", metadata)
#    metadata <- gsub(" +", "", metadata)
#    
#    scale <- gsub(".*(<Items>.+</Items>).*", "\\1", metadata)
#    scale.x <-
#        as.numeric(gsub(".*<DistanceId=\"X\"><Value>(.{1,30})</Value>.*",
#                        "\\1", scale))
#    scale.y <-
#        as.numeric(gsub(".*<DistanceId=\"Y\"><Value>(.{1,30})</Value>.*",
#                        "\\1", scale))
#    if(!is.na(scale.x)){
#        if(scale.x == scale.y){
#            # scale in x- and y-direction is the same. This should be the case.
#            return(scale.x)
#        }else{
#            print("Scales in x- and y-directions are not the same.")
#            return()
#        }
#    }else{
#        return(NA)
#    }
#}



# getLineIndices calculates all points needed for a straight computer ------
# line connecting two points.

getLineIndices <- function(start.x, start.y, end.x, end.y){
    
    # Distinguish between the following cases:
    # 1. Horizontal 2. vertical line
    # 3. Slope between 0 and 1
    # 4. Slope larger than 1
    # 5. Slope between 0 and -1
    # 6. Slope smaller than -1
    
    reverse.it <- FALSE
    
    # 1st case
    if(end.y == start.y){
        points <- matrix(c(start.x:end.x))
        points <- cbind(points, start.y)
        
    }else if(end.x == start.x){ # 2nd case
        points <- matrix(c(start.y:end.y))
        points <- cbind(start.x, points)
        
    }else{
        
        slope <- (end.y - start.y)/(end.x-start.x)
        
        # 3rd case
        if(0 < slope & slope <= 1){
            
            if(end.y - start.y < 0){
                .dumx <- start.x
                .dumy <- start.y
                
                start.x <- end.x
                start.y <- end.y
                
                end.x <- .dumx
                end.y <- .dumy
                
                reverse.it <- TRUE
            }
            
            A <- 2 * (end.y - start.y)
            B <- A - 2 * (end.x - start.x)
            P <- A - (end.x - start.x)
            points <- c(start.x, start.y)
            current.point <- c(start.x, start.y)
            
            while(!isTRUE(all.equal(current.point,c(end.x,end.y)))){
                if(P <= 0){
                    current.point <-
                        c(current.point[1]+1, current.point[2])
                    points <- rbind(points, current.point)
                    P <- A + P
                }else{
                    current.point <-
                        c(current.point[1]+1, current.point[2]+1)
                    points <- rbind(points, current.point)
                    P <- B + P
                }
            }
        }
        
        # 4th case
        if(slope > 1){
            
            if(end.y - start.y < 0){
                .dumx <- start.x
                .dumy <- start.y
                
                start.x <- end.x
                start.y <- end.y
                
                end.x <- .dumx
                end.y <- .dumy
                
                reverse.it <- TRUE
            }
            
            A <- 2*(end.x-start.x)
            B <- A - 2*(end.y-start.y)
            P <- A-(end.y-start.y)
            points <- c(start.x, start.y)
            current.point <- c(start.x, start.y)
            
            while(!isTRUE(all.equal(current.point,c(end.x,end.y)))){
                if(P <= 0){
                    current.point <-
                        c(current.point[1], current.point[2]+1)
                    points <- rbind(points, current.point)
                    P <- A + P
                }else{
                    current.point <-
                        c(current.point[1]+1, current.point[2]+1)
                    points <- rbind(points, current.point)
                    P <- B + P
                }
            }
        }
        
        # 5th case
        
        if(slope < 0 & slope >= -1){
            
            if(end.y - start.y > 0){
                .dumx <- start.x
                .dumy <- start.y
                
                start.x <- end.x
                start.y <- end.y
                
                end.x <- .dumx
                end.y <- .dumy
                
                reverse.it <- TRUE
            }
            
            A <- abs(2*(end.y-start.y))
            B <- A - 2*abs(end.x-start.x)
            P <- A - abs(end.x-start.x)
            points <- c(start.x, start.y)
            current.point <- c(start.x, start.y)
            
            while(!isTRUE(all.equal(current.point,c(end.x,end.y)))){
                if(P <= 0){
                    current.point <-
                        c(current.point[1]+1, current.point[2])
                    points <- rbind(points, current.point)
                    P <- A + P
                }else{
                    current.point <-
                        c(current.point[1]+1, current.point[2]-1)
                    points <- rbind(points, current.point)
                    P <- B + P
                }
            }
        }
        
        # 6th case
        if(slope < -1){
            
            if(end.y - start.y > 0){
                .dumx <- start.x
                .dumy <- start.y
                
                start.x <- end.x
                start.y <- end.y
                
                end.x <- .dumx
                end.y <- .dumy
                
                reverse.it <- TRUE
            }
            
            A <- 2*abs(end.x-start.x)
            B <- A - 2*abs(end.y-start.y)
            P <- A-abs(end.y-start.y)
            points <- c(start.x, start.y)
            current.point <- c(start.x, start.y)
            
            while(!isTRUE(all.equal(current.point,c(end.x,end.y)))){
                if(P <= 0){
                    current.point <-
                        c(current.point[1], current.point[2]-1)
                    points <- rbind(points, current.point)
                    P <- A + P
                }else{
                    current.point <-
                        c(current.point[1]+1, current.point[2]-1)
                    points <- rbind(points, current.point)
                    P <- B + P
                }
            }
        }
        
    }
    
    if(reverse.it){
        points <- apply(points, 2, rev)
    }
    
    points <- unname(points)
    
    return(points)
}



# Function to find a sudden change in brightness while going alongside a ---
# line. This change marks the edge (outline) of the otolith.

# parameter.for.end signals, when the edge is being found. (When brightness
# of a current point is below moving.average / parameter.for.end .)

# TODO: Try another method of edge detection
# (https://en.wikipedia.org/wiki/Edge_detection)

findEdge <- function(image.grey,
                     start.pixel, end.pixel,
                     parameter.for.end){
    
    # results contains the line-indices (y,x)-pairs (in our coordinate
    # system)
    results <- getLineIndices(start.x = start.pixel[2],
                              start.y = start.pixel[1],
                              end.x = end.pixel[2],
                              end.y = end.pixel[1])
    
    moving.average <- image.grey[start.pixel[1], start.pixel[2]]
    
    end <- dim(results)[1]
    
    i <- 1
    
    current.point <- image.grey[results[i, 2], results[i, 1]]
    test <- current.point > moving.average / parameter.for.end
    
    while(test){
        i <- i+1
        current.point <- image.grey[results[i, 2], results[i, 1]]
        
        moving.average <- (3 * moving.average + current.point) / 4
        
        test <- current.point > moving.average / parameter.for.end
        if(i == end){
            test <- FALSE
        }
    }
    
    # result is the edge which has been found.
    result <- c(results[i, 2], results[i, 1])
    
    return(result)
}


# Count rings --------------------------------------------------------------
count.rings <- function(df.line.values){
    ring <- 1
    if(df.line.values$ring[1] != 0){
        ring.change <- TRUE
    }else{
        ring.change <- FALSE
    }
    
    
    for(i in 1:length(df.line.values$ring)){
        
        if(df.line.values$ring[i] == 0){
            
            if(ring.change == TRUE){
                ring.change <- FALSE
                ring <- ring + 1
            }
            
        }else{
            df.line.values$ring[i] <- ring
            
            if(ring.change == FALSE){
                ring.change <- TRUE
            }
        }
    }
    
    return(df.line.values)
}


# Get Block Pixels ---------------------------------------------------------
getBlockPixels <- function(upper.left.corner, lower.right.corner){
    vertical.pixels <- upper.left.corner[1]:lower.right.corner[1]
    horizontal.pixels <- upper.left.corner[2]:lower.right.corner[2]
    
    blockPixels <- as.matrix(expand.grid(vertical.pixels, horizontal.pixels,
                                         stringsAsFactors = FALSE))
    
    return(blockPixels)
}


# Get Corner Pixels --------------------------------------------------------
getCornerPixels <- function(upper.left.corner, lower.right.corner){
    
    vertical.pixels <- upper.left.corner[1]:lower.right.corner[1]
    horizontal.pixels <- upper.left.corner[2]:lower.right.corner[2]
    
    upper.line <- matrix(
        data = c(rep(upper.left.corner[1], length(horizontal.pixels)),
                 horizontal.pixels), ncol = 2)
    
    lower.line <- matrix(
        data = c(rep(lower.right.corner[1], length(horizontal.pixels)),
                 horizontal.pixels), ncol = 2)
    
    right.line <- matrix(
        data = c(vertical.pixels,
                 rep(lower.right.corner[2], length(vertical.pixels))),
        ncol = 2)
    
    left.line <- matrix(
        data = c(vertical.pixels,
                 rep(upper.left.corner[2], length(vertical.pixels))),
        ncol = 2)
    
    
    cornerPixels <- rbind(upper.line, right.line, lower.line, left.line)
    cornerPixels <- cornerPixels[!duplicated(cornerPixels),]
    
    rm(upper.line, right.line, lower.line, left.line)
    
    return(cornerPixels)
}