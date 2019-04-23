############################################################################
# detect.rings function                                                    #
############################################################################

detect.rings <- function(image.grey = NULL,
                         image.information = NULL,
                         first.point = NULL,
                         second.point = NULL,
                         line.to.follow = NULL,
                         par.hyaline = 1.1,
                         points.to.jump = 10,
                         points.for.reconnecting = 20,
                         remove.points.at.beginning = 0,
                         remove.points.at.end = 10,
                         t = 2,
                         difference.in.percent = 0.1,
                         show.all.possible.rings = FALSE){
    
    # Start working with the image -----------------------------------------
    
    if(is.null(line.to.follow)){
        connecting.line <- getLineIndices(start.x = first.point[1],
                                          start.y = first.point[2],
                                          end.x = second.point[1],
                                          end.y = second.point[2])
    }else{
        index.of.line.start <- which(first.point[1] == line.to.follow[,1])
        index.of.line.end   <- which(second.point[1] == line.to.follow[,1])
        
        if(index.of.line.start < index.of.line.end){
            connecting.line <-
                line.to.follow[index.of.line.start:index.of.line.end,]
        }else{
            connecting.line <-
                line.to.follow[index.of.line.end:index.of.line.start,]
        }
        
        # Reverse the order of the values to start calculations in the
        # center of the otolith
        connecting.line[,1] <- rev(connecting.line[,1])
        connecting.line[,2] <- rev(connecting.line[,2])
    }
    
    
    df.line.values <- data.frame(x = connecting.line[,2],
                                   y = connecting.line[,1])
    
    df.line.values.copy <- df.line.values
    df.line.values.copy$rownumber <- 1:nrow(df.line.values.copy)
    
    df.line.values$line.values <- image.grey[cbind(connecting.line[,2],
                                     connecting.line[,1])]
    
    
    
    
    # Clean the values of the line -----------------------------------------
    # Remove end of the line which might already be in the outer zone of the
    # otolith
    if(length(df.line.values$line.values) < remove.points.at.end){
        remove.points.at.end <- 0
    }
    
    df.line.values <-
        df.line.values[1:(length(df.line.values$line.values) -
                                          remove.points.at.end),]
    
    # Find outliers and delete them
    lower.limit <- stats::median(df.line.values$line.values) -
        t * stats::sd(df.line.values$line.values)
    upper.limit <- stats::median(df.line.values$line.values) +
        t * stats::sd(df.line.values$line.values)
    
    df.line.values <- df.line.values[
        !df.line.values$line.values > upper.limit, ]
    df.line.values <- df.line.values[
        !df.line.values$line.values < lower.limit, ]
    
    # Smooth the curve
    smooth.line.values <- df.line.values$line.values
    for(j in 1:100){
        for(i in 1:(length(smooth.line.values)-2)){
            smooth.line.values[i+1] <-
                0.5 * smooth.line.values[i+1] +
                0.25 * (smooth.line.values[i] +
                            smooth.line.values[i+2])
        }
    }
    
    names(df.line.values)[names(df.line.values) == "line.values"] <- 
        "original.line.values"
    df.line.values$value <- smooth.line.values

    # Ring detection 1: Find the first ring --------------------------------
    
    # Find the first ring within the first half of the line
    find.first.ring.within <- as.integer(nrow(df.line.values)/2)
    
    limit1 <- par.hyaline * mean(
        df.line.values$value[1:find.first.ring.within])
    
    df.line.values$ring <- ifelse(df.line.values$value > limit1, 1, 0)
    
    
    # Try to merge all smaller rings to a bigger one depending on the
    # parameters chosen
    points.to.jump.copy <- points.to.jump
    
    while(points.to.jump > 0){
        pattern <- c(1, rep(0, points.to.jump), 1)
        pattern <- paste(pattern, collapse = "")
        
        replacement <- c(1, rep(1, points.to.jump), 1)
        replacement <- paste(replacement, collapse = "")
        
        hyaline.vector <- paste(df.line.values$ring, collapse = "")
        hyaline.vector <- gsub(pattern, replacement, hyaline.vector)
        
        df.line.values$ring <-
            as.numeric(unlist(strsplit(hyaline.vector, split = "")))
        
        points.to.jump <- points.to.jump - 1
    }
    
    points.to.jump <- points.to.jump.copy
    
    # Count rings
    df.line.values <- count.rings(df.line.values = df.line.values)
    number.of.rings <- max(df.line.values$ring)
    
    # Include all found rings to the image
    if(show.all.possible.rings == TRUE){
        hyline.points <- cbind(df.line.values$x[df.line.values$ring!=0],
                               df.line.values$y[df.line.values$ring!=0])
        image.information[
            cbind(hyline.points[,1], hyline.points[,2], 3)] <- 1
    }
    
    # Delete rings which are too close to the starting point
    if(length(which(df.line.values$ring == 1)) > 0){
        if(which(df.line.values$ring == 1)[1] <
           remove.points.at.beginning){
            df.line.values$ring[df.line.values$ring == 1] <- 0
        }
    }
    if(length(which(df.line.values$ring == 2)) > 0){
        if(which(df.line.values$ring == 2)[1] <
           remove.points.at.beginning){
            df.line.values$ring[df.line.values$ring == 2] <- 0
        }
    }
    
    # Count rings
    df.line.values <- count.rings(df.line.values = df.line.values)
    number.of.rings <- max(df.line.values$ring)
    
    # Delete all hyaline rings which have no real maxima
    if(number.of.rings > 0){
        for(i in 1:number.of.rings){
            max.index  <- which(
                df.line.values$value == max(
                    df.line.values$value[df.line.values$ring == i]))
            last.index <- which(
                df.line.values$ring == i)[length(
                    which(df.line.values$ring == i))]
            
            if(max.index == last.index){
                df.line.values$ring[df.line.values$ring == i] <- 0
            }
        }
    }
    
    # Count rings
    df.line.values <- count.rings(df.line.values = df.line.values)
    number.of.rings <- max(df.line.values$ring)
    
    # Delete rings that are too close to each other
    if(number.of.rings > 1){
        for(i in 1:(number.of.rings-1)){
            last.point <- which(df.line.values$ring == (i))[
                length(which(df.line.values$ring == (i)))]
            first.point <- which(df.line.values$ring == (i+1))[1]
            ring.seperating.points <- first.point - last.point - 1
            
            if(ring.seperating.points < points.for.reconnecting){
                if(all(df.line.values$value[last.point:first.point] >
                       mean(df.line.values$value))){
                    df.line.values$ring[last.point:first.point] <- 1
                }
            }
        }
    }
    
    # Delete all but the first ring
    
    df.line.values$ring[df.line.values$ring != 1] <- 0
    
    # Ring detection 2: Find all other (but the last) rings ----------------
    
    # Start at the last value of the first ring
    
    # Find all maxima und minima after the first ring
    
    last.index.of.first.ring <-
        which(df.line.values$ring == 1)[length(
            which(df.line.values$ring == 1))]
    
    # If there is no first ring which could be found, start at the midpoint
    if(length(last.index.of.first.ring) == 0){
        last.index.of.first.ring <- find.first.ring.within
    }
    
    index.of.minima <- NULL
    index.of.maxima <- NULL
    
    final.index.of.minima <- NULL
    final.index.of.maxima <- NULL
    
    dummy.min <- df.line.values$value[last.index.of.first.ring]
    dummy.max <- 0
    dummy.counter <- 0
    
    find.min <- TRUE
    find.max <- FALSE
    
    df.line.values$Anstieg <- NA
    for(i in last.index.of.first.ring : length(df.line.values$value)){
        
        if(df.line.values$value[i] > df.line.values$value[i-1]){
            df.line.values$Anstieg[i] <- "up"
            
            if(find.min){
                index.of.minima <- c(index.of.minima, i-1)
                find.min <- FALSE
            }
            find.max <- TRUE
            
        }else{
            df.line.values$Anstieg[i] <- "down"
            
            if(find.max){
                index.of.maxima <- c(index.of.maxima, i-1)
                find.max <- FALSE
            }
            find.min <- TRUE
        }
        
    }
    
    
    
    df.line.values$max <- 0
    df.line.values$min <- 0
    
    df.line.values$max[index.of.maxima] <- 1
    df.line.values$min[index.of.minima] <- 1
    
    # Delete last minimum, if it is one too much
    if((length(index.of.minima) - 1) == length(index.of.maxima)){
        df.line.values$min[index.of.minima[length(index.of.minima)]] <- 0
        index.of.minima <- index.of.minima[-length(index.of.minima)]
    }
    
    # Delete extrema which are not real ones
    if(length(index.of.minima) > 0 &
       length(index.of.minima) == length(index.of.maxima)){
        if(index.of.minima[1] < index.of.maxima[1]){
            if(length(index.of.minima) > 1){
                
                i <- 1
                
                # Begin with rising branch
                while(i == 1 & length(index.of.maxima) > 0){
                    
                    # Find correct minimum
                    dum.current.max <-
                        df.line.values$value[index.of.maxima[i]]
                    dum.current.min <-
                        df.line.values$value[index.of.minima[i]]
                    dum.current.fraction <-
                        dum.current.max / dum.current.min
                    
                    # Delete the maximum if the difference is not great
                    # enough and choose the smallest minimum
                    if(dum.current.fraction < par.hyaline){
                        
                        # Delete the last maximum and minimum in case of
                        # only one
                        if(length(index.of.maxima) == 1){
                            df.line.values$max[index.of.maxima[i]] <- 0
                            index.of.maxima <- index.of.maxima[-i]
                            df.line.values$min[index.of.minima[i]] <- 0
                            index.of.minima <- index.of.minima[-i]
                        }else{
                            df.line.values$max[index.of.maxima[i]] <- 0
                            index.of.maxima <- index.of.maxima[-i]
                            
                            if(df.line.values$value[index.of.minima[i]] <
                               df.line.values$value[index.of.minima[i+1]]){
                                df.line.values$min[index.of.minima[i+1]] <-
                                    0
                                index.of.minima <- index.of.minima[-(i+1)]
                            }else{
                                df.line.values$min[index.of.minima[i]] <- 0
                                index.of.minima <- index.of.minima[-i]
                            }
                        }
                    }else{
                        i <- i + 1
                    }
                }
                
                # Check all other extrema
                while(i < length(index.of.maxima)){
                    
                    changed <- FALSE
                    # Falling branches
                    # Find correct maximum
                    dum.current.max <-
                        df.line.values$value[index.of.maxima[i-1]]
                    dum.current.min <-
                        df.line.values$value[index.of.minima[i]]
                    dum.current.fraction <-
                        dum.current.max / dum.current.min
                    
                    # Delete the minimum if the difference is not great
                    # enough and choose the largest maximum
                    if(dum.current.fraction < par.hyaline){
                        
                        df.line.values$min[index.of.minima[i]] <- 0
                        index.of.minima <- index.of.minima[-i]
                        
                        if(df.line.values$value[index.of.maxima[i-1]] <
                           df.line.values$value[index.of.maxima[i]]){
                            df.line.values$max[index.of.maxima[i-1]] <- 0
                            index.of.maxima <- index.of.maxima[-(i-1)]
                        }else{
                            df.line.values$max[index.of.maxima[i]] <- 0
                            index.of.maxima <- index.of.maxima[-i]
                        }
                        
                        changed <- TRUE
                    }
                    
                    # Rising branch
                    # Find correct maximum
                    if(i < length(index.of.minima)){
                        dum.current.max <-
                            df.line.values$value[index.of.maxima[i]]
                        dum.current.min <-
                            df.line.values$value[index.of.minima[i]]
                        dum.current.fraction <-
                            dum.current.max / dum.current.min
                        
                        # Delete the maximum if the difference is not great
                        # enough and choose the smallest minimum
                        if(dum.current.fraction < par.hyaline){
                            
                            df.line.values$max[index.of.maxima[i]] <- 0
                            index.of.maxima <- index.of.maxima[-i]
                            
                            if(df.line.values$value[index.of.minima[i]] <
                               df.line.values$value[index.of.minima[i+1]]){
                                df.line.values$min[index.of.minima[i+1]] <-
                                    0
                                index.of.minima <- index.of.minima[-(i+1)]
                            }else{
                                df.line.values$min[index.of.minima[i]] <- 0
                                index.of.minima <- index.of.minima[-i]
                            }
                            
                            changed <- TRUE
                        }
                    }
                    
                    # Start all over at the initial i-value if a value
                    # has been deleted
                    if(!changed){
                        i <- i + 1
                    }
                    
                }
            }
        }else{
            print("We could not find a minimum after the first maximum!")
        }
    }else{
        print("We have not found the same amount of minima/maxima.")
    }
    
    
    # Find rings between two minima by calculating the mean value between
    # two maxima
    
    if(sum(df.line.values$max) - 1 > 0){
        for(i in 1:(sum(df.line.values$max)-1)){
            
            if(i==1){
                current.range <-
                    last.index.of.first.ring:which(df.line.values$max==1)[1]
            }else{
                current.range <-
                    which(df.line.values$max==1)[i-1]:which(
                        df.line.values$max==1)[i]
            }
            
            current.mean.value <- mean(df.line.values$value[current.range])
            
            # Mark the ring between two minima
            current.range <-
                which(df.line.values$min==1)[i]:which(
                    df.line.values$min==1)[i+1]
            
            copy.par.hyaline <- par.hyaline
            current.range.ring <- which(
                df.line.values$value[current.range] >
                    par.hyaline * current.mean.value)
            while(length(current.range.ring)==0 & par.hyaline > 1.001){
                par.hyaline <- ((par.hyaline - 1)/2+1)
                current.range.ring <- which(
                    df.line.values$value[current.range] >
                        par.hyaline * current.mean.value)
            }
            par.hyaline <- copy.par.hyaline
            
            df.line.values$ring[current.range[current.range.ring]] <- 1
        }
        
        # Ring detection 3: Find the last ring -----------------------------
        current.range <-
            which(df.line.values$min==1)[sum(df.line.values$min)]:
            length(df.line.values$value)
        current.mean.value <- mean(df.line.values$value[current.range])
        
        # Mark the ring between two minima
        current.range.ring <- which(df.line.values$value[current.range] >
                                        par.hyaline * current.mean.value)
        
        # If the maximum is not large enough
        copy.par.hyaline <- par.hyaline
        current.range.ring <- which(df.line.values$value[current.range] >
                                        par.hyaline * current.mean.value)
        
        while(length(current.range.ring)==0 & par.hyaline > 1.001){
            par.hyaline <- ((par.hyaline - 1)/2+1)
            current.range.ring <-
                which(df.line.values$value[current.range] >
                          par.hyaline* current.mean.value)
        }
        par.hyaline <- copy.par.hyaline
        
        df.line.values$ring[current.range[current.range.ring]] <- 1
        
        # Count rings
        df.line.values <- count.rings(df.line.values = df.line.values)
        number.of.rings <- max(df.line.values$ring)
        
    }
    
    # Add lost rows and fill gaps inside the rings -------------------------
    df.line.values <- merge(df.line.values.copy,
                            df.line.values, all.x = TRUE)
    df.line.values <- df.line.values[order(df.line.values$rownumber),]
    row.names(df.line.values) <- df.line.values$rownumber
    df.line.values <- df.line.values[,-3]
    
    
    # Start to fill NAs from the end and the beginning
    current.row <- nrow(df.line.values)
    while(is.na(df.line.values$ring[current.row])){
        df.line.values$ring[current.row] <- 0
        current.row <- current.row - 1
    }
    current.row <- 1
    while(is.na(df.line.values$ring[current.row])){
        df.line.values$ring[current.row] <- 0
        current.row <- current.row + 1
    }
    
    number.of.nas <- sum(is.na(df.line.values$ring))
    rows.of.nas <- which(is.na(df.line.values$ring))
    
    if(number.of.nas > 0){
        for(i in 1:number.of.nas){
            current.row <- rows.of.nas[i]
            
            # add ring identification, if the row is inside a ring
            if(current.row > 1 & current.row < nrow(df.line.values)){
                ring.value.before <- NA
                step <- 1
                while(is.na(ring.value.before)){
                    ring.value.before <-
                        df.line.values$ring[current.row - step]
                    step <- step + 1
                }
                ring.value.after <- NA
                step <- 1
                while(is.na(ring.value.after)){
                    ring.value.after <-
                        df.line.values$ring[current.row + step]
                    step <- step + 1
                }
                
                if(ring.value.before == ring.value.after){
                    df.line.values$ring[current.row] <- ring.value.before
                }else{
                    df.line.values$ring[current.row] <- 0
                }
            }
        }
    }
    
    rm(current.row)
    
    # Cleaning up ----------------------------------------------------------
    
    # Delete first ring, if it is smaller than points.for.reconnection
    # and if its brightest point is darker than the brightest point from the
    # second ring
    
    if(sum(df.line.values$ring == 1) < points.for.reconnecting){
        brightness.first.ring <-
            max(df.line.values$values[df.line.values$ring == 1])
        brightness.second.ring <-
            max(df.line.values$values[df.line.values$ring == 2])
        if(brightness.first.ring <= brightness.second.ring){
            #print("Deleting first ring(s).")
            df.line.values$ring[df.line.values$ring == 1] <- 0
            df.line.values$ring[df.line.values$ring == 2] <- 1
        }
    }
    
    # Count rings
    df.line.values <- count.rings(df.line.values = df.line.values)
    number.of.rings <- max(df.line.values$ring)
    
    # Delete small rings that are found after a large ring, but only if it
    # is followed by a bigger ring and only if it is smaller than
    # points.for.reconnecting
    if(number.of.rings > 2){
        current.ring.before <- 1
        current.ring        <- 2
        while(current.ring < number.of.rings){
            length.of.ring.before <-
                sum(df.line.values$ring == current.ring.before)
            length.of.ring        <-
                sum(df.line.values$ring == current.ring)
            
            # Test whether the next ring is larger, if not, test whether it
            # is so small to be disregarded
            test.delete.ring <- FALSE
            length.of.ring.after <-
                sum(df.line.values$ring == (current.ring + 1))
            
            if(sum(df.line.values$ring == current.ring) <
               sum(df.line.values$ring == (current.ring + 1)) |
               length.of.ring.after / length.of.ring.before <
               difference.in.percent
            ){
                test.delete.ring <- TRUE
            }
            
            if(length.of.ring/length.of.ring.before<difference.in.percent &
               test.delete.ring & current.ring < (number.of.rings-1) &
               length.of.ring < points.for.reconnecting){
                df.line.values$ring[
                    df.line.values$ring == current.ring] <- 0
                
                #print(paste("Deleted ring ", current.ring, ".", sep=""))
                
                current.ring <- current.ring + 1
            }else{
                current.ring.before <- current.ring
                current.ring <- current.ring + 1
            }
        }
    }
    
    # Count rings
    df.line.values <- count.rings(df.line.values = df.line.values)
    number.of.rings <- max(df.line.values$ring)
    
    # NEU: Include all found rings to the image
    if(show.all.possible.rings == TRUE){
        hyline.points <- cbind(df.line.values$x[df.line.values$ring!=0],
                               df.line.values$y[df.line.values$ring!=0])
        image.information[
            cbind(hyline.points[,1], hyline.points[,2], 1)] <- 0.5
        image.information[
            cbind(hyline.points[,1], hyline.points[,2], 3)] <- 0.5
    }#Bis Hier
    
    # Delete rings that are too close to previous rings
    current.ring        <- 2
    current.ring.before <- 1
    ring.distances <-
        which(df.line.values$ring == current.ring.before)[1]
    #print(ring.distances)
    
    # Check ring distance from current ring to the previous ring
    while(current.ring <= number.of.rings){
        #print(paste("Current ring:", current.ring,". Ring before: ",
        #            current.ring.before, sep=""))
        
        mean.ring.distance <- mean(ring.distances)
        
        current.ring.lines <- which(df.line.values$ring == current.ring)
        #print(current.ring.lines)
        current.ring.before.lines <- 
            which(df.line.values$ring == current.ring.before)
        #print(current.ring.before.lines)
        distance.to.ring.before <- current.ring.lines[1] -
            current.ring.before.lines[length(current.ring.before.lines)]
        
        #print(paste("Distance to previous ring: ", distance.to.ring.before,
        #            sep=""))
        #print(paste("Mean distance: ", mean.ring.distance, sep=""))
        # Combine the rings if the distance to the ring before that
        # is too small.
        if((4.5 * distance.to.ring.before) < mean.ring.distance){
            if(length(current.ring.before.lines) >
               length(current.ring.lines)){
                df.line.values$ring[current.ring.before.lines] <-
                    current.ring
                df.line.values$ring[current.ring.lines] <- 0
                
            }else{
                
                df.line.values$ring[current.ring.lines] <-
                    current.ring
                df.line.values$ring[current.ring.before.lines] <- 0
                
            }
            #df.line.values$ring[
            #current.ring.before.lines[1]:
            #        current.ring.lines[length(current.ring.lines)]] <-
            #    current.ring
            
            #print("We deleted one of two rings.")
        }else{
            ring.distances <- c(ring.distances, distance.to.ring.before)
        }
        
        current.ring <- current.ring + 1
        current.ring.before <- current.ring.before + 1
    }
    
    # Count rings
    df.line.values <- count.rings(df.line.values = df.line.values)
    number.of.rings <- max(df.line.values$ring)
    
    # Enter the information into image.information -------------------------
    hyline.points <- cbind(df.line.values$x[df.line.values$ring!=0],
                           df.line.values$y[df.line.values$ring!=0])
    
    image.information[cbind(hyline.points[,1], hyline.points[,2], 3)] <- -1
    image.information[cbind(hyline.points[,1], hyline.points[,2], 2)] <- -1
    image.information[cbind(hyline.points[,1], hyline.points[,2], 1)] <- 1
        
    # If function is used by measure.otolith()
    return(list(image.information, df.line.values))
    
}
