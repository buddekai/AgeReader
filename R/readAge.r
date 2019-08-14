#' @title read.age
#' @description Main function for determining the age of a fish
#' @details Input should be tif-format.
#' The output will be written in the current directory.
#' Please be aware that the coordinate system is turned by 90° to the right.
#' The origin of the x- and y-axes is in the upper-left corner of the image.
#' (The x-axis points downwards and the y-axis to the right.)
#' @aliases readage age.read ageread
#' @author Kai Budde
#' @export read.age
#' @param input.dir A character
#' @param input.file A character
#' @param user.file.en A character
#' @param user.file.de A character
#' @param distance A number (Lines for finding the outline of the otolith
#' are this many pixels apart on the x- and y-axes.)
#' @param parameter.for.end A number (The edge is being found by calculating
#' a floating mean. The function marks an edge, if the next point differs
#' more than floating mean divided by parameter.for.end.)
#' @param parameter.distance.deviation A number (Outliers get detected by
#' multiplying the parameter with the mean distance between points.)
#' @param par.hyaline A number (This parameter is multiplied by the mean
#' of the brightness of the image to give a lower limit for ring detection.)
#' @param points.to.jump A number (It indicates how many points can be
#' missed while finding a large ring.)
#' @param points.for.reconnecting A number (It indicates how far rings
#' should be away from each other. Otherwise they are being merged.)
#' @param remove.points.at.beginning A number (It indicates how far the
#' first ring should be away from the starting (midpoint).)
#' @param remove.points.at.end A number (It indicates how many points should
#' be disregarded at the end of the line.)
#' @param t A number (This parameter is being multiplied by the standard
#' deviation in order to delete outliers in the line values.)
#' @param difference.in.percent A number (Rings that are smaller than this
#' parameter in comparison to the previous and next ring are being deleted.)
#' @param outer.zone.pixel A number (It defines how many pixels at the
#' end of the line belong to the outer zone.)
#' @param show.all.possible.rings A boolean (If it is true, than all
#' possible rings are being maked blue.)
#' @examples
#' \dontrun{
#' read.age(input.file = "example.tif")
#' }

read.age <- function(input.dir = NULL,
                     input.file = NULL,
                     user.file.en = NULL,
                     user.file.de = NULL,
                     distance = 20,
                     parameter.for.end = NULL,
                     parameter.distance.deviation = 2,
                     par.hyaline = 1.1,
                     points.to.jump = 10,
                     points.for.reconnecting = 20,
                     remove.points.at.end = 25,
                     remove.points.at.beginning = 0,
                     t = 2,
                     difference.in.percent = 0.25,
                     outer.zone.pixel = 50,
                     show.all.possible.rings = FALSE,
                     distance.second.lines = 50,
                     points.to.look.for.midpoint = 50){
    

    # Basics ---------------------------------------------------------------
    .old.options <- options()
    on.exit(options(.old.options))
    options(stringsAsFactors = FALSE, warn=-1)
    
    # -------------------------------------------------------------------- #
    #                         1. Initialization                            #
    # -------------------------------------------------------------------- #
    
    # Data input -----------------------------------------------------------
    
    # Input directory or input file - either one of them must be
    # submitted. If not: close function call.
    if(is.null(input.dir) & is.null(input.file)){
        print(paste("Please call function with either an input directory ",
                    "which contains images of otoliths or with a single ",
                    "image file.", sep=""))
        return()
    }
    
    # User file (either german (de) or english (en) version) - only one of
    # them must be submitted.
    if(is.null(user.file.en) & is.null(user.file.de)){
        language.mode <- "NA"
        # Make data.frame from scratch
        df.user.file <- data.frame(pic_name = NA)
        user.file.from.scratch <- TRUE
    }
    # If both are submitted, close function call
    if(!is.null(user.file.en) & !is.null(user.file.de)){
        print("Please call function with only one user file.")
        return()
    }
    
    
    # If German version is entered. (";" separate cells,
    # use "," for decimals)
    if(!is.null(user.file.de) & is.null(user.file.en)){
        df.user.file <- utils::read.table(
            file = user.file.de, sep = ";", dec = ",", header = TRUE)
        language.mode <- "de"
        user.file.from.scratch <- FALSE
        
        # Remove empty lines
        df.user.file <- df.user.file[!df.user.file$pic_name=="",]
        df.user.file <- df.user.file[!df.user.file$pic_name=="NA",]
        df.user.file <- df.user.file[!is.na(df.user.file$pic_name),]
    }
    # If English version is entered. ("," separate cells,
    # use "." for decimals)
    if(is.null(user.file.de) & !is.null(user.file.en)){
        df.user.file <- utils::read.table(
            file = user.file.de, sep = ",", dec = ".", header = TRUE)
        language.mode <- "en"
        user.file.from.scratch <- FALSE
        
        # remove empty lines
        df.user.file <- df.user.file[!df.user.file$pic_name=="",]
        df.user.file <- df.user.file[!df.user.file$pic_name=="NA",]
        df.user.file <- df.user.file[!is.na(df.user.file$pic_name),]
    }
    
    # Data output ----------------------------------------------------------
    
    # Make a new subdirectory inside the input directory
    if(is.null(input.file) & !is.null(input.dir)){
        
        if(grepl("\\\\", input.dir)){
            input.dir  <- gsub("\\$", "", input.dir)
            output.dir <- paste(input.dir, "\\output\\", sep="")
        }else{
            input.dir  <- gsub("/$", "", input.dir)
            output.dir <- paste(input.dir, "/output/", sep="")
        }
        
    }
    
    # Make a new subdirectory where the file is from
    if(!is.null(input.file) & is.null(input.dir)){
        
        # If test file from package is used
        if(input.file == "example.tif"){
            example.image <- TRUE
            input.file <- system.file("extdata", "example.tif",
                                      package = "AgeReader")
            output.dir <- getwd()
            output.dir <- paste(output.dir, "/output/", sep="")
            
        # Else
        }else{
            example.image <- FALSE
            # If the path is written as "dir\\file.tif"
            if(grepl("\\\\", input.file)){
                output.dir <- gsub("(.*)\\\\.*\\.tif+", "\\1", input.file)
                output.dir <- paste(output.dir, "\\output\\", sep="")
                # If the path is written as "dir/file.tif"
            }else{
                output.dir <- gsub("(.*)/.*\\.tif+", "\\1", input.file)
                output.dir <- paste(output.dir, "/output/", sep="")
            }
        }
    }
    
    # Create the subdirectory
    dir.create(output.dir, showWarnings = FALSE)
    
    # Adapt user file data.frame -------------------------------------------
    df.user.file <- add.col.to.user.file(df.user.file)
    
    # Save the file names (tifs) ----------------------------------
    if(is.null(input.dir)){
        if(grepl("\\\\", input.file)){
            file.names <- gsub(".*\\\\(.*\\.tif+)", "\\1", input.file)
            # If the path is written as "dir/file.tif"
            }else{
                file.names <- gsub(".*/(.*\\.tif+)", "\\1", input.file)
                }
        }else{
            file.names <- list.files(path = input.dir)
        }
    
    file.names.tif <- file.names[grepl("tif", file.names)]
    #file.names.czi <- file.names[grepl("czi", file.names)]
    rm(file.names)
    
    
    
    # -------------------------------------------------------------------- #
    #               2. Reading and Editing every picture                   #
    # -------------------------------------------------------------------- #
    
    
    # i = 1 .. n(images) Go through all images (tifs) ----------------------
    for(i in 1:length(file.names.tif)){
        
        print(paste("Dealing with ", file.names.tif[i], ". (It is now ",
                    Sys.time(), ".)", sep=""))
        
        ## Save the row number of data.frame which contains the current ----
        ## image.
        
        # Name of the image without the ending
        image.name <- gsub("(.*)\\.tif*", "\\1", file.names.tif[i])
        
        # Path of the current image to work with
        image.path <- ifelse(
            test = is.null(input.dir),
            yes = input.file[i],
            no = paste(input.dir, file.names.tif[i], sep="\\"))
        
        current.row.number <- which(df.user.file$pic_name %in% image.name)
        
        # There is no image with that name
        if(length(current.row.number) == 0){
            # append row to user file data.frame
            df.user.file <- rbind(df.user.file, NA)
            df.user.file$pic_name[nrow(df.user.file)] <- image.name
            current.row.number <-
                which(df.user.file$pic_name %in% image.name)
        }
        # There is more than one image with that name
        if(length(current.row.number) > 1){
            print(paste("Please clean your user file. Image names are ",
                        "not unique.", sep=""))
            return()
        }
        
        
        ## Mark the current line as being processed.
        #df.user.file$PROCESSED[current.row.number] <- "Yes"
        
        ## Find scale of image
        ## Only try to look for the czi-file if there are any.
        #if(length(file.names.czi) > 0){
        #    
        #    image.name.czi <- paste(image.name, ".czi", sep="")
        #    
        #    # Check whether the czi-file of the current image exists
        #    if(sum(grepl(image.name.czi, file.names.czi))==1){
        #        
        #        # The czi-file exists and will be for scale of image
        #        scale <- get.scale(
        #            file.name = paste(input.dir, image.name.czi, sep="\\"))
        #        if(is.na(scale)){
        #            scale <- 1 / df.user.file$SCALE[current.row.number]
        #            print(paste("czi-file is corrupted.",
        #                        " Could not read the scale.", sep=""))
        #        }
        #        
        #        df.user.file$SCALE_mperpix[current.row.number] <- scale
        #        
        #    }else{
        #        print(paste("File ", image.name.czi, " does not exist ",
        #                    "or it is not clearly named.", sep=""))
        #    }
        #    
        #}
        
        # Read in image and save different version for ring detection ------
        
        image <- tiff::readTIFF(source = image.path, info = FALSE)
        
        # Save an empty array which will be filled with information of
        # lines etc.
        
        image.information <- array(data = 0, dim = dim(image))
        
        # Save two different grey versions of the image
        image.grey.outline  <-
            edit.image(image = image, grey.mode = "normal.grey")
        image.grey.ring     <-
            edit.image(image = image, grey.mode = "red.grey")
        
        # Find Edge --------------------------------------------------------
        # Save the edge of the otolith in image.information.

        function.results <- detect.outline(
            image.grey = image.grey.outline,
            image.information = image.information,
            distance = distance,
            parameter.for.end = parameter.for.end,
            parameter.distance.deviation = parameter.distance.deviation)
        
        image.information <- function.results[[1]]
        #image.border = c(top.y, right.x, bottom.y, left.x)
        image.border      <- function.results[[2]]
        left.point        <- function.results[[3]]
        right.point       <- function.results[[4]]
        
        
        # Save x- and y-values of the borders in the data.frame
        # df.user.file. 
        
        #df.user.file$top_border[current.row.number]   <- image.border[1]
        #df.user.file$right_border[current.row.number] <- image.border[2]
        #df.user.file$lower_border[current.row.number] <- image.border[3]
        #df.user.file$left_border[current.row.number]  <- image.border[4]
        #
        #df.user.file$left_point_x[current.row.number]  <- left.point[2]
        #df.user.file$left_point_y[current.row.number]  <- left.point[1]
        #df.user.file$right_point_x[current.row.number] <- right.point[2]
        #df.user.file$right_point_y[current.row.number] <- right.point[1]
        
        rm(function.results)
        
        
        # Find hyaline rings -----------------------------------------------
        
        # First pair of lines ----------------------------------------------
        # Calculate the line indices
        #print("first lines")
        # left line
        # TODO: Call first line: left line and second line right line
        first.line <- getLineIndices(start.x = left.point[1],
                                     start.y = left.point[2],
                                     end.x = image.border[2], # right.x
                                     end.y = image.border[1]) # top.y
        
        # right line
        second.line <- getLineIndices(start.x = right.point[1],
                                      start.y = right.point[2],
                                      end.x = image.border[4], # left.x
                                      end.y = image.border[1]) # top.y
        
        lines <- rbind(first.line, second.line)
        #print(lines)
        
        image.information[cbind(lines[,2], lines[,1], 3)] <- -1
        image.information[cbind(lines[,2], lines[,1], 2)] <- 1
        image.information[cbind(lines[,2], lines[,1], 1)] <- -1
        
        midpoint <- lines[which(duplicated(lines))[1],]
        
        # if the lines do not cross each other exactly
        if(all(is.na(midpoint))){
            # move second line one pixel up
            second.line[,2] <- second.line[,2] - 1
            
            lines <- rbind(first.line, second.line)
            
            midpoint.left <- lines[which(duplicated(lines))[1],]
            if(all(is.na(midpoint.left))){
                print("Something went wrong.")
            }
            midpoint.right <- midpoint.left
            midpoint.right[2] <- midpoint.right[2] + 1

            second.line[,2] <- second.line[,2] + 1
        }else{
            midpoint.left <- midpoint
            midpoint.right <- midpoint
        }
        
        
        # TODO: Find a better midpoint -> darker one? ----------------------
        
        frame.center <- c(0,0)
        frame.center[1] <- (image.border[1] + image.border[3])%/%2
        frame.center[2] <- (image.border[2] + image.border[4])%/%2
        
        for(i in 1:3){
            if(i == 1){
                
                # midpoint is (y,x) and must be reversed
                coordinates.of.midpoint <- c(0,0)
                coordinates.of.midpoint[1] <- (frame.center[1] + midpoint.left[2]) %/% 2
                coordinates.of.midpoint[2] <- (frame.center[2] + midpoint.left[1]) %/% 2
                
                coordinates.of.midpoint <-
                    find.midpoint(image = image,
                                  image.grey = image.grey.outline,
                                  center.point = coordinates.of.midpoint,
                                  number.of.blocks.in.row = 4,
                                  search.length = 160,
                                  image.path)
                
            }else{
                coordinates.of.midpoint <-
                    find.midpoint(image = image,
                                  image.grey = image.grey.outline,
                                  center.point = coordinates.of.midpoint,
                                  number.of.blocks.in.row = 4,
                                  search.length = (160/(6**(i-1))),
                                  image.path)
            }
            print(paste("Coordinates of midpoint: ",
                        paste(coordinates.of.midpoint, collapse = " ")))

        }
        
            
        
        #left line
        # index of midpoint
        #index.midpoint <- which(first.line==midpoint.left)[1]
        #coordinates <- first.line[(index.midpoint-points.to.look.for.midpoint):(index.midpoint+points.to.look.for.midpoint),]
        #coordinates2 <- coordinates[,c(2,1)]
        
        #grey.values.along <- image.grey.ring[coordinates2]
                                             
        #new.mid.point.left <-coordinates[which(image.grey.ring[coordinates2] == min(grey.values.along) ),]
        
        #right line
        
        
        # Go through lines and find hyaline (bright) ring structures.
        
        # Left line
        #print("Left Line")
        if(midpoint.left != left.point){
            function.results <- detect.rings(
                image.grey = image.grey.ring,
                image.information = image.information,
                first.point = midpoint.left,
                second.point = left.point,
                line.to.follow = first.line,
                par.hyaline = par.hyaline,
                points.to.jump = points.to.jump,
                points.for.reconnecting = points.for.reconnecting,
                remove.points.at.beginning = remove.points.at.beginning,
                remove.points.at.end = remove.points.at.end,
                t = t,
                difference.in.percent = difference.in.percent,
                show.all.possible.rings)
            
            image.information <- function.results[[1]]
            df.line.values.left <- function.results[[2]]
            
            rm(function.results)
        }
        
        
        # Right line
        #print("Right Line")
        if(midpoint.right != right.point){
            function.results <- detect.rings(
                image.grey = image.grey.ring,
                image.information = image.information,
                first.point = midpoint.right,
                second.point = right.point,
                line.to.follow = second.line,
                par.hyaline = par.hyaline,
                points.to.jump = points.to.jump,
                points.for.reconnecting = points.for.reconnecting,
                remove.points.at.beginning = remove.points.at.beginning,
                remove.points.at.end = remove.points.at.end,
                t = t,
                difference.in.percent = difference.in.percent,
                show.all.possible.rings)
            
            image.information <- function.results[[1]]
            df.line.values.right <- function.results[[2]]
            
            rm(function.results)
        }
        
        # # Second pair of lines (above) -------------------------------------
        # 
        # # Calculate the line indices
        # # left line
        # print("second lines")
        # 
        # first.line <- getLineIndices(start.x = left.point[1],
        #                              start.y = left.point[2] - distance.second.lines,
        #                              end.x = image.border[2], # right.x
        #                              end.y = image.border[1]) # top.y
        # 
        # # right line
        # second.line <- getLineIndices(start.x = right.point[1],
        #                               start.y = right.point[2]- distance.second.lines,
        #                               end.x = image.border[4], # left.x
        #                               end.y = image.border[1]) # top.y
        # 
        # lines <- rbind(first.line, second.line)
        # #print(lines)
        # 
        # image.information[cbind(lines[,2], lines[,1], 3)] <- -1
        # image.information[cbind(lines[,2], lines[,1], 2)] <- 1
        # image.information[cbind(lines[,2], lines[,1], 1)] <- -1
        # 
        # midpoint <- lines[which(duplicated(lines))[1],]
        # 
        # # if the lines do not cross each other exactly
        # if(all(is.na(midpoint))){
        #     # move second line one pixel up
        #     second.line[,2] <- second.line[,2] - 1
        #     
        #     lines <- rbind(first.line, second.line)
        #     
        #     midpoint.left <- lines[which(duplicated(lines))[1],]
        #     if(all(is.na(midpoint.left))){
        #         print("Something went wrong.")
        #     }
        #     midpoint.right <- midpoint.left
        #     midpoint.right[2] <- midpoint.right[2] + 1
        #     
        #     second.line[,2] <- second.line[,2] + 1
        # }else{
        #     midpoint.left <- midpoint
        #     midpoint.right <- midpoint
        # }
        # 
        # # Go through lines and find hyaline (bright) ring structures.
        # 
        # # Left line
        # #print("Left Line")
        # if(midpoint.left != left.point){
        #     function.results <- detect.rings(
        #         image.grey = image.grey.ring,
        #         image.information = image.information,
        #         first.point = midpoint.left,
        #         second.point = left.point,
        #         line.to.follow = first.line,
        #         par.hyaline = par.hyaline,
        #         points.to.jump = points.to.jump,
        #         points.for.reconnecting = points.for.reconnecting,
        #         remove.points.at.beginning = remove.points.at.beginning,
        #         remove.points.at.end = remove.points.at.end,
        #         t = t,
        #         difference.in.percent = difference.in.percent,
        #         show.all.possible.rings)
        #     
        #     image.information <- function.results[[1]]
        #     df.line.values.left <- function.results[[2]]
        #     
        #     rm(function.results)
        # }
        # 
        # 
        # # Right line
        # #print("Right Line")
        # if(midpoint.right != right.point){
        #     function.results <- detect.rings(
        #         image.grey = image.grey.ring,
        #         image.information = image.information,
        #         first.point = midpoint.right,
        #         second.point = right.point,
        #         line.to.follow = second.line,
        #         par.hyaline = par.hyaline,
        #         points.to.jump = points.to.jump,
        #         points.for.reconnecting = points.for.reconnecting,
        #         remove.points.at.beginning = remove.points.at.beginning,
        #         remove.points.at.end = remove.points.at.end,
        #         t = t,
        #         difference.in.percent = difference.in.percent,
        #         show.all.possible.rings)
        #     
        #     image.information <- function.results[[1]]
        #     df.line.values.right <- function.results[[2]]
        #     
        #     rm(function.results)
        # }
        # 
        # # Thrid pair of lines (below) --------------------------------------
        # 
        # # Calculate the line indices
        # # left line
        # print("second lines")
        # 
        # first.line <- getLineIndices(start.x = left.point[1],
        #                              start.y = left.point[2] + distance.second.lines,
        #                              end.x = image.border[2], # right.x
        #                              end.y = image.border[1]) # top.y
        # 
        # # right line
        # second.line <- getLineIndices(start.x = right.point[1],
        #                               start.y = right.point[2] + distance.second.lines,
        #                               end.x = image.border[4], # left.x
        #                               end.y = image.border[1]) # top.y
        # 
        # lines <- rbind(first.line, second.line)
        # #print(lines)
        # 
        # image.information[cbind(lines[,2], lines[,1], 3)] <- -1
        # image.information[cbind(lines[,2], lines[,1], 2)] <- 1
        # image.information[cbind(lines[,2], lines[,1], 1)] <- -1
        # 
        # midpoint <- lines[which(duplicated(lines))[1],]
        # 
        # # if the lines do not cross each other exactly
        # if(all(is.na(midpoint))){
        #     # move second line one pixel up
        #     second.line[,2] <- second.line[,2] - 1
        #     
        #     lines <- rbind(first.line, second.line)
        #     
        #     midpoint.left <- lines[which(duplicated(lines))[1],]
        #     if(all(is.na(midpoint.left))){
        #         print("Something went wrong.")
        #     }
        #     midpoint.right <- midpoint.left
        #     midpoint.right[2] <- midpoint.right[2] + 1
        #     
        #     second.line[,2] <- second.line[,2] + 1
        # }else{
        #     midpoint.left <- midpoint
        #     midpoint.right <- midpoint
        # }
        # 
        # # Go through lines and find hyaline (bright) ring structures.
        # 
        # # Left line
        # #print("Left Line")
        # if(midpoint.left != left.point){
        #     function.results <- detect.rings(
        #         image.grey = image.grey.ring,
        #         image.information = image.information,
        #         first.point = midpoint.left,
        #         second.point = left.point,
        #         line.to.follow = first.line,
        #         par.hyaline = par.hyaline,
        #         points.to.jump = points.to.jump,
        #         points.for.reconnecting = points.for.reconnecting,
        #         remove.points.at.beginning = remove.points.at.beginning,
        #         remove.points.at.end = remove.points.at.end,
        #         t = t,
        #         difference.in.percent = difference.in.percent,
        #         show.all.possible.rings)
        #     
        #     image.information <- function.results[[1]]
        #     df.line.values.left <- function.results[[2]]
        #     
        #     rm(function.results)
        # }
        # 
        # 
        # # Right line
        # #print("Right Line")
        # if(midpoint.right != right.point){
        #     function.results <- detect.rings(
        #         image.grey = image.grey.ring,
        #         image.information = image.information,
        #         first.point = midpoint.right,
        #         second.point = right.point,
        #         line.to.follow = second.line,
        #         par.hyaline = par.hyaline,
        #         points.to.jump = points.to.jump,
        #         points.for.reconnecting = points.for.reconnecting,
        #         remove.points.at.beginning = remove.points.at.beginning,
        #         remove.points.at.end = remove.points.at.end,
        #         t = t,
        #         difference.in.percent = difference.in.percent,
        #         show.all.possible.rings)
        #     
        #     image.information <- function.results[[1]]
        #     df.line.values.right <- function.results[[2]]
        #     
        #     rm(function.results)
        # }
        
        
        # Add information to user.file data.frame --------------------------
        
        # Ring number
        if(midpoint.left != left.point){
            ring.number.left <- max(df.line.values.left$ring)
        }else{
            ring.number.left <- 0
        }
        
        if(midpoint.right != right.point){
            ring.number.right <- max(df.line.values.right$ring)
        }else{
            ring.number.right <- 0
        }
        
        
        df.user.file$number_of_rings_left[current.row.number]  <-
            ring.number.left
        df.user.file$number_of_rings_right[current.row.number] <-
            ring.number.right
        
        #df.user.file$equal_ring_number[current.row.number] <-
        #    ifelse(ring.number.left == ring.number.right,
        #           "yes", "no")
        
        # take the minimum ring number for further calculations
        number.of.rings <- min(c(ring.number.left, ring.number.right))
        
        # READABILITY (0: identical number of rings, 4: number of rings
        # are only slightly different on each side, 8: unreadable)
        if(ring.number.left == ring.number.right &&
           ring.number.left != 0){
            df.user.file$readability[current.row.number] <- 0
        }else if(abs(ring.number.left - ring.number.right) == 1){
            df.user.file$readability[current.row.number] <- 4
        }else{
            df.user.file$readability[current.row.number] <- 8
        }
        
        # Add the OUTER_ZONE (depending how far the last hyaline ring is
        # away from the edge)
        if(number.of.rings > 0){
            number.of.points.after.last.ring.left <-
                nrow(df.line.values.left) - 
                which(df.line.values.left$ring==ring.number.left)[
                    sum(df.line.values.left$ring==ring.number.left)
                    ]
            
            number.of.points.after.last.ring.right <-
                nrow(df.line.values.right) - 
                which(df.line.values.right$ring==ring.number.right)[
                    sum(df.line.values.right$ring==ring.number.right)
                    ]
            
            if(ring.number.left == ring.number.right){
                if(number.of.points.after.last.ring.left <
                   outer.zone.pixel |
                   number.of.points.after.last.ring.right <
                   outer.zone.pixel){
                    df.user.file$outer_zone[current.row.number] <- "H"
                }else{
                    df.user.file$outer_zone[current.row.number] <- "O"
                }
            }else{
                if(number.of.points.after.last.ring.left >
                   outer.zone.pixel &
                   number.of.points.after.last.ring.right >
                   outer.zone.pixel){
                    df.user.file$outer_zone[current.row.number] <- "O"
                }else{
                    df.user.file$outer_zone[current.row.number] <- "H"
                }
            }
            
            # Add age and cohort of the fish only for those otoliths
            # where the rings could be detected in a good way.
            subtract.year <- 0
            if(df.user.file$readability[current.row.number] %in% c(0,4)){
                if("catch_date" %in% names(df.user.file)){
                    catch.date <-
                        df.user.file$catch_date[current.row.number]
                    catch.month <- as.integer(substr(catch.date, 3,4))
                    catch.year <- as.integer(substr(catch.date, 5,6))
                    
                    
                    # Only count the last hyaline ring at the edge if the
                    # fish was caught between Jan. 1 and June 30.
                    if(df.user.file$outer_zone[current.row.number] == "H" &
                       catch.month %in% c(7:12) &
                       ring.number.left == ring.number.right){
                        subtract.year <- 1
                    }
                    
                    df.user.file$age[current.row.number]    <-
                        number.of.rings - subtract.year
                    df.user.file$cohort[current.row.number] <-
                        catch.year - number.of.rings + subtract.year
                }else{
                    # If no catch date is being given
                    df.user.file$age[current.row.number]    <-
                        number.of.rings
                    }
                }else{
                    # If the difference of both sides is exactly 2, take
                    # the higher number as the age.
                    if("catch_date" %in% names(df.user.file)){
                        catch.date <-
                            df.user.file$catch_date[current.row.number]
                        catch.month <- as.integer(substr(catch.date, 3,4))
                        catch.year <- as.integer(substr(catch.date, 5,6))
                        
                        if(abs(ring.number.left - ring.number.right) == 2){
                            if(catch.month %in% c(7:12)){
                                subtract.year <- 1
                            }
                            
                            number.of.rings <- max(ring.number.left,
                                                   ring.number.right)
                            df.user.file$age[current.row.number]    <-
                                number.of.rings - subtract.year
                            df.user.file$cohort[current.row.number] <-
                                catch.year - number.of.rings + subtract.year
                        }
                    }
                }
            
            
            # Add diameter of first three hyaline and opaque rings
            consider.rings <- ifelse(number.of.rings > 3, 3,
                                     number.of.rings)
            #if(consider.rings > 0 &
            #   !is.na(df.user.file$SCALE_mperpix[current.row.number])){
            #    for(i in 1:consider.rings){
            #        # opaque ring
            #        dia.o.x1 <-
            #            df.line.values.left$x[
            #                df.line.values.left$ring==i][1]
            #        dia.o.y1 <-
            #            df.line.values.left$y[
            #                df.line.values.left$ring==i][1]
            #        dia.o.x2 <-
            #            df.line.values.right$x[
            #                df.line.values.right$ring==i][1]
            #        dia.o.y2 <-
            #            df.line.values.right$y[
            #                df.line.values.right$ring==i][1]
            #        
            #        dia.o <-
            #            (dia.o.x2 - dia.o.x1)^2 + (dia.o.y2 - dia.o.y1)^2
            #        dia.o <- sqrt(dia.o)
            #        
            #        if(!is.na(df.user.file$SCALE_mperpix[
            #            current.row.number])){
            #            dia.o <- df.user.file$SCALE_mperpix[
            #                current.row.number]*
            #                dia.o
            #            col <- paste("diameter_opaque_ring_", i, sep="")
            #            df.user.file[[col]][current.row.number] <- dia.o
            #        }
                    
                    
            #        # hyaline ring
            #        dia.h.x1 <-
            #            df.line.values.left$x[df.line.values.left$ring==i][
            #                sum(df.line.values.left$ring==i)]
            #        dia.h.y1 <-
            #            df.line.values.left$y[df.line.values.left$ring==i][
            #                sum(df.line.values.left$ring==i)]
            #        dia.h.x2 <-
            #            df.line.values.right$x[
            #                df.line.values.right$ring==i][
            #                    sum(df.line.values.right$ring==i)]
            #        dia.h.y2 <-
            #            df.line.values.right$y[
            #                df.line.values.right$ring==i][
            #                    sum(df.line.values.right$ring==i)]
            #        
            #        dia.h <-
            #            (dia.h.x2 - dia.h.x1)^2 + (dia.h.y2 - dia.h.y1)^2
            #        dia.h <- sqrt(dia.h)
            #        
            #        if(!is.na(df.user.file$SCALE_mperpix[
            #            current.row.number])){
            #            dia.h <-
            #                df.user.file$SCALE_mperpix[current.row.number]*
            #                dia.h
            #            col <- paste("diameter_hyaline_ring_", i, sep="")
            #            df.user.file[[col]][current.row.number] <- dia.h
            #        }
            #    }
            #}
            
            
            ## Add the total diameter of the otolith
            #total.diameter <- sqrt(sum((left.point - right.point)^2))
            #total.diameter <- df.user.file$SCALE_mperpix[
            #    current.row.number] * total.diameter
            #df.user.file$otolith_diameter[current.row.number] <-
            #    total.diameter
        }
        
        
        
        
        # Save the image with all image information ------------------------
        image.information <- image + image.information
        
        image.information <-
            ifelse(image.information < 0, 0, image.information)
        image.information <-
            ifelse(image.information > 1, 1, image.information)
        
        if(is.null(input.dir)){
            if(example.image == TRUE){
                save.file.name <- paste(output.dir,"example_done.tif", sep="")
                print(paste("Example saved in ", save.file.name, ".",
                            sep=""))
            }else{
                save.file.name <- paste(output.dir, image.name, "_row_",
                                        (current.row.number + 1),
                                        "_in_userfile_done.tif", sep="")
            }
        }else{
            save.file.name <- paste(output.dir,
                                    df.user.file$readability[
                                        current.row.number],
                                    "_",
                                    image.name,
                                    ".tif", sep="")
            #save.file.name <- paste(output.dir,
            #                        df.user.file$readability[
            #                            current.row.number],
            #                        image.name, "_row_",
            #                        (current.row.number + 1),
            #                        "_in_userfile_done.tif", sep="")
        }
        
        tiff::writeTIFF(what = image.information,
                        where = save.file.name,
                        bits.per.sample = 8L, compression = "none",
                        reduce = TRUE)
        
    }
    
    # -------------------------------------------------------------------- #
    #   3. Save the extended table with all data obtained from the images  #
    # -------------------------------------------------------------------- #
    
    
    # Save the customized user file ----------------------------------------
    # Delete first row if we have build the user file from scratch.
    if(user.file.from.scratch == TRUE){
        df.user.file <- df.user.file[-1,]
    }
    
    if(language.mode == "de"){
        user.file.output <- paste(
            output.dir,
            gsub(".+\\\\(.+).csv", "\\1", user.file.de),
            "results.csv",
            sep="")
        
        #user.file.name <- paste(gsub(".csv","",user.file.de),
        #                        "_extended.csv", sep="")
        
        utils::write.table(x = df.user.file, file = user.file.output,
                           sep = ";", dec = ",", row.names = FALSE)
    }else{
        if(language.mode == "en"){
            user.file.output <- paste(
                output.dir,
                gsub(".+\\\\(.+).csv", "\\1", user.file.en),
                "results.csv",
                sep="")
            
            #user.file.name <- paste(gsub(".csv","",user.file.en),
            #                        "_extended.csv", sep="")
            
            utils::write.table(x = df.user.file, file = user.file.output,
                               sep = ",", dec = ".", row.names = FALSE)
        }else{
            # There was no data sheet input
            user.file.output <- paste(
                output.dir,
                "results.csv",
                sep="")
            
            utils::write.table(x = df.user.file, file = user.file.output,
                               sep = ";", dec = ",", row.names = FALSE)
        }
    }
    
    
    print("Done!")
    
}
