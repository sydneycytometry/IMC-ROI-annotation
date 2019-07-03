# IMC ROI annotation script v0.1
    # Thomas Ashhurst
    # 2019-07-03
    # https://sydneycytometry.org.au


############################## START USER INPUT ##############################

### 1. Installing packages, loading packages, setting the working directory

    ## Install packages if required
        if(!require('flowCore')) {install.packages('flowCore')}
        if(!require('Biobase')) {install.packages('Biobase')}
        if(!require('data.table')) {install.packages('data.table')}

    ## Load packages
        library('flowCore')
        library('Biobase')
        library('data.table')

    ## Create Y-axis inversion function
        all.neg <- function(test) -1*abs(test)

    ## In order for this to work, a) rstudioapi must be installed and b) the location of this .r script must be in your desired working directory
        dirname(rstudioapi::getActiveDocumentContext()$path)            # Finds the directory where this script is located
        setwd(dirname(rstudioapi::getActiveDocumentContext()$path))     # Sets the working directory to where the script is located
        getwd()
        PrimaryDirectory <- getwd()
        PrimaryDirectory
    
    ## Use this to manually set the working directory
        #setwd("/Users/Tom/Desktop/Experiment")                          # Set your working directory here (e.g. "/Users/Tom/Desktop/") -- press tab when selected after the '/' to see options
        #getwd()                                                         # Check your working directory has changed correctly
        #PrimaryDirectory <- getwd()                                     # Assign the working directory as 'PrimaryDirectory'
        #PrimaryDirectory

### 2. Input data
    
    ## Use to list the .csv files in the working directory -- important, the only CSV files in the directory should be the one desired for analysis. If more than one are found, only the first file will be used
        FileNames <- list.files(path=PrimaryDirectory, pattern = ".csv")     # see a list of CSV files
        as.matrix(FileNames) # See file names in a list
        
    ## Read data from Files into list of data frames
        DataList=list() # Creates and empty list to start 
    
        for (File in FileNames) { # Loop to read files into the list
          tempdata <- fread(File, check.names = FALSE)
          File <- gsub(".csv", "", File)
          DataList[[File]] <- tempdata
        }
    
        rm(tempdata)
        AllSampleNames <- names(DataList)
    
    ## Chech data quality
       head(DataList)

    
    ## Review column names in dataset 
        colNam <- list() # creates an empty list
        for (i in c(1:length(DataList))){colNam[[i]] <- colnames(DataList[[1]])} # creates a table of all column names
        colNam <- data.frame(matrix(unlist(colNam), nrow=length(colNam), byrow=T)) # condenses table
        
        # Review column names
        colNam
        
        #################################################################################################
        ### It is important that all column names in each file are consistent for this script to work ###
        #################################################################################################
        
        
    ## What kind of files would you like to generate
        do.csv <- 1      # CSV files, yes = 1, no = 0
        do.fcs <- 1      # FCS files, yes = 1, no = 0
        
    
### 3. Invert Y-axis -- do you wish to create an inverted Y-axis -- coverts all the values to negative values, to correct the direction of the Y-axis

    ## Examine column names
        as.matrix(names(DataList[[1]]))
        
    ## Specify a demo sample
        num <- 1
        
    ## Specify the parameter that denotes the X-axis and Y-axis position
        xaxis <- "X_position"
        yaxis <- "Y_position"
        
    ## Plot X vs y for one sample
        demo.invert <- DataList[[num]]
        plot(demo.invert[[xaxis]], demo.invert[[yaxis]])
        
    ## Invert y-xais in test image
        d <- demo.invert[[yaxis]]
        d.res <- all.neg(d)
        demo.invert[["y-axis-invert"]] <- d.res
        
    ## Compare results
        plot(demo.invert[[xaxis]], demo.invert[[yaxis]])              # View uninverted data
        plot(demo.invert[[xaxis]], demo.invert[["y-axis-invert"]])    # View inverted data
        
    ## Do you wish to create an inverted version of the Y-aix (yes = 1, no = 0). This will no replace the y-axis values, but rather add a new column.
        do.invert <- 1
        
        
### 4. Investigate arcsinh transformation for data (if desired)

    ## Test a transform value on a demo parameter
        as.matrix(colNam)
        
        test.sampleNum <- 3    # Choose a sample for demonstration
        test.colNum1   <- 8    # Choose the first column for a demonstration transformation
        test.colNum2   <- 25   # Choose the second column for a demonstration transformation
        
    ## Create transformed versions of selected parameters
        transf.test <- data.frame(DataList[[test.sampleNum]][[test.colNum1]], DataList[[test.sampleNum]][[test.colNum2]])
        names(transf.test) <- c("One","Two")
        transf.test["One.transf"] <- transf.test["One"]
        transf.test["Two.transf"] <- transf.test["Two"]
        
    ## Plot the untransferred parameters
        plot(transf.test$One, transf.test$Two)
        
    ## Set and apply an demonstration arcsinh scale value (recommended 0.5 - 2)
        test.asinh.scale <- 2
  
        transf.test["One.transf"] <- asinh(transf.test[, "One"] / test.asinh.scale)
        transf.test["Two.transf"] <- asinh(transf.test[, "Two"] / test.asinh.scale)
      
    ## Plot untransformed and transformed data
        plot(transf.test$One, transf.test$Two)                    # Untransformed
        plot(transf.test$One.transf, transf.test$Two.transf)      # Transformed
        
        plot(transf.test$One, transf.test$One.transf)             # Untransformed vs transformed

    ## Choose columns to be transformed
        col.names.dl <- names(DataList[[1]])    # show data with headings
        as.matrix(col.names.dl)                 # view the column 'number' for each parameter
        
        col.nos.scale <- c(3:50)                # specify column numbers to be transformed - e.g. c(11, 23, 10)] 
        
        col.names.dl[col.nos.scale]             # Columns to transform
        col.names.dl[-col.nos.scale]            # Columns NOT to transform
        
    ## Choose the co-factor for arcsinh tranformation (recommended = 1)
        do.transform <- 1   
        asinh.scale <- 1
        

############################## END USER INPUT ##############################

### Create new output directory 
        
      x <- Sys.time()
      x <- gsub(":", "-", x)
      x <- gsub(" ", "_", x)
  
      newdir <- paste0("Output_IMC-ROI", "_", "transf=", do.transform, "_", "cf=", asinh.scale ,"yaxis-invert=", do.invert, "_", x)
      
      setwd(PrimaryDirectory)
      dir.create(paste0(newdir), showWarnings = FALSE)
      setwd(newdir)
    
### Sample loop
      
    for(i in c(1:length(AllSampleNames))){
      data_subset <- DataList[i]
      data_subset <- rbindlist(as.list(data_subset))
      data_subset <- as.data.frame(data_subset)
      dim(data_subset)
      a <- names(DataList)[i]

      ## Invert Y-axis
      if(do.invert == 1){
        invert <- data_subset[[yaxis]]
        invert.res <- all.neg(invert)
        data_subset[["y-axis-invert"]] <- invert.res
      }

      ## Perform transform (if selected in preferences, otherwise transformation will not run)
      if(do.transform == 1){
        
        col.names.dl <- names(data_subset)
        col.names.SCALE <- col.names.dl[col.nos.scale]
        data_subset[, col.names.SCALE] <- asinh(data_subset[, col.names.SCALE] / asinh.scale)
        head(data_subset)
        summary(data_subset)
      }
      
      if(do.csv == 1){
        fwrite(x = data_subset, file = paste0(a, ".csv"), row.names=FALSE)
      }

      if(do.fcs == 1){
        ## Metadata
        metadata <- data.frame(name=dimnames(data_subset)[[2]],desc=paste('column',dimnames(data_subset)[[2]]))
        
        ## Create FCS file metadata - ranges, min, and max settings
        #metadata$range <- apply(apply(data_subset,2,range),2,diff)
        metadata$minRange <- apply(data_subset,2,min)
        metadata$maxRange <- apply(data_subset,2,max)
        
        data_subset.ff <- new("flowFrame",exprs=as.matrix(data_subset), parameters=AnnotatedDataFrame(metadata)) # in order to create a flow frame, data needs to be read as matrix by exprs
        head(data_subset.ff)
        write.FCS(data_subset.ff, paste0(a, ".fcs"))
      }

    }

    