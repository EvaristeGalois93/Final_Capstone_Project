#--------------------------------------------------
# R Server Code for the Capstone Project Shiny App
#--------------------------------------------------

suppressWarnings(library(tm))
suppressWarnings(library(stringr))
suppressWarnings(library(shiny))

# import n-grams data
# setwd("C:/Users/luca-/Desktop/Data_Science/Course10_CapstoneProject")

load("unigram.Rda")
load("bigram.Rda")
load("trigram.Rda")
load("quadrigram.Rda")
load("fivegram.Rda")

mesg = as.character(NULL)

# -------------------------------------------------
# Cleaning input sentence
# -------------------------------------------------
CleanInputString = function(inStr){
    
    # First remove useless characters
    inStr = iconv(inStr, "latin1", "ASCII", sub=" ");
    inStr = gsub("[^[:alpha:][:space:][:punct:]]", "", inStr);
    
    # Convert to a Corpus
    inStrCrps = VCorpus(VectorSource(inStr))
    
    # Clean-up
    inStrCrps = tm_map(inStrCrps, content_transformer(tolower))
    inStrCrps = tm_map(inStrCrps, removePunctuation)
    inStrCrps = tm_map(inStrCrps, removeNumbers)
    inStrCrps = tm_map(inStrCrps, stripWhitespace)
    inStr = as.character(inStrCrps[[1]])
    inStr = gsub("(^[[:space:]]+|[[:space:]]+$)", "", inStr)
    
    # Return the cleaned resulting senytense
    # If the resulting string is empty return empty and string.
    if (nchar(inStr) > 0) {
        return(inStr); 
    } else {
        return("");
    }
}

#---------------------------------------
# Prediction Algorithm
#---------------------------------------

PredNextTerm = function(input_string){
    
    # Global environment to make use of the N-grams data frames
    assign("mesg", "in PredNextWord", envir = .GlobalEnv)
    
    # It is important to clean up the input string as well
    input_string = CleanInputString(input_string)
    
    # Split the input string across white spaces and then extract the length
    input_string = unlist(strsplit(input_string, split=" "))
    inStrLen = length(input_string)
    
    nxtTermFound = FALSE
    predNxtTerm = as.character(NULL)
    
    # 1. First test the Five Gram using the five gram data frame
    if (inStrLen >= 4 & !nxtTermFound){
        
        # Assemble the terms of the input string separated by one white space each
        input_string1 = paste(input_string[(inStrLen-2):inStrLen], collapse=" ")
        
        # Subset the Four Gram data frame 
        searchStr = paste("^",input_string1, sep = "")
        DF5Temp = DF5[grep(searchStr, DF5$word), ]
        
        # Check to see if any matching record returned
        if ( length(DF5Temp[, 1]) > 1 ){
            
            predNxtTerm = DF5Temp[1,1]
            nxtTermFound = TRUE
            mesg = "Next word is predicted using 5-gram."
        }
        
        DF5Temp = NULL
    }
    
    # 2. Then test the Four Gram using the four gram data frame
    if (inStrLen >= 3 & !nxtTermFound){
        
        # Assemble the terms of the input string separated by one white space each
        input_string1 = paste(input_string[(inStrLen-2):inStrLen], collapse=" ")
        
        # Subset the Four Gram data frame 
        searchStr = paste("^",input_string1, sep = "")
        DF4Temp = DF4[grep(searchStr, DF4$word), ]
        
        # Check to see if any matching record returned
        if(length(DF4Temp[, 1]) > 1 ){
            
            predNxtTerm = DF4Temp[1,1]
            nxtTermFound = TRUE
            mesg = "Next word is predicted using 4-gram."
        }
        
        DF4Temp = NULL
        
    }
    
    # 3. Next test the Three Gram using the three gram data frame
    if (inStrLen >= 2 & !nxtTermFound){
        
        # Assemble the terms of the input string separated by one white space each
        input_string1 = paste(input_string[(inStrLen-1):inStrLen], collapse=" ")
        
        # Subset the Three Gram data frame 
        searchStr = paste("^",input_string1, sep = "")
        DF3Temp = DF3[grep (searchStr, DF3$word), ]
        
        # Check to see if any matching record returned
        if ( length(DF3Temp[, 1]) > 1 ){
            
            predNxtTerm = DF3Temp[1,1]
            nxtTermFound = TRUE
            mesg = "Next word is predicted using 3-gram."
            
        }
        
        DF3Temp = NULL
        
    }
    
    # 4. Next test the Two Gram using the three gram data frame
    if (inStrLen >= 1 & !nxtTermFound){
        
        # Assemble the terms of the input string separated by one white space each
        input_string1 = input_string[inStrLen]
        
        # Subset the Two Gram data frame 
        searchStr = paste("^",input_string1, sep = "")
        DF2Temp = DF2[grep(searchStr, DF2$word), ]
        
        # Check to see if any matching record returned
        if ( length(DF2Temp[, 1]) > 1 ){
            
            predNxtTerm = DF2Temp[1,1]
            nxtTermFound = TRUE
            mesg = "Next word is predicted using 2-gram."
            
        }
        
        DF2Temp = NULL
        
    }
    
    # 5. If no next term found in Four, Three and Two Grams return the most
    #    frequently used term from the One Gram using the one gram data frame
    if (!nxtTermFound & inStrLen > 0){
        
        predNxtTerm = DF1$word[1]
        mesg = "No next word found, the most frequent word is selected as next word."
        
    }
    
    nextTerm = word(predNxtTerm, -1)
    
    if (inStrLen > 0){
        DFTemp1 = data.frame(nextTerm, mesg)
        return(DFTemp1)
    } else {
        nextTerm = ""
        mesg = ""
        DFTemp1 = data.frame(nextTerm, mesg)
        return(DFTemp1)
    }
}

# Server script

msg = ""

shinyServer(function(input, output) {
    
    output$prediction = renderPrint({
        str2 = CleanInputString(input$inputString)
        strDF = PredNextTerm(str2)
        input$action
        msg = as.character(strDF[1,2])
        cat("", as.character(strDF[1,1]))
        cat("\n\t");
        cat("\n\t");
        cat("Note: ", as.character(strDF[1,2]));
    })
    
    output$text1 = renderText({
        paste("Input Sentence: ", input$inputString)})
    
    output$text2 = renderText({
        input$action
    })
}
)
