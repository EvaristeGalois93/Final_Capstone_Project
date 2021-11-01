#-------------------------------------------------
# It is important to clean-up the input string as well
#-------------------------------------------------
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
