# author: yunzhe li
# date: Sep. 18

getfiles = function(x)
  # get rid of duplicate files
{
  setwd(x)
  if(getwd() == x)
  {
    raw_files = list.files(path = x, pattern = "*.pdf")
    file_string = lapply(raw_files, function(x) substr(x, 1, nchar(x) - 5))
    index = which(lapply(1:(length(file_string)-1), function(x) {file_string[x][[1]] == file_string[x+1][[1]]}) == TRUE) + 1
    files = raw_files[-index]
  }
  return(files)
}
files = getfiles("/home/rstudio/dsiPDFs")

getxmls = function(x)
  # get all legitiment xmls
{
  xmls = lapply(x, function(x) try(convertPDF2XML(x)))
  scannedPDF = lapply(xmls, function(x) try(isScanned(x)))
  useful_index = which(scannedPDF == FALSE)
  xmls = xmls[useful_index]
  return(xmls)
}
xmls = getxmls(files)

texts = lapply(xmls, function(x) getDocText(doc = x))
readPDFXML(files[[1]])
convertPDF2XML((files[[1]]))


e = getSectionText(files)
getSectionText
e$design
e$Study


## get data from google drive
library(googlesheets)
gs_ls()
sheet <- gs_title("forInterns2.csv")
data <- gs_read(ss = sheet2)
download_data = as.data.frame(data)

make_test_dictionary = function(x)
  # using downloaded data to make a dictionary of diagnostic tests
{
  x = strsplit(x, ",|;|:| and | or |\\)|\\(")
  x = unlist(x)
  x = gsub(x = x,pattern = 'test|tests|assay|microtest',replacement = '')
  x = trimws(x, "l")
  x = trimws(x, "r")
  x = x[which(x != "")]
  x = tolower(x)
  return(x)
}
tests = make_test_dictionary(download_data$most_specific_diagnostic_Test)

library(tm)
library(RWeka)

# First version that I made
# Runs slow and tedious
# 
# convertToMatrix = function(x)
#   # create a list of matrices
# {
#   x = gsub(pattern = "", replacement = " ", x)
#   myCorpus = Corpus(VectorSource(x))
#   dtm = DocumentTermMatrix(myCorpus, control = list(tokenize = AlphabeticTokenizer,
#                                                     removePunctuation = TRUE,
#                                                     wordLengths = c(1, Inf)))
#   df = as.data.frame(as.matrix(dtm))
#   return(df)
# }
# 
# df = lapply(tests, convertToMatrix)
# 
# makeMatricesSameLength = function(x)
# # making all matrices the same length to be easily calculated
# {
#   std = as.data.frame(matrix(rep(0,26), nrow = 1, byrow = TRUE))
#   colnames(std) = letters
#   new = as.matrix(std)
#   col = match(colnames(x), colnames(as.matrix(std)))
#   new[,col] <- new[,col] + x
#   mine = as.data.frame(new)
#   colnames(mine) = letters
#   return(mine)
# }
# 
# df_new = lapply(df, makeMatricesSameLength)
# 


convertToMatrix = function(x)
  # use tm package to split words and put into a matrix
{
  x = gsub(pattern = "", replacement = " ", x)
  myCorpus = Corpus(VectorSource(x))
  dtm = DocumentTermMatrix(myCorpus, control = list(tokenize = AlphabeticTokenizer,
                                                    removePunctuation = TRUE,
                                                    wordLengths = c(1, Inf)))
  df = as.data.frame(as.matrix(dtm))
  df = df[ ,order(names(df), decreasing = FALSE)]
  temp_x = convertToMatrix(paste(letters, sep = "", collapse = ""))
  temp_x[which(temp_x != 0)] = 0
  temp_x[sapply(colnames(df), function(x) grep(x, colnames(temp_x)))] = as.numeric(df)
  df = as.matrix(temp_x)
  return(df)
}

mt = convertToMatrix(tests)

# combine matrix with tests names for the convenience of checking
mt_names = cbind(mt, tests)
head(mt_names)

mag = function(x)
  # take magnitude of a matrix
{
  if(!is.vector(x))
    print("x is not a vector")
  else
  {
    y = x %*% x
    z = sqrt(y)
    return(z)
  }
}

cosine_similarity = function(x = as.matrix(x), y = as.matrix(x))
  # calculate cosine similarity of two vectors
{
  a = sum(x * y)
  b = mag(x) * mag(y)
  c = a / b
  return(c)
}

cosine_similarity(mt[43,], mt[713,])
e = grep("pcr", tests, value = TRUE)
unique(e)
e = grep("nrt-pcr", tests)
unique(e)
tests[43]
tests[713]
e = grep("elisa", tests, value = TRUE)
unique(e)
unique(tests)
table(tests)
# plaque-reduction neutralization vs plaque-reduction neutralizaton
# rt-pcr vs nrt-pcr

adist("elisa", "elise")
cosine_similarity(convertToMatrix("elisa"), convertToMatrix("elise"))

### note ###
# if I am going to use cosine similarity for this problem.
# then I need to make a matrix for every word. 
### end ###