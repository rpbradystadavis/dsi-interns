# author: yunzhe li
# date: Sep. 18

library(ReadPDF)
library(XML)

getfiles = function(x)
  # get rid of duplicate files
{
  setwd("/home")
  setwd(x)
  raw_files = list.files(path = x, pattern = "*.pdf")
  file_string = lapply(raw_files, function(x) substr(x, 1, nchar(x) - 5))
  index = which(lapply(1:(length(file_string)-1), function(x)
    {file_string[x][[1]] == file_string[x+1][[1]]}) == TRUE) + 1
  files = raw_files[-index]
  return(files)
}
files = getfiles("/home/yunzheli/DSI-ReadPDF/subsetPapers2/pdf/")

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

## get data from google drive
## could also download the .csv files manually
library(googlesheets)
gs_ls()
sheet1 = gs_title("forInterns.csv")
data1 = gs_read(ss = sheet1)
sheet2 <- gs_title("forInterns2.csv")
data2 <- gs_read(ss = sheet2)
download_data1 = as.data.frame(data1)
download_data2 = as.data.frame(data2)
# one as training set, another as testing set.


# method 1: use text mining methods to clean the dictionary first.
make_test_dictionary = function(x)
  # using downloaded data to make a dictionary of diagnostic tests
{
  x = strsplit(x, ",|;|:| and | or |\\)|\\(")
  x = unlist(x)
  x = gsub(x = x,pattern = 'test|tests|assay|microtest',replacement = '')
  # x = trimws(x, "l")
  # x = trimws(x, "r")
  x = x[which(x != "")]
  # x = tolower(x)
  return(x)
}
tests = make_test_dictionary(download_data1$most_specific_diagnostic_Test)
# sort(unique(tests))


library(NLP)
library(tm)
library(RWeka)

convertToMatrix = function(x)
  # use tm package to split words and put into a matrix
{
  x = c(paste(letters, collapse = ""),x)
  x = gsub(pattern = "", replacement = " ", x)
  myCorpus = Corpus(VectorSource(x))
  dtm = DocumentTermMatrix(myCorpus, control = list(tokenize = AlphabeticTokenizer,
                                                    removePunctuation = TRUE,
                                                    wordLengths = c(1, Inf)))
  df = as.data.frame(as.matrix(dtm))
  df = as.matrix(df)
  df = df[-1,]
  return(df)
}

mt = convertToMatrix(tests)


# Method 2: Simply use the dictionary from the file,
# Apply it to the text, get rid of the unused ones 
testsClean = function(words)
  # regular way to clean the tests names
{
  words = strsplit(words, ",|;|:| and | or |\\)|\\(")
  words = unlist(words)
  words = gsub(words, pattern = 'test|tests|assay|microtest',replacement = '')
  
  words = gsub(x = words, pattern = '-|\\.', replacement = " ")
  # words = gsub(x = words, pattern = "$[ .]{1,2}", replacement = "")
  words = trimws(words, "both")
  words = words[which(words != "")]
  words = tolower(words)
  # set the first letter as upper case
  # words = paste(toupper(substring(words, 1,1)), substring(words, 2), sep = "")
  words = unique(words)
  words = sort(words)
}

testsDictionary = testsClean(download_data1$most_specific_diagnostic_Test)
testsDictionary2 = testsClean(download_data2$most_specific_diagnostic_Test)


# cosine similarity
library(SnowballC)
library(lsa)
mt1 = convertToMatrix(testsDictionary)
mt2 = convertToMatrix(testsDictionary2)
lsa:: cosine(mt1[1,],mt1[2,])
testsDictionary[1]
testsDictionary[2]




