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
files = getfiles("/home/rstudio/DSI-ReadPDF/subsetPapers2/pdf")

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
library(googlesheets)
gs_ls()
sheet <- gs_title("forInterns2.csv")
data <- gs_read(ss = sheet)
download_data = as.data.frame(data)

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
tests = make_test_dictionary(download_data$most_specific_diagnostic_Test)
tests


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
