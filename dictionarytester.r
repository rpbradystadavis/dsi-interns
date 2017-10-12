library(ReadPDF)

dictionary = read.csv('testdictionary.csv',stringsAsFactors = FALSE) #Github File
dictionary$x

list.files()
testfinder = function(text,dictionary){
  #searches text for tests
  
  fixtext1 = paste(dictionary,'\\b',sep ='')
  fixtext2 = paste('(?i)\\b',fixtext1,sep='')
  indices = which(sapply(fixtext2,grepl,text))
  result = dictionary[unique(indices)]
  return(result)
  
  
}


#First Example
getwd()
Corman=  getSectionText('Corman.pdf')
cormantext = paste(Corman$`The Study`,collapse = '')


testfinder(cormantext,dictionary$x)

