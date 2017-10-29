library(ReadPDF)
setwd('dsiPDFs/subsetPapers2/')
dictionary = read.csv('testdictionary.csv',stringsAsFactors = FALSE) #Github File
dictionary$x
dictionary = c(dictionary$x,'igm elisa')
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

Andria=  getSectionText('Andriamandimby-2011-Crimean-Congo hemorrhagic.pdf')
andriatext = paste(Andria$design,collapse = '')
dictionary
testfinder(andriatext,dictionary)


asnis =  getSectionText('Asnis-2010-Lymphocytic choriomeningitis virus.pdf')
andriatext = paste(Andria$design,collapse = '')

testfinder(andriatext,dictionary)

Balling = getSectionText('Balling-2014-Prevalence of antibodies against.pdf')
ballingtext = paste(Balling$methods,collapse = '')
testfinder(ballingtext,dictionary)


setwd('dsiPDFs/subsetPapers2/')

names(Balling)



'methods' %in% names(Balling)

getcorrecttext = function(x){
  
  sections = getSectionText(x)
  if('methods' %in% names(sections)){
    correctext = paste(sections[['methods']],collapse = '')
    print('hi')
    return(testfinder(correctext,dictionary))
  }
  else if('design' %in% names(sections)){
    correctext = paste(sections[['design']],collapse = '')
    print('hello')
    return(testfinder(correctext,dictionary))
    
  }
  else{
    print('idk what to do ')
  }
  }
  

getcorrecttext('Andriamandimby-2011-Crimean-Congo hemorrhagic.pdf')






