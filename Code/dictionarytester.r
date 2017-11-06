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


testfinder(cormantext,dictionary)

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


Chevalier = getSectionText('Chevalier-2010-Environmental risk factors of W.pdf')
Chevalier$`METH ODO L OGY`
Chevaliertext= paste(Chevalier$`METH ODO L OGY`,collapse = '')
testfinder(Chevaliertext,dictionary)

asnis= getSectionText('Asnis-2010-Lymphocytic choriomeningitis virus.pdf')
asnis$
Chevaliertext= paste(Chevalier$`METH ODO L OGY`,collapse = '')
testfinder(Chevaliertext,dictionary)

Bosch = getSectionText('Bosch-2007-West Nile Virus, Venezuela.pdf')
Bosch$
Chevaliertext= paste(Chevalier$`METH ODO L OGY`,collapse = '')
testfinder(Chevaliertext,dictionary)

Breed = getSectionText('Breed-2010-Prevalence of henipavirus and rubul.pdf')
Breed$`The Study`
breedtext  = paste(Breed$`The Study`,collapse = '')
testfinder(breedtext,dictionary)

#####################








getcorrecttext = function(x,dictionary){
  #Attempts to find test names by section name
  sections = getSectionText(x)
  
  
  #Find test in section name
  sectionNames = gsub('\\s','',x = names(sections))
  testinheader = lapply(sectionNames,function(x){any(grepl(pattern = x,x = dictionary ,ignore.case = T))})
  position = which(sapply(testinheader,isTRUE))
  print(sectionNames[position])
  print(testfinder(sections[position],dictionary))
  
  #Continue to look at other Sections
  if(any(grepl('methods',names(sections),ignore.case = T))){
    correctext = paste(sections[['methods']],collapse = '')
    print('hi')
    return(testfinder(correctext,dictionary))
  }
  else if('design' %in% names(sections)){
    correctext = paste(sections[['design']],collapse = '')
    print('hello')
    return(testfinder(correctext,dictionary))
    
  }
  else if (any(grepl('the study',names(sections),ignore.case = T))){
    correctext = paste(sections[grep('the study',names(sections),ignore.case = T)])
    print('sup jack')
    return(testfinder(correctext,dictionary))
    
  }
  
  else{
    #Message to go back and see problem
    print('idk what to do ')
  }
  
}
  
write.csv(x = dictionary,file = 'dictionary11417')


