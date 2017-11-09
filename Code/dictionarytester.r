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


asnis$
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
andria = getSectionText('Andriamandimby-2011-Crimean-Congo hemorrhagic.pdf',dictionary)

#####################

nonum = gsub(x=names(andria),replacement = ' ',pattern = '\\d')
nopunc = gsub(x=nonum,replacement = ' ',pattern = '\\.')
getcorrecttext('Andriamandimby-2011-Crimean-Congo hemorrhagic.pdf',dictionary)

getcorrecttext = function(x,dictionary){
  #Attempts to find test names by section name
  sections = getSectionText(x)
  names(sections) = tolower(names(sections))
  
  
  #Find test in section name
  sectionNames = gsub('\\s','',x = names(sections))
  nonum = gsub(x=sectionNames,replacement = ' ',pattern = '\\d')
  nopunc = gsub(x=nonum,replacement = ' ',pattern = '\\.')
  trims = gsub("^\\s+|\\s+$", "", nopunc)
  testinheader = try(lapply(trims,function(x){any(agrep(pattern = x,x = dictionary ,ignore.case = T,max.distance = 3))}))
  untest = try(unlist(testinheader))
  print(untest)
  if(length(untest) != 0 & !is.na(untest)){
  if(any(untest)){
    position = try(which(unlist(testinheader)))
    print(position)}
}
  if(length(position) != 0){
  #print section names
  print(sectionNames[position])
    #search sections for more matches
  print(testfinder(sections[position],dictionary))}
  #Continue to look at other Sections
  if(any(grepl('methods',names(sections),ignore.case = T))){
    correctext = paste(sections[agrep('methods',names(sections))],collapse = '')
    print('hi')
    tests = testfinder(correctext,dictionary)
    print(tests)
    return(tests)
  }
  else if('design' %in% names(sections)){
    correctext = paste(sections[['design']],collapse = '')
    tests = testfinder(correctext,dictionary)
    print(tests)
    return(tests)
    
  }
  else if (any(grepl('the study',names(sections),ignore.case = T))){
    correctext = paste(sections[grep('the study',names(sections),ignore.case = T)])
    print('sup jack')
    tests = testfinder(correctext,dictionary)
    print(tests)
    return(tests)

    
  }else if(any(grepl('materials and methods',nopunc,ignore.case = T))){
      correctext = paste(sections[['materials and methods']],collapse = '')
    
      return(testfinder(correctext,dictionary))
  }
  else{
    #Message to go back and see problem
    print('idk what to do ')
    tests = testfinder(sections,dictionary)
    print(tests)
    return(testfinder(sections,dictionary))
  }
  
  }

  

pdfs = pdfs[-1]
getcorrecttext('Buckley-2003-Serological evidence of West Nile.pdf',dictionary)
getcorrecttext('')

jack = getSectionText('Burgueno-2013-Seroprevalence of St. Louis enc2.pdf')
jack$`2.3. Plaque Reduction Neutralization Test (PRNT).`
pdfs = list.files(pattern = '.pdf')
sapply(pdfs[-10],function(x){ print(x)
  getcorrecttext(x,dictionary)})
nodigit = gsub(pattern = '\\d',replacement = ' ',x = names(jack))
nopunc = gsub(pattern = '\\.',replacement = ' ',x = nodigit)
nopunc
getcorrecttext('Causey-1970-Congo virus from domestic livestoc.pdf',dictionary)
testing = getSectionText('Causey-1970-Congo virus from domestic livestoc.pdf')
which(pdfs == 'Causey-1970-Congo virus from domestic livestoc.pdf')
pdfs[-10]

getcorrecttext('Buckley-2003-Serological evidence of West Nile.pdf',dictionary)




getcorrecttext('Buckley-2003-Serological evidence of West Nile.pdf',dictionary )

bucks = getSectionText('Buckley-2003-Serological evidence of West Nile.pdf')

testfinder(testing,dictionary)
testing = paste(bucks$METHODS, collapse = '')




getcorrecttext('Yuan 2012.pdf',dictionary)


