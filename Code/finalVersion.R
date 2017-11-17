# required packages
library(XML)
library(ReadPDF)

# findTestsInWholeText(wholeText,dictionary)
# findTestInSection(key,wholeText,sectionNames,dictionary)
# findTestInSectionName(sectionNames,dictionary)
# getKey(sectionNames)
# testfinder(text,dictionary)

# main function
findTests = function(file, dictionary)
  # find all tests in the pdf.
{
  # extract all text grouped by section names.
  wholeText = getSectionText(file)
  
  # extract all section names the getSectionText return. 
  sectionNames = names(wholeText)
  
  # get tests in section names
  testsInSectionName = findTestInSectionName(sectionNames, dictionary)
  
  # git rid of whitespace
  # example: make "METH ODO L OGY" to "METHODOLOGY"
  sectionNames = gsub("\\s", "", sectionNames)
  
  # get indices of key sections
  key = getKey(sectionNames)
  
  if(length(key) != 0)
  { 
    # If we can find the methods, study, or design in section names
    # Then we only focus on the text under the section and ignore the rest
    
  
    testsInSection = findTestInSection(key, wholeText, sectionNames, dictionary)
    tests = unique(c(testsInSectionName, testsInSection))
    return(tests)
  }else{
    testsInWholeText = findTestsInWholeText(wholeText, dictionary)
    tests = unique(c(testsInSectionName, testsInWholeText))
    return(tests)
  }
}

testfinder = function(text,dictionary){
  #searches text for tests by using our dictionary files
  
  fixtext1 = paste(dictionary,'\\b',sep ='')
  fixtext2 = paste('(?i)\\b',fixtext1,sep='')
  indices = which(sapply(tolower(fixtext2),grepl,tolower(text)))
  result = dictionary[unique(indices)]
  return(result)
}

getKey = function(sectionNames)
  # get the section with words Study, Method, or Design
  # sectionNames is a vector
{
  key = grep("study|method|design", sectionNames, ignore.case = TRUE, value = FALSE)
  
  # determine whether the section with key words has subsections
  # example
  # 2.MaterialsandMethods  2.1.StudySites. 2.2.SampleCollection.
  # 2.3.PlaqueReductionNeutralizationTest(PRNT).
  # 2.1 to 2.3 are under section Materials and Methods but shows as new sections
  # we check the pattern digit followd by dot, dot followed by letter
  # such as 2.MaterialsandMethods
  new_key = key[grepl("^\\d.\\D", sectionNames[key])]
  
  if(length(new_key) != 0)
  {
    if(grepl("[:digit:]{1,1}.[:alpha:]{1,1}", sectionNames[new_key]))
    {    
      # get digit part
      num = gsub("\\D+", "", sectionNames[new_key])
      
      # get all subsections
      pattern = sprintf("^(%d.)", new_key)
      key = grep(pattern, sectionNames)
    }
  }
  return(key)
}


findTestInSectionName = function(sectionNames, dictionary)
  # get tests in section names
{
  if(length(sectionNames != 0))
  {
    nameString = paste(sectionNames, sep = "", collapse = " ")
    testsInName = testfinder(nameString, dictionary)
    return(testsInName)
  }
}

findTestsInWholeText = function(wholeText, dictionary)
{
  # deal with two different inputs
  # 1. word by word with whitespace spereated.
  # 2. sentence by sentence without whitespace sperated
  if(any(wholeText == " "))
  {
    wholeText = paste(wholeText, sep ='', collapse = '')
  }else{
    wholeText = paste(wholeText, sep = '', collapse = ' ')
  }
  tests = testfinder(wholeText, dictionary)
  tests = unique(tests)
  return(tests)
}


findTestInSection = function(key, wholeText, sectionNames, dictionary)
  # input: file name and dictionary file
  # output: get all tests names in the section of method or study
{
   
    section_text = unlist(wholeText[key], recursive = TRUE, use.names = FALSE)
    
    # deal with two different inputs
    # 1. word by word with whitespace spereated.
    # 2. sentence by sentence without whitespace sperated
    if(any(section_text == " "))
    {
      sectionText = paste(section_text, sep ='', collapse = '')
    }else{
      sectionText = paste(section_text, sep = '', collapse = ' ')
    }
    
    # get rid of wierd character
    sectionText = gsub("Â", " ", sectionText)
    tests = testfinder(sectionText, dictionary)
    tests = unique(tests)
    return(tests)
}


