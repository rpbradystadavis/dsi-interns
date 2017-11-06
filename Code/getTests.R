# running them individually, then we compare what methods that we should use;
# then if all the TestInSection or TestInSectionNames are necessary to have.
# Finally, combining all three functions to a bigger one.

findTestInSection = function(file, dictionary)
  # input: file name and dictionary file
  # output: get all tests names in the section of method or study
{
  wholeText = getSectionText(file)
  sectionNames = names(wholeText)
  sectionNames = gsub("\\s", "", sectionNames)
  
  key = grep("study|method|design", sectionNames, ignore.case = TRUE, value = FALSE)
  new_key = key[grepl("[:digit:].[:alpha:]{1,1}", sectionNames[key])]
  if(length(new_key) != 0)
  {
    if(grepl("[:digit:]{1,1}.[:alpha:]{1,1}", sectionNames[new_key]))
    {    
      pattern = sprintf("^(%d.)", new_key)
      key = grep(pattern, sectionNames)
    }
  }
  
  if(length(key) != 0)
  {  
    section_text = unlist(wholeText[key], recursive = TRUE, use.names = FALSE)
    if(any(section_text == " "))
    {
      sectionText = paste(section_text, sep ='', collapse = '')
    }else{
      sectionText = paste(section_text, sep = '', collapse = ' ')
    }
    sectionText = gsub("Â", " ", sectionText)
    tests = testfinder(sectionText, dictionary)
    tests = unique(tests)
    return(tests)
  }else{
    return("I dont know")
  }
}

findTestInSectionName = function(file, dictionary)
{
  wholeText = getSectionText(file)
  sectionNames = names(wholeText)
  sectionNames = gsub("\\s", "", sectionNames)
  
  if(length(sectionNames != 0))
  {
    nameString = paste(sectionNames, sep = "", collapse = " ")
    testsInName = testfinder(nameString, dictionary)
    return(testsInName)
  }else{
    return("I don't know")
  }
}

findTestsInWholeText = function(file, dictionary)
{
  wholeText = getSectionText(file)
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
