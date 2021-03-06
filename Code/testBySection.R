# apply dictionary to the sections, such as Masterial and Methods, method, study design.
# Yunzhe Li
# 10/20/2017

# extract the words from the section study and design

# from Ryan
testfinder = function(text,dictionary){
  #searches text for tests
  
  fixtext1 = paste(dictionary,'\\b',sep ='')
  fixtext2 = paste('(?i)\\b',fixtext1,sep='')
  indices = which(sapply(tolower(fixtext2),grepl,tolower(text)))
  result = dictionary[unique(indices)]
  return(result)
}


# still not work properly for the paper which don't have such sections
# computational expensive, because of using findSectionHeader() and getSectionText()
# in the same function
locate_section = function(xml, dictionary) 
  # only look for section of "study design" or "methods and materials"
  # input : one xml tree
  # output : texts of sections
{
  x = xml[[1]]
  header = findSectionHeaders(x)
  key = grep("study|method", names(header), ignore.case = TRUE, value = TRUE)
  
  if(length(unlist(key)) < 4 & length(unlist(key)) > 0){
    section = getSectionText(x)
    section_names = names(section)
    indices = sapply(section_names, function(x) grepl(x, key, ignore.case = TRUE))
    section_text = section[indices]
    # print(section_text)
    # section_text = paste(section_text, collapse =  "")
    # print(section_text)
    tests = testfinder(section_text, dictionary)
    tests = unique(tests)
    return(tests)
  } else{
    print("else")
  }
}
  
giveMeIndex = function(header)
{
  TF = grepl("study|method", names(header), ignore.case = TRUE)
  if(length(which(TF==TRUE)) != 0)
    return(1)
  else
    return(0)
}
giveMeSectionFile = function(headers)
{
  indices = sapply(headers, giveMeIndex)
  real_indices = which(indices == 1)
  sectionFiles = files[real_indices]
  return(unlist(sectionFiles))
}



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
