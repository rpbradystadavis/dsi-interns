# apply dictionary to the sections, such as Masterial and Methods, method, study design.
# Yunzhe Li
# 10/20/2017

# extract the words from the section study and design

# from Ryan
testfinder = function(text,dictionary){
  #searches text for tests
  
  fixtext1 = paste(dictionary,'\\b',sep ='')
  fixtext2 = paste('(?i)\\b',fixtext1,sep='')
  indices = sapply(fixtext2,grepl,text)
  result = dictionary[indices]
  return(result)
  
  
}

# still not work properly for the paper which don't have such sections
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
    section_text = paste(section_text, sep = " ", collapse = " ")
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
