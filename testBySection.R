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
locate_section = function(x) 
  # only look for section of "study design" or "methods and materials"
  # input : one xml tree
  # output : texts of sections
{
  if(is.list(x))
    x = x[[1]]
  header = findSectionHeaders(x)
  key = grep("study|method", names(header), ignore.case = TRUE, value = TRUE)
  # keys = unlist(strsplit(key, ' '))
  # keys = noquote(paste("\`", keys, "\`", sep = ""))
  # names(keys) = NULL
  # print(length(unlist(key)))
  if(length(unlist(key)) < 4 & length(unlist(key)) > 0){
    section = getSectionText(x)
    section_names = names(section)
    indices = sapply(section_names, function(x) grepl(x, key, ignore.case = TRUE))
    section_text = section[indices]
    section_text = paste(section_text, sep = " ", collapse = " ")
    tests = testfinder(section_text, testsDictionary)
    tests = unique(tests)
    return(tests)
  } else{
    # print("else")
    section = getSectionText(x)
    section_text = paste(section_text, sep = " ", collapse = " ")
    tests = testfinder(section_text, testsDictionary)
    tests = unique(tests)
    return(tests)
  }
}
