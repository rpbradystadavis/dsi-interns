# yunzhe
# 10/23/2017

########################################

# Using 'tm' package to get the words, not better yet, so comment out temporarily
# 
# header = ReadPDF::findSectionHeaders(xmls[[1]])
# headers = sapply(xmls, function(x) ReadPDF::findSectionHeaders(x[[1]]))
# section = header$`3. Study design`
# section_text <- getSectionText(xmls[[1]])
# section_text$design
# t1 = ReadPDF::getDocText(section[[1]])
# t2 = ReadPDF::getDocText(section[[2]])
# t3 = ReadPDF::getDocText(section[[3]])
# texts = sapply(doc, ReadPDF::getDocText)
# texts = texts[1]
# someCorpus = Corpus(VectorSource(texts))
# tdm = TermDocumentMatrix(someCorpus, control = list(tokenize = AlphabeticTokenizer,
#                                                     removePunctuation = TRUE,
#                                                     removeNumbers = TRUE,
#                                                     wordLengths = c(1, Inf)))
# thedf = as.data.frame(as.matrix(tdm))
# paperwords = rownames(thedf)
# View(thedf)
# t = sapply(testsDictionary1, function(x) grep(x, paperwords, value = TRUE, ignore.case = TRUE))
# sapply(t, length)
# grepl = grep in logical

########################################

# RyansDictionary = read.csv("../../dsi-interns/Files/testdictionary.csv", stringsAsFactors = FALSE, fill = TRUE)
# RyansDictionary = RyansDictionary$x
# findTestInSection("Andriamandimby-2011-Crimean-Congo hemorrhagic.pdf", RyansDictionary)

# test the functions with different dictionaries
locate_section(xmls[[13]], RyansDictionary) # "antibodies" "ELISA"      "isolation" 
locate_section(xmls[[13]], testsDictionary) # "antibodies" "elisa"      "isolation" 
files[13] #  "Corman.pdf"

locate_section(xmls[[3]], RyansDictionary) # "antibodies" "ELISA" 
locate_section(xmls[[3]], testsDictionary) # "antibodies" "elisa" "immunofluorescence" "neutralization"   
files[3] # "Balling-2014-Prevalence of antibodies against.pdf"
download_data2$most_specific_diagnostic_Test[grep(files[3], download_data2)]
download_data2$PDF[grep(files[3], download_data2)]



# findSectionHeaders is very computationally expensive
headers = sapply(files, findSectionHeaders)
sectionFiles = giveMeSectionFile(headers)
# write it to a text file for next time use
# This file contains all files which has section of study or methods
write(sectionFiles, "sectionFiles.csv")

read.table("../../dsi-interns/Files/sectionFiles.txt")


setwd("../../subsetPapers2/pdf/")
library(ReadPDF)
library(XML)
my_tests = sapply(files, function(x) try(findTestInSection(x, RyansDictionary)))
myTests = lapply(my_tests, function(x) paste(x, collapse = ","))
myTests = sapply(myTests, unlist)
Files = sapply(files, unlist)
forIntern = sapply(Files, function(x) grep(x, download_data2$PDF))
forInternTests = sapply(forIntern, function(x) download_data2$most_specific_diagnostic_Test[x])
forInternTests_new = sapply(forInternTests, unique)
forInternTests_new = s

forInternTests = sapply(forInternTests_new, function(x) paste(x, collapse = ","))
names(forInternTests) = NULL
myFile = as.data.frame(cbind(Files, myTests, forInternTests))
write.csv(myFile, "../../dsi-interns/Files/myTests.csv")

               


JacksDictionary = read.csv("../../dsi-interns/Files/testdictionary (modified).csv", stringsAsFactors = FALSE)[[1]]
# computation is too expensive

findTestInSection = function(file, dictionary)
  # input: file name and dictionary file
  # output: get all tests names in the section of method or study
{
  # file = Files[1]
  # dictionary = JacksDictionary
  wholeText = getSectionText(file)
  sectionNames = names(wholeText)
  sectionNames = gsub("\\s", "", sectionNames)
  key = grep("study|method|design", sectionNames, ignore.case = TRUE, value = FALSE)
  new_key = key[grepl("[:digits:]{1,1}.[:alpha:]{1,1}", sectionNames[key])]
  if(length(new_key) != 0)
  {
    if(grepl("[:digits:]{1,1}.[:alpha:]{1,1}", sectionNames[new_key]))
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

Files
setwd("../../subsetPapers2/pdf/")
t=2
Files[t]
findTestInSection(Files[t], JacksDictionary)
findTestsInWholeText(Files[t], JacksDictionary)
forInternTests[t]



TestInSection = sapply(Files[1:10], function(x) try(findTestInSection(x, JacksDictionary)))
names(TestInSection) = NULL
TestInWholeText = sapply(Files[1:10], function(x) try(findTestsInWholeText(x, JacksDictionary)))
names(TestInWholeText) = NULL
TestInSectionName = sapply(Files[1:10], function(x) try(findTestInSectionName(x, JacksDictionary)))
names(TestInSectionName) = NULL