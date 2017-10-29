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




RyansDictionary = read.csv("../../dsi-interns/testdictionary.csv", stringsAsFactors = FALSE, fill = TRUE)
RyansDictionary = RyansDictionary$x


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
# headers = sapply(xmls, findSectionHeaders)
sectionFiles = giveMeSectionFile(headers)
# write it to a text file for next time use
# This file contains all files which has section of study or methods
write(sectionFiles, "sectionFiles.txt")