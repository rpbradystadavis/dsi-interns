
locate_section(xmls[[13]])

sapply(xmls, function(x) try(locate_section(x)))

locate_section(xmls[[2]])


header = ReadPDF::findSectionHeaders(xmls[[1]])
headers = sapply(xmls, function(x) ReadPDF::findSectionHeaders(x[[1]]))

section_names = sapply(headers, names)
section_names = unique(as.vector(unlist(section_names)))
grep("*study*", section_names, value = TRUE, ignore.case = TRUE)
grep("*method*", section_names, value = TRUE, ignore.case = TRUE)



section = header$`3. Study design`
section_text <- getSectionText(xmls[[1]])
section_text$design
t1 = ReadPDF::getDocText(section[[1]])
t2 = ReadPDF::getDocText(section[[2]])
t3 = ReadPDF::getDocText(section[[3]])
texts = sapply(doc, ReadPDF::getDocText)
texts = texts[1]
someCorpus = Corpus(VectorSource(texts))
tdm = TermDocumentMatrix(someCorpus, control = list(tokenize = AlphabeticTokenizer,
                                                    removePunctuation = TRUE,
                                                    removeNumbers = TRUE,
                                                    wordLengths = c(1, Inf)))
thedf = as.data.frame(as.matrix(tdm))
paperwords = rownames(thedf)
View(thedf)
t = sapply(testsDictionary1, function(x) grep(x, paperwords, value = TRUE, ignore.case = TRUE))
sapply(t, length)
# grepl = grep in logical


x1 = paste(testsDictionary1,'\\b',sep ='')
x2 = paste('(?i)\\b',x1,sep='')
indices = sapply(x2,grepl,texts)
indices1 = sapply(x2,grepl,paperwords)
testsDictionary1[unique(indices)]


Corman=  getSectionText('Corman.pdf')
cormantext = paste(Corman$`The Study`,collapse = '')


Andria_Header = ReadPDF::findSectionHeaders("Andriamandimby-2011-Crimean-Congo hemorrhagic.pdf")
Andria_Header_update = nodesByLine(Andria_Header)

Andria = ReadPDF::getSectionText("Andriamandimby-2011-Crimean-Congo hemorrhagic.pdf")
nodesByLine(Andria)
Andria_text = paste(c(Andria$`3.`, Andria$Study, Andria$design), sep = " ", collapse = "")

# from dictionarytester.r
testfinder(Andria_text, testsDictionary1) # "elisa"
testfinder(Andria_text, testsDictionary2) # "elisa"     "igg elisa" "igm"


