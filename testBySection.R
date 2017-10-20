# apply dictionary to the sections, such as Masterial and Methods, method, study design.
# Yunzhe Li
# 10/20

# extract the wrods from the section study and design
header = ReadPDF::findSectionHeaders(xmls[[1]])
# header = ReadPDF::nodesByLine(header)
doc = header$`3. Study design`
texts = sapply(doc, ReadPDF::getDocText)
test = texts[1]
test
someCorpus = Corpus(VectorSource(test))
tdm = TermDocumentMatrix(someCorpus, control = list(tokenize = AlphabeticTokenizer,
                                                    removePunctuation = TRUE,
                                                    removeNumbers = TRUE,
                                                    wordLengths = c(1, Inf)))
thedf = as.data.frame(as.matrix(tdm))
paperwords = rownames(thedf)
testsDictionary
View(thedf)