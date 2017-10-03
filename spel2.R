library(ReadPDF)
library(XML)
library(stringr)
setwd('dsiPDFs/subsetPapers2/')
list.files()
testassay = convertPDF2XML('Andriamandimby-2011-Crimean-Congo hemorrhagic.pdf')


findSectionHeaders(testassay)

getwd()

firstest = read.csv('Workbook1.csv')
assaytext = getSectionText(testassay)
findSectionHeaders
newtext = paste(assaytext$design,collapse = ' ')
class(newtext)
FIXED = gsub(pattern = '  ',replacement = ' ',x = newtext)
str_extract(string = FIXED,pattern = 'The assay')
assaytext$design

fix.duncan = nodesByLine(findSectionHeaders(testassay))
fix.duncan$`3. Study design`

ourtext = paste(assaytext$design,collapse = ' ')
newtextstring = str_replace_all(ourtext,pattern = '  ','')
str_extract_all(newtextstring,pattern = regex('[^.]*. the assays',ignore_case = T))
str_extract_all(newtextstring,pattern = regex('tested by ([a-zA-Z- ]{0,40})',ignore_case = T))

firstest


gregexpr(text = firstest,',',ignore.case = T)

grepl(firstest[24],pattern = ',')
firstest[24]
str_extract(firstest[416],'(?<=;)(.*?)(?=;)')
strsplit(firstest
         ,
         )




1 + 2
firstest = as.character(firstest)

newtest = strsplit(firstest,";|,|:| and | or | \\)|\\(")
jacklist = unlist(newtest)
newjack =tolower(fixjack)
 #install.packages("tm")
library(tm)
makejackgreatagain

makejackgreatagainfixjack
makejackgreatagain = gsub(newjack,pattern = '',replacement = ' ')

makejackgreatagain[8]
fixjack = trimws(makejackgreatagain,'both')

TermDocumentMatrix(makejackgreatagain)
library(slam)
slam::simple_triplet_matrix(makejackgreatagain,j = letters)
testingjacks = as.DocumentTermMatrix(makejackgreatagain,weighting = 1)
newlist = c('a',)
newlist
fixjack[1]

letters
newjack

termFreq(newjack)
makejackgreatagain
termFreq(makejackgreatagain[1])
termFreq(letters)
makejackgreatagain[2]

letters
newjack
myjack = 'a b c d e f g h i j k l m n o p q r s t u v w x y z'
termFreq(myjack)
myjack
apes= 'hello my name is jack. Hello my name is Jack.'
termFreq(apes,control = list(stopwords = 'FALSE',stemming = FALSE,removePunctuation = FALSE))
stopwords(kind = 'en')
termFreq(myjack,control = list(stopwords = 'FALSE',stemming = FALSE,removePunctuation = FALSE))



5 + 5
library(tm)
tm:: Corpus(myjack,)
DocumentTermMatrix(newcorp)
reuters
newcorp = VCorpus(VectorSource(apes))
newcorp

reut21578 <- system.file("texts", "crude", package = "tm")
reuters <- VCorpus(DirSource(reut21578),
                   readerControl = list(reader = readReut21578XMLasPlain))

reuters


inspect(newcorp)
inspect(reuters)
inspect(testingjacks)
apes
testingit =strsplit(apes,'\\W')
termFreq(testingit[[1]])



strsplit(testingit[[1]],'[a-z]')

listofletters = trimws(gsub("", " ", testingit[[1]]))

myjack

newjack = str_split(myjack,' ')
testingjack  = str_split(makejackgreatagain,' ')
length(testingjack)
str_count(testingjack,'a')
newmat = matrix(nrow = 764,ncol = 1)
for(i in 1:26){
  newmat = cbind(newmat,str_count(testingjack,newjack[i]))
}
newmat
newjack =unlist(newjack)
listofletters
str

length(str_count(listofletters,newjack[i]))
newmat = newmat[,-1]


newmat

testingjack[[1]]
alphadf = as.data.frame(newmat)
holdf = alphadf
colnames(alphadf) = newjack
row.names(alphadf) = tolower(jacklist)

alphadf$names =  tolower(jacklist)

alphadf



alphadf$c = alphadf$c - 1 



alphadf$sum = rowSums(alphadf[,1:26])
alphadf$names

testspellchecker = function(x){
  nopuncx = gsub(x = x,";|,|:| and | or | \\)|\\|-",'')
  loweredx = tolower(nopuncx)
  xsplit = strsplit(loweredx,'')
  alphamat = matrix(nrow = 1,ncol = 1)
  
  for(i in 1:26){
    alphamat = cbind(alphamat,str_count(xsplit,newjack[i]))
  }
  
  todf = as.data.frame(alphamat)
  todf = todf[,-1]
  todf$names =  loweredx
  todf$c[1] = todf$c[1] - 1
  
  todf$sum = rowSums(todf[,1:26])
  colnames(todf) = colnames(alphadf)
  checker = rbind(todf,alphadf)
  
  closewords = checker[checker$sum > todf$sum -5 & checker$sum < todf$sum + 5,]
  score = c(0,0)
  for(i in 1:length(closewords$sum)){
    score = rbind(score,as.matrix(todf[,1:26])%*%t(as.matrix(closewords[i,1:26]))/(sqrt(rowSums(todf[,1:26]^2))*sqrt(rowSums(closewords[i,1:26]^2))))

    
  }
  actscore = 1 - (2*acos(score))/pi
  closewords$scores = actscore[-1,]
  
  return(closewords)
  
}
str_count('hello',newjack[12])
getresults[2,]

testspellchecker('hello')
(sqrt(todf$sum^2)  *sqrt(x$sum^2))
todf
cos(1)^-1
1 - 2*acos(1)/3.14



getresults = testspellchecker('neutralization')
elisamatch = getresults[getresults$scores > .7,]
alphadf
getresults[getresults > .9]

as.matrix(getresults[1,1:26])%*%t(as.matrix(getresults[1,1:26]))/(sqrt(rowSums(getresults[1,1:26]^2))*sqrt(rowSums(getresults[1,1:26]^2)))

acos(1)
getresults$sum = 5
elisamatch

getresults$names
factor(elisamatch$names)
alphadf$names













letters[1]





getresult = testspellchecker('neturalization')
whitespaceremove = trimws(tolower(jacklist),'both')
newmat = matrix(nrow = 764,ncol = 1)

for(i in 1:26){
  newmat = cbind(newmat,str_count(whitespaceremove,letters[i]))
}
actualmat = newmat[,-1]
testlist = as.data.frame(actualmat)
names(testlist) = letters
testlist$names = whitespaceremove

str_count(whitespaceremove,'g')




x

testspellchecker = function(x,dictionary){
  nopuncx = gsub(x = x,";|,|:| and | or | \\)|\\|-",'')
  loweredx = tolower(nopuncx)
  alphamat = matrix(nrow = 1,ncol = 1)
  
  for(i in 1:26){
    alphamat = cbind(alphamat,str_count(loweredx,newjack[i]))
  }
  
  todf = as.data.frame(alphamat)
  todf = todf[,-1]
  todf$names =  loweredx
  
  colnames(todf) = colnames(dictionary)
  checker = rbind(todf,dictionary)
  checker$sum = rowSums(checker[,1:26])
  closewords = checker[checker$sum > checker$sum[1] -3 & checker$sum < checker$sum[1] + 3,]
  convertmatrix = as.matrix(closewords)[,1:26]
  #print(class(as.numeric(convertmatrix)[1,1]))
  a = NA
  #for( i in 1:length(convertmatrix[,1])){
   # a = cbind(a,cosine(as.vector(convertmatrix[1,]),as.vector(convertmatrix[i,])))
  #}
  return(convertmatrix)
  
}

a  = 1:26
sapply(a,function(x){
  print("x \n")
})
?print
costest = testspellchecker('neutralization',testlist)
as.numeric(costest)
length(costest[,1])

testlist

rownames(getresult[getresult$scores > .8,])
for(i in 1:26){
  costest[,i] = as.numeric(costest[,i])
}
costest[,1]

install.packages('sos')
library(sos)
findFn("cosine", maxPages=2, sortby="MaxScore")
library(svs)
install.packages('svs')
svs:: dist_cosine(x = as.matrix(costest[,1:26]))

costest[1,]
install.packages('lsa')
library(lsa)
lsa:: cosine(testing,testing2)
class(as.vector(costest[1,1:26]))
class(costest[1,1:26])
vector(costest[1,1:26])
testing2 = as.vector(as.matrix(costest[2,1:26]))
