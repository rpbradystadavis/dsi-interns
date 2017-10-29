library(ReadPDF)
library(XML)
library(stringr)






firstest = read.csv('Workbook1.csv') #Workbook1 is a combination of For_interns1 and 2's column most_specific_diagnostic_Test
#Construct the Dictionary

createdict = function(x){
  #X is the list of tests on google spreadsheets on the google drives, CSV for student-interns 1 and 2 combined
  newtest = strsplit(x,";|,|:| and | or | \\)|\\(") #Sometimes they list multiple tests on same line, create new line for every test
  jacklist = unlist(newtest) #flatten
  whitespaceremove = trimws(tolower(jacklist),'both') #remove white space
  newmat = matrix(nrow = 764,ncol = 1)
  
  for(i in 1:26){
    newmat = cbind(newmat,str_count(whitespaceremove,letters[i]))
  }
  actualmat = newmat[,-1]
  testlist = as.data.frame(actualmat)
  names(testlist) = letters
  testlist$names = whitespaceremove
  
  return(testlist)
}
testdict = createdict(firstest)





#Spell Checker
testspellchecker = function(x,dictionary){
  #x is the word you want to see if there are any misspellings
  #Dictionary is the dataframe  created in createdict
  #First part of function cleans up entered x
  nopuncx = gsub(x = x,";|,|:| and | or | \\)|\\|-",'') 
  loweredx = tolower(nopuncx)
  alphamat = matrix(nrow = 1,ncol = 1)
  
  for(i in 1:26){
    alphamat = cbind(alphamat,str_count(loweredx,letters[i]))
  }
  
  todf = as.data.frame(alphamat)
  todf = todf[,-1]
  todf$names =  loweredx
  
  colnames(todf) = colnames(dictionary)
  checker = rbind(todf,dictionary)
  checker$sum = rowSums(checker[,1:26])
  closewords = checker[checker$sum > checker$sum[1] -5 & checker$sum < checker$sum[1] + 5,]
  onlynums = closewords[,1:26]
  convertmatrix = as.matrix(onlynums)
  #print(class(as.numeric(convertmatrix)[1,1]))
  a = NA
  for( i in 1:length(convertmatrix[,1])){
    a = cbind(a,cosine(as.vector(convertmatrix[1,]),as.vector(convertmatrix[i,])))
  }
  scores = a[,-1]
  closewords$scores = scores
  return(closewords)
  
}


#Example
costest = testspellchecker('neutralization test',testdict)
costest[order(costest$scores,decreasing = T),]


testlist[testlist$names == 'neutraliztion',]




#FOLLOWING IS HOW I FOUND THE COSINE TEST
install.packages('sos')
library(sos)
findFn("cosine", maxPages=2, sortby="MaxScore")

install.packages('lsa')
library(lsa)
lsa:: cosine(testing,testing2)