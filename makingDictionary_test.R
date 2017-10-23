# get files
files = getfiles("/home/yunzheli/DSI-ReadPDF/subsetPapers2/pdf/")

# convert to xml documents
xmls_files = getxmls(files) # xmls[,1] are xmls, xmls[,2] are corresponding files names
xmls = xmls_files[,1]
files = xmls_files[,2]

## get data from google drive
## could also download the .csv files manually
library(googlesheets)
gs_ls()
sheet1 = gs_title("forInterns.csv")
data1 = gs_read(ss = sheet1)
sheet2 <- gs_title("forInterns2.csv")
data2 <- gs_read(ss = sheet2)
download_data1 = as.data.frame(data1)
download_data2 = as.data.frame(data2)

# clean the data
testsDictionary1 = testsClean(download_data1$most_specific_diagnostic_Test)
testsDictionary2 = testsClean(download_data2$most_specific_diagnostic_Test)
testsDictionary = c(testsDictionary1, testsDictionary2)

