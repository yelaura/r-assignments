logfile <- readLines("C:\\Users\\lye\\Downloads\\logfile.txt")

logfile <- logfile[logfile != ""]

grepl_out <- grepl("pitopi", logfile, ignore.case=TRUE)
grepl_next <- append(grepl_out, FALSE, after=0)[1:length(grepl_out)]
lines_to_keep <- grepl_next

logfile <- logfile[lines_to_keep]
logfile
