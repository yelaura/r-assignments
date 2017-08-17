# Exercise 2.3

# a. Read the complete file using readLines.

ex = readLines('example.txt')

# b. Separate the vector of lines into a vector containing comments and a vector containing the data. 
# Hint: use grepl.

comments = ex[grepl("//", ex)]
data = ex[!grepl("//", ex)]

# c. Read the data into a matrix as follows.
# (i) Split the character vectors in the vector containing data lines by semicolon (;) using strsplit.

mat = strsplit(data, ";")

# (ii) Find the maximum number of fields retrieved by strsplit. Append rows that are shorter with NAs.

num <- 0

for (item in mat){
  if (num < length(item)) {
    num = length(item)
  }
}

for (i in 1:length(mat)){
  if (length(mat[[i]]) != num) mat[[i]] = c(mat[[i]],"NA")
}

# (iii) Use unlist and matrix to transform the data to row-column format.

mat_rc <- unlist(mat)
mat_rc <- matrix(mat_rc, nrow = 3)
mat_rc <- t(mat_rc)


# d. From comment lines 2-4, extract the names of the fields. Set these as colnames for the matrix you just created.

fields = comments[grepl("Field", comments)]

names = c()

for (i in fields){
  names[length(names)+ 1] <- substr(i, 13, nchar(i))
}

colnames(mat_rc) <- names

# Exercise 2.4

# a. Coerce the matrix to a data.frame, making sure all columns are character columns.

df = data.frame(mat_rc)

# b. Use a string distance technique to transform the Gender column into a factor variable with labels man and woman.

lvls = levels(df$Gender)
for (i in 1:length(lvls)){
  if (adist(lvls[i], "female") > adist(lvls[i], "male")) {
    lvls[i] = "Man"
  } else{
    lvls[i] = "Woman"
  }
}
levels(df$Gender) = lvls

# c. Coerce the Age column to integer.

df$Age..in.years. <- as.numeric(df$Age..in.years.)

# d. Coerce the weight column to numeric. Hint: use gsub to replace commas with a period.

df$Weight..in.kg. <- as.numeric(gsub(",", ".", df$Weight..in.kg.))
