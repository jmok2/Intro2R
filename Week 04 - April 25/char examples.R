rm(list = ls())

x <- c("hello there", "i'm the second element", "and this is the third")
nchar(x)

# extract
substr(x, 9, 15)

# replace
substr(x, 1, 5) <- "aaaaa"
x

# arguments are vectorized
substr(x, nchar(x) - 2, nchar(x))


# split character strings
x.sp <- strsplit(x, " ")
x.sp <- strsplit(x, "th")


# paste character strings
paste(letters[1:6], 1:6)
paste(letters[1:2], 1:10)
paste(rep(letters[1:6], each = 2), 1:2)

# change separator within elements
paste(letters[1:2], 1:10, sep = "-")
paste("Hap ", letters[1:2], 1:10, sep = "-")

# create single element
paste(letters[1:2], 1:10, collapse = "*")

load("merge data.rdata")
paste(haps$id, collapse = ", ")

# no separator
paste(letters[1:2], 1:10, sep = "")
paste0(letters[1:2], 1:10)


# Regular Expressions
x <- c("Here is some text", "This is more text", "I have the number 1",
       "22 is the number I have")

# which elements have the word "the"?
grep("the", x)

# which elements have numbers?
grep("[[:digit:]]", x)

grep("number [[:digit:]]", x)

gsub("This", "That", x)
