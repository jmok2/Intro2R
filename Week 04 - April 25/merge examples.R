rm(list = ls())
load("merge data.rdata")

# returns rows that are same in both (default all = FALSE)
cr.hp <- merge(cranial, haps)

# change to all = TRUE
cr.hp2 <- merge(cranial, haps, all = TRUE)

# retain haplotypes for all skull measurements
cr.hp3 <- merge(cranial, haps, by = "id", all.x = TRUE)

# add sex to cr.hp3 specifying by
cr.hp3.sex <- merge(cr.hp3, sex, by.x = "id", by.y = "specimens")

colnames(sex)[1] <- "id"
merge(cr.hp3, sex)

load("merge data.rdata")
x.cr <- cranial
x.hp <- haps
# assign rownames to use for indexing
rownames(x.cr) <- x.cr$id
rownames(x.hp) <- x.hp$id

x.cr$haps <- NA

i <- rownames(x.hp)[rownames(x.hp) %in% rownames(x.cr)]
x.cr[i, "haps"] <- x.hp[i, "haps"]


z <- intersect(rownames(x.hp), rownames(x.cr))
x.cr[z, "haps"] <- x.hp[z, "haps"]

# character indexing with exclusion
x.cr[setdiff(rownames(x.cr), c("Specimen-1", "Specimen-37")), ]

