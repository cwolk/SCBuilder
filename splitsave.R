library(readr)

BASE <- "~/data/corpora/SAVE/Plain text/"

# for (subcorp in list.files(BASE)) {
#   for (newsp in list.files(paste0(BASE, "/", subcorp))) {
#     for (file in list.files(paste0(BASE, "/", subcorp, "/", newsp))) {
#       txt <- strsplit(read_file(paste0(BASE, "/", subcorp, "/", newsp, "/", 
#                                        file)), "\r\n\r\n")[[1]] 
#       for (subtextnum in 1:length(txt)) {
#         cat(txt[subtextnum],
#           file=paste0("~/data/save-single/", gsub("\\.txt$","", file), "-", 
#                       subtextnum, ".txt"))
#       }
#     }
#   }
# }

corp <- data.frame(file=(list.files(BASE, recursive = TRUE)))
                     #list.files("~/data/save-single/"))
corp$Country <- gsub("^([^/]*)/.*", "\\1", corp$file)
# corp$Country <- gsub("^([^_]+)_.*", "\\1", corp$file)
corp$Newspaper <- gsub("^[^_]+_([^_]+)_.*", "\\1", corp$file)
corp$Year <- as.numeric(gsub(".*(\\d{4}).*", "\\1", corp$file))
corp$Month <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")[ as.numeric(gsub(".*[^\\d](\\d\\d)[_-].*", "\\1", corp$file)) ]

write.csv(corp, "~/data/save-meta.csv", row.names = FALSE)
