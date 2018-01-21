library(magrittr)
library(dplyr) 

dracLines <- readLines("unformat.txt", encoding = "WINDOWS-1252")


sections <- grep("Study Guide", dracLines, value=TRUE) %>%
  gsub("\\?", "", . )

(q <- grep("[0-9]+[.].*\\?", dracLines))

questions <- dracLines[q]

answers <- dracLines[q+1]

dfx <- data.frame(questions, answers, stringsAsFactors = FALSE)

xl <- which(dfx$answers=="")

# fix some missing skips:

dfx$answers[which(dfx$answer=="")] <- dracLines[q[xl]+3]

xsections <- grep("^1[.]", dfx$questions)

dfx$section <- "TBD"



for (i in 1: length(xsections)){
  dfx$section[xsections[i]:length(dfx$section)] <- sections[i]
}


for(i in 1:length(xsections)-1){
  dfx$section[xsections[i]:xsections[i+1]] <- sections[i]
}


quizes <- select(dfx, Chapter = section, Question = questions, Answer = answers)


cat(
  "---
title: \"Dracula Quiz\"
output:
  pdf_document: default
  word_document:
    reference_docx: mystyles.docx
---\n\n",
  
  
  file = "outquiz.rmd")

quizes <- sample_n(quizes, size = dim(quizes)[[1]])

#select sections

quizes <- filter(quizes, Chapter %in% sections[1:2])

for(i in 1:dim(quizes)[[1]]){
  cat(paste(quizes$Chapter[[i]], "\n\n", gsub("^[0-9]+", i, quizes$Question[[i]]),
             "\n\\vspace{1in}\n\n"),
      file = "outquiz.rmd",
      append = TRUE)
}

cat("\n\\newpage", file = "outquiz.rmd", append = TRUE)
cat("\n## Answers\n\n", file = "outquiz.rmd", append = TRUE)

for(i in 1:dim(quizes)[[1]]){
  cat(paste0( 
             "\n\n", i,".", quizes$Answer[[i]], "\n\n"),
      file = "outquiz.rmd",
      append = TRUE)
}



