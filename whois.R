library(rentrez)
keyword <- 'lncRNA'
db <- 'pubmed'
web_env_search <- entrez_search(db=db, term = keyword, use_history=TRUE)
wh <- web_env_search$web_history
xml_res <- entrez_fetch(db = db, web_history = wh, retmax = 1000,rettype = 'xml')
res <- parse_pubmed_xml(xml_res)
authors <- lapply(res,function(e) e$authors)
names(authors) <- 1:length(authors)
article_keywords <- lapply(res, function(e) e$key_words) 
journal <- lapply(res, function(e) e$journal) 
titles <- lapply(res, function(e) e$title) 
year <- lapply(res, function(e) e$year)

save(res, file='searchRes.rda')

library(ewrefxn)
list2OccTable(authors) -> occtable
apply(occtable,1,sum) -> n

print(which(n==max(n)))
occtable <- occtable[ n != 1,]

##author interaction network
myHammingDist <- function(e1,e2) sum(e1 != e2)

tmp <- t(combn(nrow(occtable),2))
apply(tmp,1,function(e, occ) {
  e1 <- occ[e[1],]
  e2 <- occ[e[2],]
  res <- myHammingDist(e1,e2)
  res
},occ=occtable) -> d
author_interaction <- data.frame(authorA=rownames(occtable)[tmp[,1]],authorB=rownames(occtable)[tmp[,2]],dist=d)
save(author_interaction,file='author_interaction.rda')