library(rentrez)
keyword <- '"epithelial mesenchymal transition"[title/abstract]'
db <- 'pubmed'
web_env_search <- entrez_search(db=db, term = keyword, use_history=TRUE)
##on 07/06/2017, there are 9990 results...
nHits = web_env_search$count
cat('# of total paper found: ',nHits,"\n")
wh <- web_env_search$web_history

nfetch=1000 ## each time we fetch 1000 records

if(nHits %% nfetch == 0) {
  nround= nHits / nfetch } else {
                           nround = nHits %/% nfetch +1
                         }
res <- vector(mode='list')
for(i in 1:nround ) {
  cat(i,"\n")
  start <- (i-1)*nfetch + 1 - 1 ##note that retstart starts with ZERO
  if(i == nround) {
    rtmax <- nHits %% nfetch  } else {
                                rtmax <- nfetch
                              }
  cat('fetch the resulti','\n')
  resi <- entrez_fetch(db=db, web_history=wh,
                       retstart=start, retmax=rtmax,rettype='xml')
  cat('parsing resulti','\n')
  resi_parsed <- parse_pubmed_xml(resi)
  res <- c(res, resi_parsed)
  Sys.sleep(5) ##sleep for a while
}
save(res, file='EMT_paper_pubmed_07062017.rda')
##we got the results we want, but with one problem: there is no keyword information available in ncbi pubmed results....
