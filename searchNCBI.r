searchNCBI = function(keyword, db = 'pubmed',out = 'searchNCBI.tsv') {
  library(ewrefxn)
  require0('rentrez')
  web_env_search <- entrez_search(db=db, term = keyword, use_history=TRUE)
  wh <- web_env_search$web_history
  xml_res <- entrez_fetch(db = db, web_history = wh, retmax = 1000,rettype = 'xml')
  res <- parse_pubmed_xml(xml_res)
  authors <- lapply(res,'[[','authors')
  authors1 = unlist(lapply(authors, function(e) paste(e, collapse = ';')))
  article_keywords <- lapply(res, '[[', 'key_words') 
  keywords1 = unlist(lapply(article_keywords, function(e) paste(e, collapse = ';')))
  journal <- lapply(res, '[[', 'journal') 
  journal1 = unlist(lapply(journal, function(e) paste(e, collapse = ';')))
  titles <- lapply(res, '[[', 'title') 
  titles1 = unlist(lapply(titles, function(e) paste(e, collapse = ';')))
  year <- lapply(res, '[[', 'year')
  year1 = unlist(lapply(year, function(e) paste(e, collapse = ';')))
  abstr <- lapply(res, '[[', 'abstract')
  abstr1 = unlist(lapply(abstr, function(e) paste(e, collapse = ';')))
  pmid <- lapply(res, '[[', 'pmid')
  pmid1 = unlist(lapply(pmid, function(e) paste(e, collapse = ';')))

  require0('dplyr')
  require0('readr')
  require0('readxl')

  ifdat = read_excel('ImpactFactorJCR_2017release.xlsx',1)
  ifdat$`Full Journal Title` = toupper(ifdat$`Full Journal Title`)
  match(toupper(outputTbl$journal), ifdat$`Full Journal Title`) -> pos
  if_score = ifdat[pos,]$`Journal Impact Factor`
  outputTbl$`Impact Factor2017` = if_score


  outputTbl = tbl_df(data.frame(
    title = titles1,
    authors = authors1,
    keywords = keywords1,
    abstr = abstr1,
    year = year1,
    journal = journal1,
    pmid = pmid1,
    ImpactFactor2017 = if_score
    )) 
  write_tsv(outputTbl, out)
}
