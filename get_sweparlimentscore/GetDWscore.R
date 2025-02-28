library(remotes)
library(pscl)
library(dwnominate)
library(pscl)
library(jsonlite)
library(dplyr)
library(dwnominate)

rm(list=ls())
if (!grepl("Sweden", getwd())) {setwd("./Sweden/")}



extract_filename <- function(file_path) {
  # Use basename to get the file name from the full path
  file_name <- basename(file_path)
  
  # Use gsub to remove the .json extension
  file_name <- gsub("\\.json$", "", file_name)
  
  return(file_name)
}

process_names <- function(input_vector, only_name = F) {
  # Extract the party 
  party <- gsub(".*\\((.*)\\).*", "\\1", input_vector)
  # Extract the names
  names_part <- gsub("\\s*\\(.*\\)", "", input_vector)
  full_name <- sapply(strsplit(names_part, ",\\s*"), function(x) paste(x[2], x[1]))
  if(only_name) {return(full_name)} else {
    result <- data.frame(name = full_name, party = party, stringsAsFactors = FALSE)
  }

  return(result)
}
process_names(authors_year)

all_files = list.files(path = "./get_sweparlimentscore/votes14_23", pattern = "\\.json$", full.names = T)
years = 2014:2023
rollcall_list = list()

for (y in 1:length(years)) {
  year = years[y]
  year_files = all_files[grep(year, all_files)]

#authors for that year
authors_year = vector()
for(file in year_files) {
    temp = fromJSON(file)
    authors_year = unique(c(authors_year,temp$namn))
}

legis_data = process_names(authors_year)
legis_data <- legis_data %>%
  distinct(name, .keep_all = TRUE)
vote_data = data.frame(namn = unique(process_names(authors_year, only_name = T)))

for(i in year_files) {
    temp = fromJSON(i)
    temp$namn = process_names(temp$namn, only_name = T)
    temp$vote =  as.vector(apply(temp[, -1], 1, function(x) colnames(temp)[-1][which.max(x)]))
    temp$vote = ifelse(temp$vote == "Ja",1,ifelse(temp$vote == "Nej",0,900))
    colnames(temp)[6] <- sprintf("vote_%s", extract_filename(i))
    vote_data = dplyr::left_join(vote_data, temp[,c("namn",sprintf("vote_%s", extract_filename(i)))], by = "namn")
    
}


vote_data[is.na(vote_data)] = 9 #replace NA as "9" not part of legislation
vote_data[vote_data == 900] = NA #replace 900 as "NA" not present for the vote
rownames(vote_data) = vote_data[,1]
vote_data = data.frame(vote_data[,-1])


rc_object <- pscl::rollcall(
    data = vote_data,  # Exclude legislator_id, party, and region columns for votes
    yea = 1,                       # '1' for "yea" votes
    nay = 0,                       # '0' for "nay" votes
    notInLegis = 9,                # '9' for not in legislature
    missing = NA,                  # 'NA' for missing votes
    legis.data = legis_data,
    vote.names = extract_filename(year_files) # vote names for rc object.
    )
rownames(rc_object$votes) = unique(process_names(authors_year, only_name = T))
rollcall_list[[y]] = rc_object
}


result = dwnominate(rollcall_list) # takes about 10 mins
saveRDS(result,"./get_sweparlimentscore/dw_score.rds")
