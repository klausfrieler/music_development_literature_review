library(tidyverse)

messagef <- function(...) message(sprintf(...))

fixed_note_columns <- function(data){
  #browser()
  col_names <- names(data)
  note_cols <- which(str_detect(col_names, "note_[0-9]+|^note$|^notes$"))
  ref_cols <- note_cols - 1
  new_names <- sprintf("note_%s", col_names[ref_cols])
  col_names[note_cols] <- new_names
  names(data) <- col_names
  data
}

read_data <- function(version = c("full", "reduced")){
  version <- match.arg(version)
  if(version == "full") {
    read_data_full()
  }
  else{
    read_data_reduced()
  }
}

normalize_authors <- function(author, year, paper_no){
  tmp <- str_split_fixed(author, ";", 2)
  tmp[,2][nzchar(tmp[,2])] <- " et al."
  sprintf("%s%s (%s) [%s]", tmp[, 1], tmp[,2], year, str_extract(paper_no, "[0-9]+"))  
}

read_data_reduced <- function(fname = "data/final_motivation_sheet_20241118.csv"){
  #sheets <- readxl::read_excel( fname, sheet = "Main Sheet") %>% arrange(author)
  sheets <- read_csv2( fname) %>% arrange(author)
  
  empty_cols <- sapply(sheets, function(x) mean(is.na(x)))
  empty_cols <- empty_cols[empty_cols == 1]
  sheets <- sheets %>% select(!names(empty_cols)) %>% janitor::clean_names()
  
  nm <- names(sheets) 
  
  logicals <- setdiff(nm[str_detect(nm, "_y_n")], "all_necessary_items_and_instructions_for_testing_available_in_paper_or_online_y_n")
  for(lg in logicals){
    sheets[[lg]] <- c("y" = 1, "n" = 0, "unclear" = -1)[sheets[[lg]]]
  }
  
  nm <- nm %>% 
    str_remove_all("[0-9]+")%>% 
    str_remove_all("_y_n")%>% 
    str_remove_all("__y__n")%>% 
    str_remove_all("_$") 
  
  sheets <- sheets %>% 
    set_names(nm) %>%
    mutate(paper_id = normalize_authors(author, year, paper_no))
  
  saveRDS(sheets, "data/all_papers_reduced.rds")
  sheets
}

read_data_full <- function(fname = "data/reviews.xlsx"){
  sheets <- readxl::excel_sheets( fname) 
  num_sheets <- length(sheets)
  master <- 
    map(1:num_sheets, function(sheet){
      messagef("Reading sheet %d [%s]", sheet, sheets[sheet])        
      tmp <- readxl::read_xlsx(fname, sheet = sheet) %>% janitor::clean_names()
      names(tmp) <- str_replace(names(tmp), "paper_no", "paper")
      if(sheet > 2){
        #browser()
        tmp <- fixed_note_columns(tmp)        
        if("paper" %in% names(tmp)){
          tmp$original_source <- str_remove(tmp$paper[1], "original source:") %>% str_trim()
          tmp <- tmp[2:nrow(tmp),]
        }
        tmp <- tmp %>% mutate(across(where(is.double), as.character))
        tmp$sheet <- sheets[sheet] 
        tmp$sheet_no <- sheet
        # if("changes_to_test" %in% names(tmp)){
        #   tmp$changes_to_test <- as.character(tmp$changes_to_test)
        # }
        # if("total_test" %in% names(tmp)){
        #   tmp$total_test <- as.character(tmp$total_test)
        # }
        # if("age_mean" %in% names(tmp)){
        #   tmp$age_mean <- as.character(tmp$age_mean)
        # }
        # if("age" %in% names(tmp)){
        #   tmp$age <- as.character(tmp$age)
        # }
        # if("scale" %in% names(tmp)){
        #   tmp$scale <- as.character(tmp$scale)
        # }
        # if("contour" %in% names(tmp)){
        #   tmp$contour <- as.character(tmp$contour)
        # }
        # if("interval" %in% names(tmp)){
        #   tmp$interval <- as.character(tmp$interval)
        # }
        if("year" %in% names(tmp)){
          tmp$year <- as.integer(tmp$year)
        }
        # if("validity" %in% names(tmp)){
        #   tmp$validity <- as.character(tmp$validity)
        # }
        # if("reliability" %in% names(tmp)){
        #   tmp$reliability <- as.character(tmp$reliability)
        # }
        # if("special_population" %in% names(tmp)){
        #   tmp$special_population <- as.character(tmp$special_population)
        # }
      }
      tmp
    }) %>% set_names(sheets)
  browser()
  invisible(list(coding_sheet = master[1], papers = bind_rows(master[3:63])))  
}

setup_workspace <- function(version = c("full", "reduced"), reread = F){
  version <- match.arg(version)
  if(reread){
    read_data(version)
  }
  else{
    if(version == "full"){
      fname <- "data/all_papers.rds"
    }
    else{
      fname <- "data/all_papers_reduced.rds"
    }
    readRDS(fname)
  }
}