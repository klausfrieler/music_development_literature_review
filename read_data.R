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

read_data <- function(fname = "data/reviews.xlsx"){
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
setup_workspace <- function(reread = F){
  if(reread){
    read_data()
  }
  else{
    readRDS("data/all_papers.rds")
  }
}