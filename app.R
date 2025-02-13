#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/ 
#
library(tidyverse)
library(DT)
library(r2d3)
library(shiny)
library(shinythemes)
library(shinyjs)
source("read_data.R")
# source("stats.R")
# source("networks.R")
type <- "reduced"
if(type == "full"){
    stop("Not supported in this version")
    tmp <- setup_workspace(reread = F)
    coding_sheet <- tmp[[1]][[1]]
    papers <- tmp$paper
    paper_list <- c("Coding Sheet", unique(papers$sheet) %>% sort())
} else{
    papers <- setup_workspace(reread = F, version = "reduced")
    papers <- papers %>% rename(sample_size = sample_size_n, 
                                duration = duration_mostly_estimates,
                                individual_or_group_testing = individual_or_group_testing_if_child_report_na_if_parent_or_teacher)
    paper_list <- c("--", unique(papers$paper_id))
    var_list <- names(papers)
}
keys <- read_keys()
print(setdiff(names(papers), keys$Column))
print(setdiff(keys$Column, names(papers)))

impressum <- function(){
    p(
        "Measuring Motivation for Music", 
        shiny::tags$br(), 
        shiny::tags$br(), 
        "Author: Klaus Frieler", 
        shiny::tags$br(), 
        shiny::a(href = "https://www.aesthetics.mpg.de/en.html", 
                 "Max Planck Institute for Empirical Aesthetics, Frankfurt/M, Germany", 
                 target = "_blank"),
        shiny::tags$br(),
        shiny::tags$br(), 
        "Powered by",
        shiny::tags$br(),
        shiny::a(href = "http://www.music-psychology.de/",
                 "Deutsche Gesellschaft für Musikspsychologie", target = "_blank"),
        style = "font-size: 10pt; display: block"
    )
    
}


ui <-   
    shiny::shinyUI(
    navbarPage(
        title = "Measuring Motivation for Music ", 
        theme = shinytheme("spacelab"),
        id = "tabs",
        tabPanel(
            "Papers",
            sidebarLayout(
                sidebarPanel(
                    shinyjs::useShinyjs(),
                    # Input: Select information ----
                    selectInput(inputId = "variables", 
                                label = "Variables",
                                choices = var_list, 
                                selected = var_list[1:5],
                                multiple = T, selectize = T),
                    selectInput(inputId = "paper", 
                                label = "Paper",
                                choices = paper_list, 
                                selected = paper_list[1],
                                multiple = F, selectize = F),
                    # selectInput(inputId = "comnunity_id", 
                    #             label = "Community ID",
                    #             choices = c("All", sort(as.integer(unique(community_data$community)))), selected = "All",
                    #             multiple = T, selectize = T),
                    impressum(),
                    width = 2
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                    DT::DTOutput("paper_stats")
                    )
                    
                )
            ),
        tabPanel(
            "Glossary",
            sidebarLayout(
                sidebarPanel(
                    shinyjs::useShinyjs(),
                    # Input: Select information ----
                    impressum(),
                    width = 2
                ),
                
                # Main panel for displaying outputs ----
                mainPanel(
                    DT::DTOutput("glossary")
                )
            )
        )
        
        # tabPanel(
        #     "Network",
        #     sidebarLayout(
        #         sidebarPanel(
        #             # Input: Select information ----
        #             selectInput(inputId = "subset", 
        #                         label = "Subnetwork",
        #                         choices = c("All", "Core", "Rim"), selected = "All",
        #                         multiple = F, selectize = T),
        #             selectInput(inputId = "highlight_community", 
        #                         label = "Highlight Community",
        #                         choices = get_community_entries(type = "communities"), selected = "---",
        #                         multiple = F, selectize = T),
        #             selectInput(inputId = "highlight_author", 
        #                         label = "Highlight Author",
        #                         choices = get_community_entries(type = "author"), selected = "---",
        #                         multiple = F, selectize = T),
        #             selectInput(inputId = "charge", 
        #                         label = "Node Charge",
        #                         choices = seq(1, 5)*(-60), selected = "-120",
        #                         multiple = F, selectize = F),
        #             selectInput(inputId = "link_distance", 
        #                         label = "Link Distance",
        #                         choices = seq(1, 5)*10, selected = "20",
        #                         multiple = F, selectize = F),
        #             selectInput(inputId = "font_size", 
        #                         label = "Font Size",
        #                         choices = seq(1, 10)*2 + 12, selected = "24",
        #                         multiple = F, selectize = F),
        #             selectInput(inputId = "opacity", 
        #                         label = "Opacity",
        #                         choices = seq(0, 1, .1), selected = "0.8",
        #                         multiple = F, selectize = F),
        #             impressum(),
        #             width = 2
        #         ),
        #         
        #         # Main panel for displaying outputs ----
        #         mainPanel(forceNetworkOutput("collab_network", height = "1000px"))
        #     )
        ))
            
        

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    # observeEvent(input$subset,{
    #     comm_entries <- get_community_entries(tolower(input$subset), type = "communities")
    #     updateSelectizeInput(session, "highlight_community", choices = comm_entries, selected = comm_entries[1])
    #     comm_entries <- get_community_entries(tolower(input$subset), type = "author")
    #     updateSelectizeInput(session, "highlight_author", choices = comm_entries, selected = "")
    # })
    # observeEvent(input$highlight_community,{
    #     if(!is.null(input$highlight_community) & input$highlight_community != "---"){
    #         updateSelectizeInput(session, "highlight_author", selected = "---")
    #     }
    # })
    # observeEvent(input$highlight_author,{
    #     if(!is.null(input$highlight_author) & input$highlight_author != "---"){
    #         updateSelectizeInput(session, "highlight_community", selected = "---")
    #     }
    # })
    # observeEvent(input$stats_type,{
    #     if(!is.null(input$stats_type)){
    #         if(input$stats_type == "Communities"){
    #             shinyjs::enable("comnunity_id")
    #         }
    #         else{
    #             shinyjs::disable("comnunity_id")
    #         }
    #     }
    # })
    output$glossary <- renderDataTable({
      keys  
    }, filter = "top", options = list(lengthMenu = list(c(10, -1), c("10",   "All"))))
    output$paper_stats <- renderDataTable({
        # generate bins based on input$bins from ui.R
        #browser()
        # data <- NULL
        # if(input$stats_type == "Basic"){
        #     data <- get_basic_stats(master)
        # }
        # if(input$stats_type == "Author"){
        #     data <- get_author_stats(master) %>% 
        #     arrange(desc(n_paper)) %>% 
        #         set_names("Name", 
        #                   "Papers", 
        #                   "First Authored", 
        #                   "Last Authored", 
        #                   "Middle Authored",
        #                   "Themes (Original)",
        #                   "Themes (Categorized)",
        #                   "Thematic Diversity (Original)",
        #                   "Thematic Diversity (Categorized)",
        #                   "Co-Workers",
        #                   "Mean Co-Workers/Paper",
        #         ) %>% select(-`Themes (Original)`, -`Thematic Diversity (Original)` )
        # }
        # if(input$stats_type == "Theme (Categorized)"){
        #     data <- get_theme_stats(master) %>% 
        #         arrange(desc(n_papers)) %>% 
        #         set_names("Theme (Categorized)", "Papers", "Authors", "Mean Author/Paper")
        # }
        # if(input$stats_type == "Theme (Original)"){
        #     data <- get_theme_stats(master, "theme_cleaned") %>% 
        #         arrange(desc(n_papers)) %>% 
        #         set_names("Theme (Original)", "Papers", "Authors", "Mean Author/Paper")
        # }
        # if(input$stats_type == "Communities"){
        #     data <- community_data
        #     #browser()
        #     if(!is.null(input$comnunity_id) && input$comnunity_id != "All"){
        #         data <- community_data %>% filter(community %in% input$comnunity_id)    
        #     }
        #     data <- data %>%
        #         mutate(core = base_com == 1) %>% 
        #         select(name, community, n_comm, core) %>% 
        #         arrange(desc(n_comm)) %>% 
        #         set_names("Name", "Community ID", "Community Size", "Core")
        # }
        if(input$paper == "Coding Sheet"){
            return(coding_sheet)
        }
        else{
            if(type == "reduced"){
                if(input$paper == "--"){
                    ret <- papers %>%  select(-paper_id)
                } else{
                    ret <-papers %>% 
                        filter(paper_id == input$paper) %>%  select(-paper_id)
                }
            } else{
                ret <-papers %>% filter(sheet == input$paper) %>% select(-paper_id)
                
            }
            #browser()
            ret %>% select(all_of(input$variables))
        }
    }, filter = "top", options = list(lengthMenu = list(c(25, 50,  -1), c("25", "50",  "All"))))
    # output$collab_network <- renderForceNetwork({
    #     d3n <- get_network(master, 
    #                        author  = input$highlight_author, 
    #                        community = input$highlight_community,
    #                        set_globals = F, 
    #                        format = "d3", 
    #                        subset = tolower(input$subset)) 
    #     plot_D3_network(d3n, 
    #                     charge = as.numeric(input$charge),
    #                     linkDistance = as.numeric(input$link_distance),
    #                     fontSize = as.numeric(input$font_size),
    #                     opacityNoHover = as.numeric(input$opacity),
    #                     file = NULL)
    # })
}

# Run the application 
shinyApp(ui = ui, server = server)
