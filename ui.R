source("global.R", local = TRUE)

collapseInput <- function(inputId, boxId) {
  #' Return the current (collapsed) state of a box.
  #'
  #' State must have been altered in the server using js$collapse(box) before the state is known.
  #' As such, the box needs to be collapsed/expanded when the server loads so the function can get the state.
  tags$script(
    sprintf(
      "$('#%s').closest('.box').on('hidden.bs.collapse', function () {Shiny.onInputChange('%s', true);})",
      boxId, inputId
    ),
    sprintf(
      "$('#%s').closest('.box').on('shown.bs.collapse', function () {Shiny.onInputChange('%s', false);})",
      boxId, inputId
    )
  )
}

ui <- dashboardPage(skin = "black",
                    
                    # Header ----
                    header <- dashboardHeader(title = "RNA Web App",
                                              tags$li(actionBttn(
                                                "getting_started_help",
                                                label = "Getting started",
                                                style = "bordered",
                                                color = "primary",
                                                size = "sm"
                                              ), style = "width: 140px;
                                              padding-right:0px;
                                              padding-top:10px;
                                              height:20px;
                                              margin-right:0px;
                                              font-size: 20px;
                                              ",
                                              class = "dropdown"),
                                              tags$li(a(style = "padding-top:50px;
                                                        padding-bottom:0px;
                                                        padding-left:0px !important;
                                                        padding-right:10px !important;
                                                        margin-right:5px !important;
                                                        margin-left:0px !important;"), class = "dropdown"),
                                              tags$li(actionBttn(
                                                inputId = "feature_request",
                                                label = "Feature request",
                                                style = "unite",
                                                color = "royal",
                                                size = "sm"
                                              ), style = "width: 130px;
                                              padding-right;15px;
                                              margin-left;15px;
                                              padding-top:10px;
                                              height:20px;
                                              margin-right:15px;
                                              font-size: 20px;
                                              ",
                                              class = "dropdown"),
                                              tags$li(actionBttn(
                                                inputId = "bug_report",
                                                label = "Bug report",
                                                style = "unite",
                                                color = "danger",
                                                size = "sm"
                                              ), style = "width: 100px;
                                              padding-right;10px;
                                              padding-top:10px;
                                              height:20px;
                                              margin-right:10px;
                                              font-size: 20px;
                                              ",
                                              class = "dropdown"),
                                              tags$li(a(href = "https://www.organisation.com",
                                                        img(src = "logo2.png",
                                                            title = "organisation",
                                                            height = "25px"),
                                                        style = "padding-top:12px;
                                                        padding-bottom:5px;
                                                        padding-left:20px !important;
                                                        margin-left:0px !important;"),
                                                      class = "dropdown"),
                                              tags$li(a(href = "apps.organisation.com",
                                                        img(src = "rna_web_app_logo.png",
                                                            title = "RNA Web App",
                                                            height = "30px"),
                                                        style = "padding-top:10px;
                                                        padding-bottom:10px;
                                                        padding-left:20px !important;
                                                        margin-left:0px !important;"),
                                                      class = "dropdown")
                    ),
                    # Sidebar ----
                    sidebar <- dashboardSidebar(width = 240,
                                                collapsed = FALSE,
                                                shinyjs::useShinyjs(),
                                                extendShinyjs(script = "extensions.js", functions = c('collapse', 'disableTab','enableTab',
                                                                                                      'changeTitleVisibility', 'sidebarState')),
                                                tags$head(
                                                  tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
                                                ),
                                                sidebarMenu(id = "main_menu", 
                                                            style = "font-size:16px",
                                                            ## About ----
                                                            menuItem("About",
                                                                     tabName = "about",
                                                                     icon = icon("info-sign",
                                                                                 lib = "glyphicon"
                                                                     )
                                                            ),
                                                            conditionalPanel(condition = "input.main_menu == 'about'"         
                                                            ),
                                                            ## Graphs ----
                                                            menuItem("Graphs",
                                                                     tabName = "graphs",
                                                                     icon = icon("stats",
                                                                                 lib = "glyphicon"
                                                                     )
                                                            ),
                                                            add_busy_bar(color = "#0096FF", timeout = 400, height = "4px"),
                                                            conditionalPanel(condition = "input.main_menu == 'graphs'",
                                                            ),
                                                            ## Data ----
                                                            menuItem("Data",
                                                                     tabName = "data",
                                                                     icon = icon("list",
                                                                                 lib = "glyphicon"
                                                                     )
                                                            ),
                                                            conditionalPanel(condition = "input.main_menu == 'data'",
                                                            ),
                                                            ## Developer ----
                                                            menuItem("Developer",
                                                                     tabName = "dev",
                                                                     icon = icon("console",
                                                                                 lib = "glyphicon"
                                                                     )
                                                            ),
                                                            conditionalPanel(condition = "input.main_menu == 'dev'"         
                                                            )
                                                )
                    ),
                    # Body ----
                    body <- dashboardBody(style = "padding:0px;",
                                          tabItems(
                                            ## About ----
                                            tabItem(tabName = "about",
                                                    fluidRow(style = "padding: 15px;", 
                                                             column(width = 12,
                                                                    style="padding:0px",
                                                                    tabBox(width = 12,
                                                                           tabPanel("User info",
                                                                                    includeMarkdown("www/USERINFO.md")
                                                                           ),
                                                                           tabPanel("Dev info",
                                                                                    includeMarkdown("README.md"
                                                                                    )
                                                                           )
                                                                    )
                                                             )
                                                             
                                                    )
                                            ),
                                            ## Graphs ----
                                            tabItem(tabName = "graphs",
                                                    fluidRow(
                                                      fluidRow(
                                                        column(width = 10, style = "padding-top:15px;",
                                                               fluidRow(style = "padding-left:15px;",
                                                                        column(width = 6, 
                                                                               ### Plot design box ----
                                                                               box(width = 12,
                                                                                   id = "plot_design_box",
                                                                                   title = "Boxplot design",
                                                                                   style = "font-size:14px; border-left: outset #0073B7;",
                                                                                   collapsible = TRUE,
                                                                                   collapsed = FALSE,
                                                                                   fluidRow(
                                                                                     column(width = 4,
                                                                                            prettySwitch("x_switch",
                                                                                                         label = "Enable x categories", 
                                                                                                         value = TRUE,
                                                                                                         status = "info",
                                                                                                         fill = TRUE)
                                                                                     ),
                                                                                     style = "padding-bottom: 0px;
                                                                                  margin-bottom: 0px;" 
                                                                                   ),
                                                                                   fluidRow(style = "padding-top: 0px;",
                                                                                            column(width = 4,
                                                                                                   selectizeInput("boxplot_select_x",
                                                                                                                  label = "Select x variable",
                                                                                                                  choices = NULL,
                                                                                                                  multiple = TRUE,
                                                                                                                  options = list(maxOptions = 50, maxItems = 1)
                                                                                                   )
                                                                                            ),
                                                                                            column(width = 4,
                                                                                                   selectizeInput("boxplot_select_y",
                                                                                                                  label = "Select y (expression unit)",
                                                                                                                  choices = NULL,
                                                                                                                  multiple = TRUE,
                                                                                                                  options = list(maxOptions = 50, maxItems = 1)
                                                                                                   ),
                                                                                            ),
                                                                                            column(width = 4,
                                                                                                   selectizeInput("boxplot_select_group",
                                                                                                                  label = "Select grouping variable",
                                                                                                                  choices = NULL,
                                                                                                                  multiple = TRUE,
                                                                                                                  options = list(maxOptions = 50, maxItems = 1)
                                                                                                   )
                                                                                            )
                                                                                   ),
                                                                                   fluidRow(
                                                                                     column(width = 4,
                                                                                            prettySwitch("facet_switch",
                                                                                                         label = "Enable faceting", 
                                                                                                         value = TRUE,
                                                                                                         status = "info",
                                                                                                         fill = TRUE)
                                                                                     ),
                                                                                     column(width = 4),
                                                                                     column(width = 4,
                                                                                            prettySwitch("threshold_switch",
                                                                                                         label = "Show threshold", 
                                                                                                         value = FALSE,
                                                                                                         status = "info",
                                                                                                         fill = TRUE)
                                                                                     ),
                                                                                   ),
                                                                                   fluidRow(
                                                                                     column(width = 4,
                                                                                            selectizeInput("boxplot_select_facet",
                                                                                                           label = "Select faceting variable",
                                                                                                           choices = NULL,
                                                                                                           multiple = TRUE,
                                                                                                           options = list(maxOptions = 50, maxItems = 1)
                                                                                            )
                                                                                     ),
                                                                                     column(width = 4,
                                                                                            numericInput(
                                                                                              inputId = "facet_number",
                                                                                              label = "Number of facet columns",
                                                                                              value = 3,
                                                                                              min = 1,
                                                                                              step = 1
                                                                                            )
                                                                                     ),
                                                                                     column(width = 4,
                                                                                            numericInput(
                                                                                              inputId = "expression_threshold",
                                                                                              label = "Expression threshold",
                                                                                              value = 0,
                                                                                              min = 0,
                                                                                              step = 1
                                                                                            )
                                                                                     )
                                                                                   ),
                                                                                   actionBttn("plot_design_help",
                                                                                              label = "Help",
                                                                                              style = "bordered",
                                                                                              color = "primary",
                                                                                              size = "xs")
                                                                               ),
                                                                               collapseInput(inputId = "is_plot_design_box_collapsed",
                                                                                             boxId = "plot_design_box"),
                                                                        ),
                                                                        column(width = 6, style = "padding:0px ;",
                                                                               ### Plot styling box ----
                                                                               box(width = 12,
                                                                                   id = "plot_styling_box",
                                                                                   title = "Global plot styling",
                                                                                   style = "font-size:14px; border-left: outset #0073B7;",
                                                                                   collapsible = TRUE,
                                                                                   collapsed = FALSE,
                                                                                   fluidRow(
                                                                                     column(width = 3,
                                                                                            textOutput("grids_bxp"
                                                                                            )
                                                                                     ),
                                                                                     column(width = 1,
                                                                                            switchInput(inputId = "grid_lines_bxp",
                                                                                                        value = FALSE,
                                                                                                        size = 'mini'
                                                                                            )
                                                                                     ),
                                                                                     column(width = 1
                                                                                     ),
                                                                                     column(width = 3,
                                                                                            textOutput("extra_size_bxp"
                                                                                            )
                                                                                     ),
                                                                                     column(width = 1,
                                                                                            switchInput(inputId = "extra_size_switch_bxp",
                                                                                                        value = FALSE,
                                                                                                        size = 'mini'
                                                                                            )
                                                                                     )
                                                                                   ),
                                                                                   fluidRow(
                                                                                     column(width = 3,
                                                                                            textOutput("rotate_axis_labels_bxp"
                                                                                            )
                                                                                     ),
                                                                                     column(width = 1,
                                                                                            switchInput(inputId = "rotate_axis_labels_switch_bxp",
                                                                                                        value = FALSE,
                                                                                                        size = 'mini',
                                                                                                        onLabel = "45°",
                                                                                                        offLabel = "0°"
                                                                                            )
                                                                                     )
                                                                                   ),
                                                                                   actionBttn("plot_styling_help",
                                                                                              label = "Help",
                                                                                              style = "bordered",
                                                                                              color = "primary",
                                                                                              size = "xs")
                                                                               ),
                                                                               collapseInput(inputId = "is_plot_styling_box_collapsed",
                                                                                             boxId = "plot_styling_box")
                                                                        ),
                                                                        fluidRow(style = "padding-top: 21px; padding-left:15px;", 
                                                                                 column(width = 12,
                                                                                        tags$div(id = "tabbox",
                                                                                                 tabBox(width = 12,
                                                                                                        ### Boxplot ----
                                                                                                        tabPanel("Boxplot",
                                                                                                                 value = "boxplot_tab",
                                                                                                                 tags$div(
                                                                                                                   tags$div(
                                                                                                                     downloadBttn("download_boxplot",
                                                                                                                                  "Download",
                                                                                                                                  size = "xs"
                                                                                                                     ),
                                                                                                                     style = "display: inline-block;
                                                                                                                 padding-right: 10px"
                                                                                                                   ),
                                                                                                                   tags$div(
                                                                                                                     prettySwitch("scale_y_boxplot",
                                                                                                                                  label = "Scale y axis", 
                                                                                                                                  value = TRUE,
                                                                                                                                  status = "info",
                                                                                                                                  fill = TRUE),
                                                                                                                     style = "display: inline-block;"
                                                                                                                   ),
                                                                                                                   style = "padding-bottom: 10px;"
                                                                                                                 ),
                                                                                                                 # tags$div(
                                                                                                                 #   
                                                                                                                 # ),
                                                                                                                 plotOutput(outputId = "bxp"
                                                                                                                 ),
                                                                                                                 tags$head( # allow height to expand with plot size
                                                                                                                   tags$style("#bxp {height:100% !important} "
                                                                                                                   )
                                                                                                                 )
                                                                                                        ),
                                                                                                        ### Boxplot summary ----
                                                                                                        tabPanel("Boxplot data",
                                                                                                                 value = "boxplot_data_tab",
                                                                                                                 tags$div(
                                                                                                                   tags$div(
                                                                                                                     downloadBttn("download_boxplot_data",
                                                                                                                                  "Download",
                                                                                                                                  size = "xs"
                                                                                                                     ),
                                                                                                                     style = "display: inline-block;
                                                                                                                 padding-right: 10px;"
                                                                                                                   ),
                                                                                                                   tags$div(
                                                                                                                     actionBttn("summary_data_help",
                                                                                                                                label = "Help",
                                                                                                                                style = "bordered",
                                                                                                                                color = "primary",
                                                                                                                                size = "xs"
                                                                                                                     ),
                                                                                                                     style = "display: inline-block;"
                                                                                                                   ),
                                                                                                                   style = "padding-bottom: 10px;"
                                                                                                                 ),
                                                                                                                 fluidRow(
                                                                                                                   column(width = 12,
                                                                                                                          selectizeInput("summary_grouping_subset",
                                                                                                                                         label = "Select groupings to use",
                                                                                                                                         choices = NULL,
                                                                                                                                         multiple = TRUE,
                                                                                                                                         options = list(maxOptions = 50)
                                                                                                                          ),
                                                                                                                          tableOutput("bxp_summary_table"      
                                                                                                                          )
                                                                                                                   )
                                                                                                                 )
                                                                                                        ),
                                                                                                        ### Correlation plot ----
                                                                                                        tabPanel("Correlation",
                                                                                                                 value = "correlation_tab",
                                                                                                                 tags$div(
                                                                                                                   tags$div(
                                                                                                                     downloadBttn("download_cor_plot",
                                                                                                                                  "Download",
                                                                                                                                  size = "xs"
                                                                                                                     ),
                                                                                                                     style = "padding-right: 10px;
                                                                                                                   display: inline-block;" # why do I have to do this
                                                                                                                   ),
                                                                                                                   tags$div(
                                                                                                                     actionBttn("correlation_plot_help",
                                                                                                                                label = "Help",
                                                                                                                                style = "bordered",
                                                                                                                                color = "primary",
                                                                                                                                size = "xs"),
                                                                                                                     style = "display: inline-block"
                                                                                                                   ),
                                                                                                                   style = "padding-bottom: 10px;"
                                                                                                                 ),
                                                                                                                 selectizeInput("cor_plot_gene_subset",
                                                                                                                                label = "Select a subset of genes",
                                                                                                                                choices = NULL,
                                                                                                                                multiple = TRUE,
                                                                                                                                options = list(maxOptions = 50, maxItems = 2)
                                                                                                                 ),
                                                                                                                 selectizeInput("cor_plot_grouping_variable",
                                                                                                                                label = "Select variable to group by",
                                                                                                                                choices = NULL,
                                                                                                                                multiple = FALSE,
                                                                                                                                options = list(maxOptions = 50, maxItems = 1)
                                                                                                                 ),
                                                                                                                 plotOutput(outputId = "cor",
                                                                                                                            width = "1080px",
                                                                                                                            height = "480px" # this is good but dimensions may need to be changed
                                                                                                                 )
                                                                                                        ),
                                                                                                        ### Heatmap ----
                                                                                                        tabPanel("Heatmap",
                                                                                                                 value = "heatmap_tab",
                                                                                                                 tags$div(
                                                                                                                   tags$div(
                                                                                                                     downloadBttn("download_heatmap",
                                                                                                                                  "Download",
                                                                                                                                  size = "xs"
                                                                                                                     ),
                                                                                                                     style = "padding-right: 10px;
                                                                                                                   display: inline-block;"
                                                                                                                   ),
                                                                                                                   tags$div(
                                                                                                                     actionBttn("heatmap_help",
                                                                                                                                label = "Help",
                                                                                                                                style = "bordered",
                                                                                                                                color = "primary",
                                                                                                                                size = "xs"),
                                                                                                                     style = "padding-right: 10px;
                                                                                                          display: inline-block;"
                                                                                                                   ),
                                                                                                                   tags$div(
                                                                                                                     prettySwitch("heatmap_scaling_switch",
                                                                                                                                  label = "Scale by sample", 
                                                                                                                                  value = TRUE,
                                                                                                                                  status = "info",
                                                                                                                                  fill = TRUE),
                                                                                                                     style = "padding-bottom: 10px;
                                                                                                        display: inline-block;"
                                                                                                                   )
                                                                                                                 ),
                                                                                                                 plotOutput("heatmap_plot")
                                                                                                        )
                                                                                                 )
                                                                                        )
                                                                                 )
                                                                        )
                                                               )
                                                        ),
                                                        column(width = 2,
                                                               fluidRow(
                                                                 ### Data filters sidebar ----
                                                                 box(width = 12,
                                                                     title = "Data filters",
                                                                     collapsible = TRUE,
                                                                     height = "100vh",
                                                                     fluidRow(
                                                                       #### Gene box ----
                                                                       box(width = 12,
                                                                           id = "gene_box",
                                                                           title = "Genes",
                                                                           solidHeader = FALSE,
                                                                           collapsible = TRUE,
                                                                           collapsed = TRUE,
                                                                           background = "blue",
                                                                           selectizeInput("global_gene_type",
                                                                                          label = "Format",
                                                                                          choices = c("Symbol",
                                                                                                      "Ensembl"),
                                                                                          multiple = FALSE
                                                                           ),
                                                                           selectizeInput("global_gene",
                                                                                          label = "Gene",
                                                                                          choices = NULL,
                                                                                          multiple = TRUE,
                                                                                          options = list(maxOptions = 50)
                                                                           ),
                                                                           tags$div( # for css
                                                                             actionBttn(inputId = "search_gene",
                                                                                        label = "Update",
                                                                                        icon = icon("ok",
                                                                                                    lib = "glyphicon"
                                                                                        ),
                                                                                        style = "unite",
                                                                                        # color = "primary",
                                                                                        size = "sm"),
                                                                             style = "padding-bottom: 10px;
                                                                             padding-right: 5px;
                                                                             display: inline-block;"
                                                                           ),
                                                                           tags$div(
                                                                             actionBttn(inputId = "gene_upload",
                                                                                         label = "Upload list",
                                                                                         icon = icon("upload",
                                                                                                     lib = "glyphicon"
                                                                                                     ),
                                                                                         style = "unite",
                                                                                         size = "sm",
                                                                           ),
                                                                           style = "display: inline-block;"
                                                                           )
                                                                       ),
                                                                       collapseInput(inputId = "is_gene_box_collapsed",
                                                                                     boxId = "gene_box"),
                                                                       #### Filters box ----
                                                                       box(width = 12,
                                                                           id = "filters_box",
                                                                           title = "Filters",
                                                                           collapsible = TRUE,
                                                                           collapsed = FALSE,
                                                                           style = "border-left: outset #3C8DBC",
                                                                           actionBttn(inputId = "update_filters",
                                                                                      label = "Update",
                                                                                      icon = icon("ok",
                                                                                                  lib = "glyphicon"
                                                                                      ),
                                                                                      style = "pill",
                                                                                      size = "sm"),
                                                                           tags$div(id = "add_id",
                                                                                    actionBttn("add_filter",
                                                                                               label = "Add filter",
                                                                                               style = "pill",
                                                                                               size = "sm",
                                                                                               icon = icon("plus",
                                                                                                           lib = "glyphicon"
                                                                                               ))
                                                                           ),
                                                                           tags$div(id = "remove_id",
                                                                                    actionBttn("remove_filter",
                                                                                               label = "Remove filter",
                                                                                               style = "pill",
                                                                                               size = "sm",
                                                                                               icon = icon("minus",
                                                                                                           lib = "glyphicon"
                                                                                               )
                                                                                    )
                                                                           )
                                                                       ),
                                                                       collapseInput(inputId = "is_filters_box_collapsed",
                                                                                     boxId = "filters_box")
                                                                     )
                                                                     
                                                                 )
                                                               )
                                                        )
                                                      )
                                                    ), style = "padding-right:15px"
                                            ),
                                            ## Data ----
                                            tabItem(tabName = "data",
                                                    fluidRow(
                                                      column(width = 12,
                                                             ### Metadata table ----
                                                             box(width = 12,
                                                                 title = "Metadata table",
                                                                 tags$div(
                                                                   tags$div(id = "metadata_download_div",
                                                                            downloadBttn("download_metadata_table",
                                                                                         "Download",
                                                                                         size = "xs"
                                                                            ),
                                                                            style = "padding-right: 10px;
                                                                          display: inline-block" # why do I have to do this
                                                                   ),
                                                                   tags$div(
                                                                     actionBttn("metadata_table_help",
                                                                                label = "Help",
                                                                                style = "bordered",
                                                                                color = "primary",
                                                                                size = "xs"),
                                                                     style = "display: inline-block;",
                                                                   ),
                                                                   style = "disply: inline-block;
                                                                 padding-bottom: 10px"
                                                                 ),
                                                                 includeMarkdown("www/help_files/metadata_warning.md"),
                                                                 selectizeInput("data_column_selection",
                                                                                label = "Included columns",
                                                                                choices = NULL,
                                                                                multiple = TRUE
                                                                 ),
                                                                 DT::dataTableOutput("metadata_table"),
                                                                 style = "padding: 15px;
                                                                 overflow-x: scroll;
                                                                 overflow-y: scroll;
                                                                 height: 83vh"
                                                             ),
                                                             style = "padding: 15px"
                                                      ),
                                                    )
                                            ),
                                            ## Developer ----
                                            tabItem(tabName = "dev",
                                                    fluidRow(style = "padding: 15px;",
                                                             column(width = 12,
                                                                    passwordInput(inputId = "shell_password", "Unlock code", value = ""
                                                                    ),
                                                                    textOutput("unlock_message"
                                                                    )
                                                             )
                                                    ),
                                                    fluidRow(style = "padding-top: 0px; padding-left: 15px; padding-right: 15px;", 
                                                             column(width = 12,
                                                                    style="padding:0px",
                                                                    tabBox(width = 12,
                                                                           tabPanel("Dev",
                                                                                    textOutput("dev_access"
                                                                                    )
                                                                           ),
                                                                           tabPanel("Log",
                                                                                    value = "log",
                                                                                    uiOutput("log_contents")
                                                                                    
                                                                           ),
                                                                           tabPanel("Shell",
                                                                                    value = "shell",
                                                                                    searchInput(inputId = "shell_input", ">_"
                                                                                    ),
                                                                                    verbatimTextOutput("shell_output"
                                                                                    )
                                                                                    
                                                                           )
                                                                    )
                                                             )
                                                    )
                                            )
                                          )
                    ),
                    
                    dashboardPage(
                      header = header,
                      sidebar = sidebar,
                      body = body,
                    )
)
