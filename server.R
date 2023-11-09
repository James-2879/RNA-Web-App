source("global.R", local = TRUE)
source("themes.R", local = TRUE)

# for contents, look bottom-left in this panel of Rstudio IDE

server <- function(input, output, session){
  
  message(paste0("---------------- New session started [", Sys.time(),"] -----------------"))
  
  #------------------------------------ JS -------------------------------------
  
  # initialize UI
  js$sidebarState() # manually change state
  js$changeTitleVisibility() # manually change state
  shinyjs::addClass(selector = "body", class = "sidebar-collapse") # manually switch class
  onclick('switchState', js$changeTitleVisibility()) # for user switching
  
  #--------------------------------- Variables ---------------------------------
  
  values <- reactiveValues() # initialize
  values$active_filters <- c() # contains which filters are currently active
  values$boxplot_height <- 480
  
  #--------------------------------- Functions ---------------------------------
  
  `%ni%` <- Negate(`%in%`)
  
  update_theme <- function(grids, size, rotate = NULL) {
    #' Update themes for ggplots.
    #' 
    #' Changes text sizes, whether a grid is applied to the plot, and whether axis labels should be rotated.
    
    # grid lines and extra size
    if (grids == FALSE & size == FALSE) {
      theme <- theme_blank_with_legend
    } else if (grids == FALSE & size == TRUE) {
      theme <- theme_blank_with_legend_large
    } else if (grids == TRUE & size == FALSE) {
      theme <- theme_grids_with_legend
    } else if (grids == TRUE & size == TRUE) {
      theme <- theme_grids_with_legend_large
    }
    # rotate axis labels
    if (is.null(rotate) == FALSE) {
      if (rotate == TRUE) {
        element <- list(theme(axis.text.x = element_text(angle = 45, margin = margin(t=30))))
        theme <- append(theme, element)
      } else if (rotate == FALSE) {
        element <- list(theme(axis.text.x = element_text(angle = 0)))
        theme <- append(theme, element)
      }
    }
    return(theme)
  }
  
  regression_function <- function() {
    #' Apply a regression line to a correlation (scatter) ggplot.
    #'
    #' Does not include regression statistics.
    feature <- geom_smooth(method = "lm")
  }
  
  coefficients_function <- function() {
    #' Calculate correlation coefficients for bivariate scatter plot.
    #'
    #' Overlays statistic as geom onto a ggplot.
    feature <- stat_cor()
  }
  
  download_manager <- function(device, object, file_name = NULL, width = NULL, height = NULL) {
    #' Manage downloads for the majority of objects.
    #'
    #' Handles downloads for ggplots, non-ggplot objects/plots, and text objects (notably dataframes).
    #' Using Cairo for ggplots anyway may increase quality in some circumstances.
    if (is.null(file_name) == TRUE) {
      if (device != "text") {
        file_name <- paste0(Sys.Date(), "_plot.png")
      } else if (device == "text") {
        file_name <- paste0(Sys.Date(), "_data.tsv")
      }
    }
    if (is.null(width) == TRUE & device != "text") {
      width <- 1280 # random default
    }
    if (is.null(height) == TRUE & device != "text") {
      height <- 720 # random default
    }
    if (device == "ggsave") { # for plots
      output <- downloadHandler(filename = file_name,
                                content = function(file) {
                                  ggsave(
                                    filename = file,
                                    plot = object, # plot to download goes here
                                    device = "png",
                                    width = width * 5,
                                    height = height * 5,
                                    units = "px",
                                    dpi = 300
                                  )
                                  isolate(values$download_flag <- values$download_flag + 1)
                                }
      ) 
    } else if (device == "cairo") { # for plots - generally gives better resolutions
      output <- downloadHandler(filename = file_name,
                                content = function(file) {
                                  CairoPNG(
                                    filename = file,
                                    width = width * 5,
                                    height = height * 5,
                                    dpi = 300
                                  )
                                  print(object) # plot var goes in here
                                  dev.off()
                                  isolate(values$download_flag <- values$download_flag + 1)
                                }
      )
    } else if (device == "text") { # for CSVs
      output <- downloadHandler(filename = file_name,
                                content = function(file) {
                                  write_tsv(object, file) # table to download goes here
                                  isolate(values$download_flag <- values$download_flag + 1)
                                }
      )
    }
    return(output)
  }
  
  #--------------------------------- Functions for data ---------------------------------
  
  tidy_data <- eventReactive(input$search_gene, {
    #' Pull data from RDS.
    #' 
    #' Remnant from when OS was source.
    req(input$global_gene)
    if (length(input$global_gene) > 0) {
      data <- suppressWarnings({annotated_data(genes = input$global_gene, syntax = input$global_gene_type)})
      unique_ids <- data %>% 
        select(ensembl_id_long, gene_name) %>% 
        group_by(ensembl_id_long) %>% 
        unique()
      unique_ids_vec <- unique_ids %>% 
        filter(duplicated(gene_name)) %>% 
        pull(gene_name)
      if (length(unique_ids_vec) > 1) {
        sendSweetAlert(session = getDefaultReactiveDomain(),
                       title = "Notice",
                       text = tags$div(
                         p("Multiple Ensembl IDs are present for at least one of the selected genes.
                            Data for all IDs is included by default, but it is possible to display 
                            only select Ensembl IDs by adding a filter from the right-hand panel. 
                            Alternatively search by 'Ensembl ID' rather than by 'Symbol'."),
                         unique_ids %>%  datatable(options = list(dom = "t"), rownames = FALSE, class = "table")
                       ),
                       type = "info"
        )
      }
    }
    enable("tabbox") # disable UI elements until data ready
    return(data)
  })
  
  filtered_data <- eventReactive(c(input$update_filters, input$search_gene), {
    #' Filter data based on user-specified variables.
    #'
    #' Pull data from `tidy_data()` and apply filters.
    #'
    #' Uses eval() unfortunately due to filters not being hardcoded to allow
    #' for flexibility of filters depending on what metadata columns are returned
    #' with the data.
    req(input$global_gene)
    data <- tidy_data()
    # browser()
    if (is.null(values$active_filters)) {
      return(data)
    } else if (!is.null(values$active_filters)) {
      start <- Sys.time()
      for (filter in values$active_filters) {
        coltype <- data %>%
          pull(!!sym(filter)) %>%
          class()
        value <- eval(parse(text = (paste0("input$", filter)))) # contents of the reactive input
        if (coltype %in% c("numeric", "integer")) {
          min_value <- value[1]
          max_value <- value[2]
          data <- data %>%
            filter(!!sym(filter) >= min_value) %>%
            filter(!!sym(filter) <= max_value)
        } else if (coltype == "character" | coltype == "logical") {
          data <- data %>% 
            filter(!!sym(filter) %in% value)
        }
      }
      end <- Sys.time()
      print(end - start)
      return(data)
    }
  })
  
  #--------------------------------- Dynamic UI --------------------------------
  
  ##------------------------------ Custom filters ------------------------------
  
  ###------------------------------ Add filters --------------------------------
  
  observeEvent(input$add_filter,{
    #' Add filters dynamically based on user selection.
    #'
    #' Uses an alert-based interface to present to user possible variables to
    #' filter by, based on what fields are present in the data.
    #'
    #' This observe chunk is part 1 of 2. This does not add the filter, but
    #' rather passes the selection on to another observe chunk.
    if (length(input$global_gene) == 0) {
      sendSweetAlert(session = getDefaultReactiveDomain(),
                     title = "Notice",
                     text = "Select genes and 'Update' before adding filters.",
                     type = "error")
    }
    # not checking if a filter is okay before hand because of processing time etc.
    confirmSweetAlert(
      session = getDefaultReactiveDomain(),
      inputId = "filter_column_selected",
      title = "Add filter",
      text = tags$div(align = "center",
                      fluidRow(
                        column(width = 12,
                               selectizeInput("add_filters_selectize",
                                              label = "Select variable to filter by",
                                              choices = colnames(tidy_data()),
                                              selected = NULL,
                                              multiple = FALSE),
                        ),
                        style = "height: 33vh; width: 100%; margin: 30px, align: center;" 
                      ),
      ),
      type = NULL,
      allowEscapeKey = TRUE,
      cancelOnDismiss = TRUE,
      closeOnClickOutside = TRUE,
      btn_labels = c("Cancel" ,"Continue")
    )
  })
  
  observeEvent(input$filter_column_selected, {
    #' Add filters dynamically based on user selection.
    #'
    #' Chooses either a logical, numeric, or character input based on data type.
    #'
    #' This observe chunk is part 2 of 2. The input element is inserted inside
    #' a div (`tags$div`) to allow for better control and easier removal. Input
    #' elements should be edited below if required.
    if (input$filter_column_selected) {
      filter_id <- input$add_filters_selectize
      filter_div_id <- paste0(filter_id, "-div")
      if (filter_id %in% values$active_filters) {
        sendSweetAlert(session = getDefaultReactiveDomain(),
                       title = "Error",
                       text = "Filter of this type already exists",
                       type = "error")
      } else if (filter_id %ni% values$active_filters) {
        # grab unique values in the column
        unique_vals <- tidy_data() %>% 
          pull(!!sym(input$add_filters_selectize)) %>% 
          na.omit()
        # if yada yada is zero
        if (!is_empty(unique_vals)) {
          values$active_filters <- c(values$active_filters, filter_id) # add new filter to list of currently active filters
          coltype <- tidy_data() %>% # get data type to decide which input element to use
            pull(!!sym(input$add_filters_selectize)) %>% 
            class()
          if (coltype == "numeric" | coltype == "integer") { # type 1 of 3
            min <- tidy_data() %>% 
              pull(!!sym(input$add_filters_selectize)) %>% 
              min()
            max <- tidy_data() %>% 
              pull(!!sym(input$add_filters_selectize)) %>% 
              max()
            insertUI(
              selector = "#add_id",
              where = "beforeBegin",
              ui = tags$div(id = filter_div_id,
                            noUiSliderInput(
                              filter_id,
                              label = input$add_filters_selectize,
                              value = c(min, max),
                              min = min,
                              max = max,
                              step = 1)
              )
            )
            # browser()
            sendSweetAlert(session = getDefaultReactiveDomain(),
                           inputId = "type",
                           title = "Success", 
                           text = paste0("Numeric filter added for range ", min, " - ", max),
                           type = "success",
                           btn_labels = NA)
            Sys.sleep(1.25)
            closeSweetAlert(session = getDefaultReactiveDomain())
          } else if (coltype == "logical") { # type 2 of 3
            insertUI(
              selector = "#add_id",
              where = "beforeBegin",
              ui = tags$div(id = filter_div_id,
                            selectizeInput(filter_id,
                                           label = input$add_filters_selectize,
                                           choices = c("TRUE", "FALSE"),
                                           selected = NULL,
                                           multiple = FALSE)
              )
            )
            sendSweetAlert(session = getDefaultReactiveDomain(),
                           inputId = "type",
                           title = "Success", 
                           text = "Logical filter added",
                           type = "success",
                           btn_labels = NA)
            Sys.sleep(1.25)
            closeSweetAlert(session = getDefaultReactiveDomain())
          } else if (coltype == "character") { # type 3 of 3
            unique_values <- tidy_data() %>% 
              pull(!!sym(input$add_filters_selectize)) %>% 
              unique()
            insertUI(
              selector = "#add_id",
              where = "beforeBegin",
              ui = tags$div(id = filter_div_id,
                            selectizeInput(filter_id,
                                           label = input$add_filters_selectize,
                                           choices = unique_values,
                                           selected = NULL,
                                           multiple = TRUE)
              )
            )
            sendSweetAlert(session = getDefaultReactiveDomain(),
                           inputId = "type",
                           title = "Success", 
                           text = "Character filter added",
                           type = "success",
                           btn_labels = NA)
            Sys.sleep(1.25)
            closeSweetAlert(session = getDefaultReactiveDomain())
          } else { # type unknown
            sendSweetAlert(session = getDefaultReactiveDomain(),
                           inputId = "type",
                           title = "Unknown type", 
                           text = "This shouldn't happen, contact app developers.",
                           type = "error")
          }
        } else if (is_empty(unique_vals)) {
          sendSweetAlert(session = getDefaultReactiveDomain(),
                         title = "no worky", 
                         text = "This column is missing data so cannot be applied as a filter - try a different filter type.",
                         type = "error")
        }
      }
    # }
  }
})
  
  ###---------------------------- Remove filters -------------------------------
  
  observeEvent(input$remove_filter, {
    #' Remove filters based on user selection.
    #' 
    #' Uses an alert-based interface to present to user which filters are currently 
    #' active and can be removed.
    #' 
    #' This is observe chunk 1 of 2. This chunk only passes on the filter that
    #' needs to be removed.
    print(values$active_filters)
    print(str(values$active_filters))
    if (length(values$active_filters) != 0) {
      confirmSweetAlert(
        session = getDefaultReactiveDomain(),
        inputId = "filter_for_removal",
        title = "Remove filter",
        text = tags$div(align = "center",
                        fluidRow(
                          column(width = 12,
                                 selectizeInput("remove_filters_selectize",
                                                label = "Select filter to remove",
                                                choices = values$active_filters,
                                                selected = NULL,
                                                multiple = FALSE)
                          )
                        ),
                        style = "height: 25vh; width: 15vw; margin: auto"
        ),
        type = NULL,
        allowEscapeKey = TRUE,
        cancelOnDismiss = TRUE,
        closeOnClickOutside = TRUE,
        danger_mode = TRUE,
        btn_labels = c("Cancel", "Continue")
      )
    } else if (is.null(values$active_filters)) {
      sendSweetAlert(
        session = getDefaultReactiveDomain(),
        title = "Error",
        text = "No filters exist to remove.",
        type = "error"
      )
    }
  })
  
  observeEvent(input$filter_for_removal, {
    #' Remove filters based on user selection.
    #' 
    #' Remove the UI element based on selection passed from previous chunk.
    #' 
    #' This is observe chunk 2 of 2.
    if (input$filter_for_removal) {
      element <- paste0("#", input$remove_filters_selectize, "-div")
      removeUI(selector = element)
      values$active_filters <- setdiff(values$active_filters, input$remove_filters_selectize)
      sendSweetAlert(session = getDefaultReactiveDomain(),
                     title = "Success",
                     text = "Filter successfully removed.",
                     type = "success",
                     btn_labels = NA)
      Sys.sleep(1.25)
      closeSweetAlert(session = getDefaultReactiveDomain())
    }
  })
  
  #--------------------------------- Reactives ---------------------------------
  
  ##--------------------------------- Boxplot ----------------------------------
  
  boxplot <- eventReactive(c(input$search_gene, input$update_filters, input$boxplot_select_x,
                             input$boxplot_select_y, input$boxplot_select_group, input$boxplot_select_facet,
                             input$grid_lines_bxp, input$extra_size_switch_bxp, input$rotate_axis_labels_switch_bxp,
                             input$facet_number, input$scale_y_boxplot, input$expression_threshold,
                             input$threshold_switch), {
                               #' Generate a customizable boxplot.
                               #' 
                               #' Takes filtered data and applies design as specified in
                               #' `Plot Design` and `Plot Styling` boxes.
                               #' 
                               #' This is complicated. As possible plot designs are not hard-coded,
                               #' the first step is to check whether all of the inputs (x var,
                               #' grouping var and faceting var) are all okay. This mostly checks that
                               #' continuous data isn't supplied as a categorical variable and vice versa etc.
                               #' 
                               #' Some plot designs are less-than-ideal, and so the user is queried about
                               #' whether to proceed with plotting. In most cases, the user is allowed to continue,
                               #' but some combinations of variables are blocked if it is suspected that plotting
                               #' will affect app stability or just makes no sense to compute.
                               
                               data <- filtered_data()
                               
                               ### STEP 1 --------------------------------------
                               # check each user specified variable
                               bad_x <- FALSE
                               bad_y <- FALSE
                               bad_group <- FALSE
                               bad_facet <- FALSE
                               
                               #### [a] check x vars ---------------------------
                               if (input$x_switch) { # execute only if x option is enabled
                                 req(!is.null(input$boxplot_select_x))
                                 unique_x <- data %>% 
                                   pull(!!sym(input$boxplot_select_x)) %>% 
                                   unique()
                                 question_x <- if_else(length(unique_x) > 25, TRUE, FALSE)
                                 if (question_x) {
                                   confirmSweetAlert(
                                     session = getDefaultReactiveDomain(),
                                     inputId = "continue_x",
                                     title = "Warning",
                                     text = "Selected variable has more than 25 unique categories, or is continuous. For time and app stability, it is strongly recommended that you do not proceed.",
                                     type = "warning",
                                     allowEscapeKey = TRUE,
                                     cancelOnDismiss = TRUE,
                                     danger_mode = TRUE,
                                     btn_labels = c("Cancel", "Proceed"),
                                     btn_colors = c("#26C3E7", "#DE2D19")
                                   )
                                   if (!input$continue_x) {
                                     bad_x <- TRUE
                                   }
                                 }
                               }
                               
                               #### [b] check group vars -----------------------
                               req(!is.null(input$boxplot_select_group))
                               unique_group <- data %>%
                                 pull(!!sym(input$boxplot_select_group)) %>% 
                                 unique()
                               question_group <- if_else(length(unique_group) > 25, TRUE, FALSE)
                               if (question_group) {
                                 confirmSweetAlert(
                                   session = getDefaultReactiveDomain(),
                                   inputId = "continue_group",
                                   title = "Warning",
                                   text = "Selected variable has more than 25 unique categories, or is continuous. For time and app stability, it is strongly recommended that you do not proceed.",
                                   type = "warning",
                                   allowEscapeKey = TRUE,
                                   cancelOnDismiss = TRUE,
                                   danger_mode = TRUE,
                                   btn_labels = c("Cancel", "Proceed"),
                                   btn_colors = c("#26C3E7", "#DE2D19")
                                 )
                                 if (!input$continue_group) {
                                   bad_group <- TRUE
                                 }
                               }
                               
                               #### [c] check facet vars -----------------------
                               if (input$facet_switch) { # execute only if facet option is enabled
                                 req(!is.null(input$boxplot_select_facet))
                                 unique_facet <- data %>% 
                                   pull(!!sym(input$boxplot_select_facet)) %>% 
                                   unique()
                                 question_facet <- if_else(length(unique_facet) > 25, TRUE, FALSE)
                                 if (question_facet) {
                                   confirmSweetAlert(
                                     session = getDefaultReactiveDomain(),
                                     inputId = "continue_facet",
                                     title = "Warning",
                                     text = "Selected variable has more than 25 unique categories, or is continuous. For time and app stability, it is strongly recommended that you do not proceed.",
                                     type = "warning",
                                     allowEscapeKey = TRUE,
                                     cancelOnDismiss = TRUE,
                                     danger_mode = TRUE,
                                     btn_labels = c("Cancel", "Proceed"),
                                     btn_colors = c("#26C3E7", "#DE2D19")
                                   )
                                   if (!input$continue_facet) {
                                     bad_facet <- TRUE
                                   }
                                 }
                               }
                               
                               ### STEP 2 --------------------------------------
                               # check all variables are okay
                               
                               if (bad_x | bad_y | bad_group | bad_facet) {
                                 continue <- FALSE
                                 # this probably isn't necessary, but for completeness
                               } else if (!bad_x | !bad_y | !bad_group | !bad_facet) {
                                 continue <- TRUE
                               } else {
                                 sendSweetAlert(
                                   session = getDefaultReactiveDomain(),
                                   title = "Error",
                                   text = "Something went wrong with error checking",
                                   type = "error"
                                 )
                               }
                               
                               ### STEP 3 --------------------------------------
                               # define aesthetic mappings based on input
                               # this has to be like this because of
                               # quasi-quotation in tidy evaluation
                               if (continue) {
                                 if (input$x_switch & input$facet_switch) {
                                   plot <- data %>% 
                                     ggplot(mapping = aes(x = !!sym(input$boxplot_select_x),
                                                          y = !!sym(input$boxplot_select_y),
                                                          color = !!sym(input$boxplot_select_group))) +
                                     geom_boxplot() +
                                     facet_wrap(vars(!!sym(input$boxplot_select_facet)), ncol = input$facet_number, scales = values$scale_y_boxplot)
                                 } else if (input$x_switch & !input$facet_switch) {
                                   plot <- data %>% 
                                     ggplot(mapping = aes(x = !!sym(input$boxplot_select_x),
                                                          y = !!sym(input$boxplot_select_y),
                                                          color = !!sym(input$boxplot_select_group))) +
                                     geom_boxplot()
                                 } else if (!input$x_switch & input$facet_switch) {
                                   plot <- data %>% 
                                     ggplot(mapping = aes(y = !!sym(input$boxplot_select_y),
                                                          color = !!sym(input$boxplot_select_group))) +
                                     geom_boxplot() +
                                     facet_wrap(vars(!!sym(input$boxplot_select_facet)), ncol = input$facet_number, scales = values$scale_y_boxplot)
                                 } else if (!input$x_switch & !input$facet_switch) {
                                   plot <- data %>% 
                                     ggplot(mapping = aes(y = !!sym(input$boxplot_select_y),
                                                          color = !!sym(input$boxplot_select_group))) +
                                     geom_boxplot()
                                 }
                                 
                                 ### STEP 4 ------------------------------------
                                 # finalize the plot
                                 plot <- plot +
                                   theme_reactive_bxp() +
                                   theme(legend.position = "bottom")
                                 if (input$threshold_switch) {
                                   plot <- plot +
                                     geom_hline(yintercept = input$expression_threshold,
                                                linewidth = 1,
                                                color = "red"
                                     )
                                 }
                                 if (!input$x_switch) {
                                   plot <- plot +
                                     theme(axis.ticks.x = element_blank(),
                                           axis.text.x = element_blank())
                                 }
                                 # check number of facet rows to define plot height
                                 build <- ggplot_build(plot)
                                 facets <- length(levels(build$data[[1]]$PANEL))
                                 rows <- facets / input$facet_number
                                 values$boxplot_height <- ceiling(rows) * 360
                               }
                               return(plot)
                             })
  
  
  summary_bxp <- reactive({
    #' Generate summary data for the boxplot.
    #' 
    #' Uses the same variables which supply the boxplot,
    #' so stats will also automatically change based on user selection.
    selected_grouping_variables <- input$summary_grouping_subset %>% 
      unique()
    
    summary <- filtered_data() %>% 
      group_by(!!!syms(selected_grouping_variables)) %>% # convert vector of variables to symbols
      summarise("n" = n(),
                "mean" = mean(!!sym(input$boxplot_select_y)),
                "median" = median(!!sym(input$boxplot_select_y)),
                "min" = min(!!sym(input$boxplot_select_y)),
                "max" = max(!!sym(input$boxplot_select_y)),
                "q1" = quantile(!!sym(input$boxplot_select_y), probs = 0.25),
                "q3" = quantile(!!sym(input$boxplot_select_y), probs = 0.75),
                "iqr" = IQR(!!sym(input$boxplot_select_y)),
                "sd" = sd(!!sym(input$boxplot_select_y)))
    enable(id = "boxplot_data_download_div")
    return(summary)
  })
  
  ##----------------------------- Correlation plot -----------------------------
  
  cor_plot <- eventReactive(c(input$search_gene, input$update_filters,
                              input$cor_plot_gene_subset, input$cor_plot_grouping_variable,
                              input$grid_lines_bxp, input$extra_size_switch_bxp), {
                                #' Generate a customizable correlation plot.
                                #' 
                                #' User must first select subset of genes to use
                                #' if more than two have been loaded into the app.
                                #' The user must also select what variables to plot.
                                #' 
                                #' Before plotting, the variables are checked to
                                #' ensure that the user selections will not affect
                                #' app stability.
                                if (length(input$cor_plot_gene_subset) == 2) {
                                  if (input$global_gene_type == "Symbol") {
                                    data <- filtered_data() %>% 
                                      filter(gene_name %in% input$cor_plot_gene_subset) %>% 
                                      select(`gene_name`, `tpm`, !!sym(input$cor_plot_grouping_variable)) %>% 
                                      pivot_wider(names_from = `gene_name`, values_from = `tpm`) %>% 
                                      unnest(cols = c(input$cor_plot_gene_subset[1], input$cor_plot_gene_subset[2]))
                                  } else if (input$global_gene_type == "Ensembl") {
                                    data <- filtered_data() %>% 
                                      filter(ensembl_id_long %in% input$cor_plot_gene_subset) %>% 
                                      select(`ensembl_id_long`, `tpm`, !!sym(input$cor_plot_grouping_variable)) %>% 
                                      pivot_wider(names_from = `ensembl_id_long`, values_from = `tpm`) %>% 
                                      unnest(cols = c(input$cor_plot_gene_subset[1], input$cor_plot_gene_subset[2]))
                                  }
                                  # at some point I should probably give the optin to use tpm or fpkm
                                  # or just have it so the boxplot design has a (global) unit option
                                  bad_group <- FALSE
                                  unique_grouping <- data %>% 
                                    pull(!!sym(input$cor_plot_grouping_variable)) %>% 
                                    unique() %>% 
                                    na.omit()
                                  bad_group <- if_else(length(unique_grouping) > 45, TRUE, FALSE)
                                  # browser()
                                  if (bad_group) {
                                    sendSweetAlert(
                                      session = getDefaultReactiveDomain(),
                                      title = "Aborted",
                                      text = "For time and app stability, factors with more than 45 unique values will not be plotted.",
                                      type = "error",
                                      allowEscapeKey = TRUE,
                                      btn_labels = c("Okay")
                                    )
                                  } else if (is_empty(unique_grouping)) {
                                    sendSweetAlert(
                                      session = getDefaultReactiveDomain(),
                                      inputId = "continue_group",
                                      title = "Error",
                                      text = "Selected variable has no valid values - try a different grouping variable.",
                                      type = "error",
                                      allowEscapeKey = TRUE,
                                      btn_labels = c("Okay")
                                    )
                                    bad_group <- TRUE
                                  }
                                  
                                  if (!bad_group) {
                                    plot <- data %>%
                                      ggplot(mapping = aes(x = !!sym(input$cor_plot_gene_subset[1]),
                                                           y = !!sym(input$cor_plot_gene_subset[2]),
                                                           color = !!sym(input$cor_plot_grouping_variable)
                                      )) +
                                      geom_point(position = "jitter") +
                                      theme_reactive_cor() +
                                      theme(legend.position = "right") +
                                      guides(colour = guide_legend(ncol = 1)) +
                                      regression_function() +
                                      coefficients_function()
                                    enable(id = "correlation_download_div")
                                    return(plot)
                                  }
                                }
                              })
  
  ##--------------------------------- Heatmap ----------------------------------
  
  heatmap <- eventReactive(c(input$search_gene, input$update_filters, input$heatmap_scaling_switch), {
    #' Generate a heatmap.
    #' 
    #' First checks that input selections (data sizes etc.) will not break app.
    data <- filtered_data()
    values$allow_heatmap <- TRUE # make sure there are not too many samples to plot
    if (dim(data)[1] > 5000 & dim(data)[1] < 10000) {
      confirmSweetAlert(session = getDefaultReactiveDomain(),
                        inputId = "confirm_heatmap_5000",
                        title = "Warning",
                        text = "Generating a heatmap with over 5000 samples may take a while. 
                        You may want to apply further filters to the data.",
                        type = "warning",
                        closeOnClickOutside = TRUE,
                        cancelOnDismiss = TRUE
      )
      if (!input$confirm_heatmap_5000) {
        values$allow_heatmap <- FALSE
        sendSweetAlert(session = getDefaultReactiveDomain(),
                       title = "Aborted",
                       text = "",
                       type = "success",
                       btn_labels = NA
        ) 
        Sys.sleep(1.25)
        closeSweetAlert()
      }
    } else if (dim(data)[1] > 10000 & dim(data)[1] < 20000) {
      confirmSweetAlert(session = getDefaultReactiveDomain(),
                        inputId = "confirm_heatmap_10000",
                        title = "Warning",
                        text = "Generating a heatmap with over 10000 samples will take a long time. 
                        It is strongly recommended that you apply further filters to the data.",
                        type = "warning",
                        closeOnClickOutside = TRUE,
                        cancelOnDismiss = TRUE
      ) 
      if (!input$confirm_heatmap_10000) {
        values$allow_heatmap <- FALSE
        sendSweetAlert(session = getDefaultReactiveDomain(),
                       title = "Aborted",
                       text = "",
                       type = "success",
                       btn_labels = NA
        ) 
        Sys.sleep(1.25)
        closeSweetAlert()
      }
    } else if (dim(data)[1] > 20000) {
      sendSweetAlert(session = getDefaultReactiveDomain(),
                     title = "Aborted",
                     text = "Generating a heatmap with over 20000 samples requires too much 
                        processing power to perform in-app.",
                     type = "error"
      ) 
      values$allow_heatmap <- FALSE 
    }
    if (values$allow_heatmap) {
      data <- data %>%
        select(fpkm, sample_id, gene_name) %>% 
        pivot_wider(names_from = "sample_id", values_from = "fpkm") %>% 
        column_to_rownames(var = "gene_name")
      exc_genes <- data %>%
        filter(rowSums(.) == 0) %>% 
        rownames_to_column(var = "gene") %>%
        pull(gene) %>% 
        paste(., collapse = ", ")
      data <- data %>% 
        filter(rowSums(.) > 0)
      sendSweetAlert(session = getDefaultReactiveDomain(),
                     title = "Notice",
                     text = paste0("The following genes have been omitted 
                                   due to zero count values for all samples ",
                                   exc_genes, "."),
                     type = "info"
      )
      if (!input$heatmap_scaling_switch) {
        data <- data # the normal matrix
      } else if (input$heatmap_scaling_switch) {
        data <- t(scale(t(data))) # the scaled matrix 
      }
      
      plot <- Heatmap(as.matrix(data[, -1]),
                      show_column_names = FALSE,
                      heatmap_legend_param = list(
                        title = "Scaled gene expression",
                        legend_direction = "horizontal",
                        legend_width = unit(6, "cm")))
      plot <- draw(plot, heatmap_legend_side = "top")
      # maybe check if any of the data points are na or smth
      
    }
    enable(id = "heatmap_download_div")
    return(plot)
  })
  
  ##----------------------------- Full data table ------------------------------
  
  metadata_table <- reactive({
    #' Present metadata table.
    #' 
    #' Columns can be added or removed from the already-filtered data.
    data <- filtered_data() %>% 
      select(input$data_column_selection)
    enable(id = "metadata_download_div")
    return(data)
  })
  
  ##----------------------------------- Dev ------------------------------------
  
  observeEvent(input$shell_password, {
    #' Allow access to dev panel.
    #' This is internal so proper security is not a requirement!
    if (input$shell_password == "sec_pass") {
      output$unlock_message <- renderText({"Access granted."})
      shinyjs::js$enableTab("shell")
      shinyjs::js$enableTab("log")
    } else if (input$shell_password != "sec_pass") {
      output$unlock_message <- renderText({"Access denied."})
      shinyjs::js$disableTab("shell")
      shinyjs::js$disableTab("log")
    }
  })
  
  shell_output_text <- eventReactive(input$shell_input, {
    req(input$shell_input)
    output <- system(input$shell_input, intern = TRUE)
    return(output)
  })
  
  #--------------------------------- Selectize ---------------------------------
  
  observe({
    #' Update selectize inputs with available choices.
    #' 
    #' This is based on the content of the data, and decides the design of the
    #' boxplot and associated summary data.
    updateSelectizeInput(session, "boxplot_select_x", choices = colnames(tidy_data()), selected = "omicsoft_land", server = TRUE)
    updateSelectizeInput(session, "boxplot_select_y", choices = c("tpm", "fpkm"), selected = "fpkm", server = TRUE)
    updateSelectizeInput(session, "boxplot_select_group", choices = colnames(tidy_data()), selected = "gender", server = TRUE) #gross_anatomical_region
    updateSelectizeInput(session, "boxplot_select_facet", choices = colnames(tidy_data()), selected = "gene_name", server = TRUE) #gene_name
    metadata_cols <- setdiff(colnames(filtered_data()), c("tpm", "fpkm"))
    updateSelectizeInput(session, "data_column_selection", choices = colnames(tidy_data()), selected = metadata_cols, server = TRUE)
  })
  
  observe({
    #' Update boxplot summary data selectize options.
    #' 
    #' By default, all possible groupings are selected.
    updateSelectizeInput(session, "summary_grouping_subset", choices = c(input$boxplot_select_x,
                                                                         input$boxplot_select_group,
                                                                         input$boxplot_select_facet), selected = c(input$boxplot_select_x,
                                                                                                                   input$boxplot_select_group,
                                                                                                                   input$boxplot_select_facet), server = TRUE)
  })
  
  observe({
    #' Update correlation selectize functions with available inputs.
    updateSelectizeInput(session, "cor_plot_gene_subset", choices = input$global_gene, selected = NULL, server = TRUE)
    updateSelectizeInput(session, "cor_plot_grouping_variable", choices = colnames(tidy_data()), selected = "omicsoft_land", server = TRUE)
  })
  
  observeEvent(input$global_gene_type, {
    #' Update user-facing gene list based on syntax.
    if (input$global_gene_type == "Symbol") {
      updateSelectizeInput(session, "global_gene", choices = gene_names, server = TRUE)
    } else if (input$global_gene_type == "Ensembl") {
      updateSelectizeInput(session, "global_gene", choices = gene_ids, server = TRUE)
    }
  })
  
  #--------------------------------- Switches ----------------------------------
  
  observeEvent(input$x_switch, {
    #' Update the x variable options based on state.
    if (!input$x_switch) {
      updateSelectizeInput(session, "boxplot_select_x", choices = "NULL", selected = "NULL", server = TRUE)
      disable(id = "boxplot_select_x")
    } else if (input$x_switch) {
      updateSelectizeInput(session, "boxplot_select_x", choices = colnames(tidy_data()), selected = "omicsoft_land", server = TRUE)
      enable(id = "boxplot_select_x")
    }
  })
  
  # use facet variable
  observeEvent(input$facet_switch, {
    #' Update the facet variable options based on state.
    if (!input$facet_switch) {
      updateSelectizeInput(session, "boxplot_select_facet", choices = "NULL", selected = "NULL", server = TRUE)
      disable(id = "boxplot_select_facet")
      disable(id = "facet_number")
    } else if (input$facet_switch) {
      updateSelectizeInput(session, "boxplot_select_facet", choices = colnames(tidy_data()), selected = "gene_name", server = TRUE)
      enable(id = "boxplot_select_facet")
      enable(id = "facet_number")
    }
  })
  
  observeEvent(input$threshold_switch, {
    #' Update the facet variable options based on state.
    if (!input$threshold_switch) {
      disable(id = "expression_threshold")
    } else if (input$threshold_switch) {
      enable(id = "expression_threshold")
    }
  })
  
  observeEvent(input$scale_y_boxplot, {
    #' Update y-axis scaling based on state.
    if (input$scale_y_boxplot) {
      values$scale_y_boxplot <- "fixed"
    } else if (!input$scale_y_boxplot) {
      values$scale_y_boxplot <- "free"
    }
  })
  
  theme_reactive_bxp <- reactive({
    #' Update boxplot based on selections in `Plot styling` box.
    theme <- update_theme(grids = input$grid_lines_bxp,
                          size = input$extra_size_switch_bxp,
                          rotate = input$rotate_axis_labels_switch_bxp
    )
    return(theme)
  })
  theme_reactive_cor <- reactive({
    #' Update correlation plot based on selections in `Plot styling` box.
    theme <- update_theme(grids = input$grid_lines_bxp,
                          size = input$extra_size_switch_bxp
    )
    return(theme)
  })
  
  #---------------------------------- Messages ---------------------------------
  
  # feature request
  observeEvent(input$feature_request, {
    inputSweetAlert(
      session = getDefaultReactiveDomain(),
      inputId = "feature_request_input",
      title = "Feature request",
      text = "Please briefly outline the request and provide your email address.
      Feature requests will be considered on a first-come, first-serve basis.
      For high priority requests, contact the Data Science team directly.",
      input = "textarea",
      btn_labels = "Send",
      reset_input = TRUE,
      allowOutsideClick = TRUE,
      showCloseButton = TRUE
    )
  })
  observeEvent(input$feature_request_input, {
    sendmail(app_email,
             maintainer,
             "Feature request",
             input$feature_request_input,
             control=list(smtpServer= "exchange.organisation.com"))
    show_alert(
      inputId = NULL,
      title = "Request submitted",
      text = "Data Science team will be in touch shortly",
      type = "success"
    )
  })
  
  # bug report
  observeEvent(input$bug_report, {
    inputSweetAlert(
      session = getDefaultReactiveDomain(),
      inputId = "bug_report_input",
      title = "Bug report",
      text = "Please outline the issue and provide your email address.
      For high priority issues, contact the Data Science team directly.",
      HTML = T,
      input = "textarea",
      btn_labels = "Send",
      reset_input = TRUE,
      allowOutsideClick = TRUE,
      showCloseButton = TRUE
    )
  })
  observeEvent(input$bug_report_input, {
    sendmail(app_email,
             maintainer,
             "Bug report",
             input$bug_report_input,
             control=list(smtpServer= "exchange.organisation.com"))
    show_alert(
      inputId = NULL,
      title = "Report submitted",
      text = "Data Science team will look into this shortly",
      type = "success"
    )
  })
  
  # help messages
  observeEvent(input$getting_started_help, {
    sendSweetAlert(
      session = getDefaultReactiveDomain(),
      title = "Getting started",
      text = tags$div(includeMarkdown("www/help_files/getting_started_help.md")),
      type = "info"
      # width = "50%"
    )
  })
  observeEvent(input$plot_design_help, {
    sendSweetAlert(
      session = getDefaultReactiveDomain(),
      title = "Help",
      text = tags$div(includeMarkdown("www/help_files/plot_design_help.md")),
      type = "info"
    )
  })
  observeEvent(input$plot_styling_help, {
    sendSweetAlert(
      session = getDefaultReactiveDomain(),
      title = "Help",
      text = tags$div(includeMarkdown("www/help_files/plot_styling_help.md")),
      type = "info"
    )
  })
  observeEvent(input$summary_data_help, {
    sendSweetAlert(
      session = getDefaultReactiveDomain(),
      title = "Help",
      text = tags$div(includeMarkdown("www/help_files/summary_data_help.md")),
      type = "info"
    )
  })
  observeEvent(input$correlation_plot_help, {
    sendSweetAlert(
      session = getDefaultReactiveDomain(),
      title = "Help",
      text = tags$div(includeMarkdown("www/help_files/correlation_plot_help.md")),
      type = "info"
    )
  })
  observeEvent(input$heatmap_help, {
    sendSweetAlert(
      session = getDefaultReactiveDomain(),
      title = "Help",
      text = tags$div(includeMarkdown("www/help_files/heatmap_help.md")),
      type = "info"
    )
  })
  observeEvent(input$metadata_table_help, {
    sendSweetAlert(
      session = getDefaultReactiveDomain(),
      title = "Help",
      text = tags$div(includeMarkdown("www/help_files/metadata_table_help.md")),
      type = "info"
    )
  })
  
  #-------------------------------- Downloads ----------------------------------
  
  observeEvent(values$download_flag, {
    values$download_flag <- 0
    show_alert(
      inputId = "download_successful",
      title = "Download successful",
      type = "success"
    )
    output$download_boxplot <- NULL # this is to clear any cache which results in downloads
    output$download_boxplot_data <- NULL # not returning the most up-to-date version of an object
    output$download_cor_plot <- NULL
    output$download_heatmap <- NULL
    output$download_metadata_table <- NULL
    output$download_boxplot <- download_manager(object = boxplot(), device = "ggsave")
    output$download_boxplot_data <- download_manager(object = summary_bxp(), device = "text")
    output$download_cor_plot <- download_manager(object = cor_plot(), device = "ggsave")
    output$download_heatmap <- download_manager(object = heatmap(), device = "cairo")
    output$download_metadata_table <- download_manager(object = metadata_table(), device = "text")
  })
  
  output$download_boxplot <- download_manager(object = boxplot(), device = "ggsave")
  output$download_boxplot_data <- download_manager(object = summary_bxp(), device = "text")
  output$download_cor_plot <- download_manager(object = cor_plot(), device = "ggsave")
  output$download_heatmap <- download_manager(object = heatmap(), device = "cairo")
  output$download_metadata_table <- download_manager(object = metadata_table(), device = "text")
  
  #--------------------------------- Outputs -----------------------------------
  
  # plot styling options
  output$grids_bxp <- renderText({"Grid lines"})
  output$extra_size_bxp <- renderText({"Extra size"})
  output$rotate_axis_labels_bxp <- renderText({"Rotate axis labels"})
  
  # data table and plot outputs
  observe({
    #' Reactive contezt for output to allow use of reactive values.
    output$bxp <- renderPlot(boxplot(), height = values$boxplot_height)
  })
  output$bxp_summary_table <- renderTable({summary_bxp()})
  output$cor <- renderPlot(cor_plot())
  output$heatmap_plot <- renderPlot(heatmap())
  output$metadata_table <- DT::renderDataTable(metadata_table(), filter = "top")
  
  #--------------------------------- Dev & JS ----------------------------------
  
  output$dev_access <- renderText({"Code required for shell access."})
  output$log_contents <- renderUI({
    newest_log <- file.info(list.files("/var/log/shiny-server/", full.names = T)) %>% # change to env var at some point
      filter(isdir == FALSE) %>%
      rownames_to_column(var = "file_name") %>% 
      filter(str_detect(file_name, "rna-web-app")) %>% 
      arrange(desc(mtime)) %>% 
      slice(c(1)) %>%  
      pull(file_name)
    raw_text <- readLines(newest_log)
    split_text <- stringi::stri_split(str = raw_text, regex = "\\n")
    formatted_text <- lapply(split_text, p)
    return(formatted_text)
  })
  output$shell_output <- renderText({shell_output_text()})
  
  js$collapse("gene_box")
  js$collapse("filters_box")
  js$collapse("plot_design_box")
  js$collapse("plot_styling_box")
  
  observeEvent(input$search_gene, {
    #' Change box states based on input state.
    if (isFALSE(input$is_gene_box_collapsed)) {
      js$collapse("gene_box")
    }
    if (isTRUE(input$is_filters_box_collapsed)) {
      js$collapse("filters_box")
    }
    if (isTRUE(input$is_plot_design_box_collapsed)) {
      js$collapse("plot_design_box")
    }
    if (isTRUE(input$is_plot_styling_box_collapsed)) {
      js$collapse("plot_styling_box")
    }
  })
  
  shinyjs::js$disableTab("shell")
  shinyjs::js$disableTab("log")
  
  # disable stuff until plots are generated
  disable("tabbox")
  disable(id = "metadata_download_div")
  
  #--------------------------- gene upload garbage
  
  observeEvent(input$gene_list_uploaded, {
    #' Read genes from an uploaded file.
    #' 
    #' The file shape is quickly checked to see if the gene list is in a long
    #' or wide format to allow for flexibility in the way the input format. It 
    #' is also checked that a gene syntax is entered, because this was quicker
    #' than writing code that tries to infer the syntax of the uploaded genes.
    #' 
    #' It should also be noted that this code also works with `tsvs`, `txts`
    #' etc. but it makes sense to suggest the user a format they can easily
    #' create.
    req(input$gene_list_uploaded)
    values$genes_good <- FALSE
    sendSweetAlert(title = "Please wait",
                   text = "Checking file for valid genes...",
                   type = "info",
                   showCloseButton = FALSE,
                   btn_labels = NA)
    file <- input$upload_gene_list
    data <- read_csv(file$datapath) 
    # read data based on shape
    if (dim(data)[1] == 0 & dim(data)[2] > 0) {
      print("Data is wide.")
      genes <- colnames(data) %>% 
        as.vector() %>%
        lapply(toupper) %>% 
        unlist() 
      values$genes_good <- TRUE
    } else if (dim(data)[1] > 0 & dim(data)[2] == 1) {
      print("Data is long.")
      colname_gene <- colnames(data) %>%
        toupper()
      genes_from_long <- data %>% 
        pull(1) %>% 
        lapply(toupper) %>% 
        unlist()
      genes <- c(colname_gene, genes_from_long)
      values$genes_good <- TRUE
    } else {
      sendSweetAlert(title = "Data layout unreadable",
                     text = "Remove column/row names, check extension, and try again.",
                     type = "error",
                     showCloseButton = FALSE,
                     btn_labels = "Okay.")
      genes <- NULL
    }
    if (values$genes_good) {
      if (input$global_gene_type == "Symbol") {
        updateSelectizeInput(session = getDefaultReactiveDomain(), inputId = "global_gene", choices = gene_names, selected = genes, server = TRUE)
      } else if (input$global_gene_type == "Ensembl") {
        updateSelectizeInput(session = getDefaultReactiveDomain(), inputId = "global_gene", choices = gene_ids, selected = genes, server = TRUE)
      }
      remove(file) # I hope this removes the file anyway I need to check
      closeSweetAlert()
      sendSweetAlert(title = "Complete",
                     # text = "Please wait a few seconds...",
                     type = "success",
                     showCloseButton = FALSE,
                     btn_labels = NA)
      Sys.sleep(1.75)
      closeSweetAlert()
    }
  })
  
  observeEvent(input$gene_upload, {
    #' UI to upload a list of genes.
    confirmSweetAlert(
      session = getDefaultReactiveDomain(),
      inputId = "gene_list_uploaded",
      title = "Upload gene list",
      text = tags$div(align = "center",
                      fluidRow(
                        column(width = 12,
                               fileInput(
                                 inputId = "upload_gene_list",
                                 label = NULL,
                                 multiple = FALSE,
                                 accept = c(".tsv", ".csv", ".txt"),
                                 width = NULL,
                                 buttonLabel = "Browse...",
                                 placeholder = "No file selected"
                               ),
                               tags$span("Note: any genes which are not in the correct format will be ignored.")
                        ),
                        style = "width: 100%; margin: 30px, align: center;" 
                      ),
      ),
      type = NULL,
      allowEscapeKey = TRUE,
      cancelOnDismiss = TRUE,
      closeOnClickOutside = TRUE,
      btn_labels = c("Cancel" ,"Continue")
    )
  })
  
  }
