library(shiny)

hide_download_btns <- function(){
  shinyjs::hide("download_grouped")
  shinyjs::hide("download_column")
  shinyjs::hide("download_transposed")
  shinyjs::hide("download_contingency")
}

show_download_btns <- function(){
  shinyjs::show("download_grouped")
  shinyjs::show("download_column")
  shinyjs::show("download_transposed")
  shinyjs::show("download_contingency")
}

dt_options <- list(
  scrollX = TRUE,
  autoWidth = FALSE,
  pageLength = 15,
  dom = "tip"
)

create_table_UI <- function(tableoutputID, valid_dataset, min_no_col=2, actual_no_col=2, msg=NULL){
  if(!valid_dataset){
    p(class="load_ds_msg", "Load a dataset first, go to the 'Original data' tab")
  } else if (actual_no_col < min_no_col) {
    p(class="load_ds_msg", 
      paste("This format requires a minimum of",  min_no_col, 
            "columns in the uploaded dataset, the current dataset has", actual_no_col)
    )
  } else {
    verticalLayout(
      DT::dataTableOutput(outputId = tableoutputID),
      p(msg, class="dataformat_msg")
    )
  }
}

show_data_table <- function(dataset){
  DT::datatable(
    dataset,
    rownames = FALSE,
    options = list(
      #dom = "ftlip", 
      dom = "tip", 
      scrollX = TRUE,
      scrollY = TRUE,
      autoWidth = FALSE
    )
  ) %>% 
    DT::formatStyle(0, target = 'row', `font-size` = '90%', lineHeight = '80%') 
}

create_download_handler <- function(dataset, dataset_name){
  downloadHandler(
    filename = function() {
      paste0(dataset_name,".csv")
    },
    content = function(file) {
      readr::write_csv(x = dataset, file)
    }
  )
}

# functions to create the different format types

create_column_output <- function(tbl, grouping_var, data_values){
  
  tbl |>
    dplyr::select(.data[[grouping_var]], .data[[data_values]]) |>
    dplyr::group_by(.data[[grouping_var]]) |>
    dplyr::mutate(id = dplyr::row_number()) |>
    dplyr::ungroup() |>
    tidyr::pivot_wider(names_from = .data[[grouping_var]], values_from = .data[[data_values]]) |>
    dplyr::select(-id)
}

create_grouped_output <- function(tbl, row_names, data_values, grouping_var){
  
  if(anyDuplicated(dplyr::select(tbl, - .data[[data_values]])) > 0){
    tbl <- tbl |>
      dplyr::group_by(.data[[grouping_var]], .data[[row_names]]) |>
      dplyr::mutate(id = dplyr::row_number()) |>
      dplyr::ungroup()
    
    tidyr::pivot_wider(tbl, values_from = .data[[data_values]], names_from = c(.data[[grouping_var]], id))
  } else {
    tidyr::pivot_wider(tbl, values_from = .data[[data_values]], names_from = .data[[grouping_var]])
  }
}
