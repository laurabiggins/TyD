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
  dom = "ftip"
)

create_table_UI <- function(tableoutputID, valid_dataset, msg=NULL){
  if(!valid_dataset){
    p(class="load_ds_msg", "Load a dataset first, go to the 'Original data' tab")
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
      dom = "ftlip", 
      scrollX = TRUE, 
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
