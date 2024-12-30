#' @title Get your REDCap projects used by RosyREDCap
#' @description
#' Everytime a setup or update is performed RosyREDCap stores the most basic information
#' about that project to the cache so the user has a running log of everywhere there project information is stored,
#' which can be used to find, move, edit, delete that data.
#' @return data.frame of projects from the cache
#' @family Project Cache Functions
#' @keywords Project Cache Functions
#' @export
get_projects <- function(){
  does_exist <- cache_projects_exists()
  is_ok <- F
  if(does_exist){
    projects <- cache_path() %>% file.path("projects.rds") %>% readRDS()
    if( ! does_exist) message("You have no projects cached. Try `setup_DB()`")
    is_ok <- all(colnames(blank_project() %in% colnames(projects)))
    if( ! is_ok)cache_clear()
  }
  if(!does_exist||!is_ok) return(blank_project())
  return(projects)
}
#' @title project_health_check
#' @description
#' Check directory, DB object, and REDCap token. Optional update.
#' @family Project Cache Functions
#' @keywords Project Cache Functions
#' @export
project_health_check <- function(update = F){
  # projects <- projects_old <- get_projects()
  # DROPS <- NULL
  # projects_old$test_dir <- F
  # projects$test_DB <- F
  # projects$test_RC <- F
  # if(nrow(projects)>0){
  #   # DROPS<- projects[which(is.na(projects$project_id)),]
  #   for(i in 1:nrow(projects_old)){#i <- 1:nrow(projects)%>%  sample1()
  #     OUT <- NULL
  #     OUT <- projects_old[i,]
  #     if(file.exists(OUT$dir_path)){
  #       OUT$test_dir <- T
  #       DB <- tryCatch({
  #         load_DB(OUT$dir_path)
  #       },error = function(e) {NULL})
  #       OUT$test_DB <- !is.null(DB)
  #       if(!OUT$test_DB){
  #         DB <- tryCatch({
  #           setup_DB(
  #             short_name = OUT$short_name,
  #             dir_path = OUT$dir_path,
  #             token_name = OUT$token_name,
  #             redcap_base = "https://redcap.miami.edu/",
  #             force = T,
  #             merge_form_name = "merged"
  #           )
  #         },error = function(e) {NULL})
  #         OUT$test_DB <- !is.null(DB)
  #       }
  #       if(OUT$test_DB){
  #         OUT$test_RC <- redcap_token_works(DB)
  #         if(OUT$test_RC){
  #           if(update)DB <- update_DB(DB)
  #         }
  #       }
  #       if(OUT$test_DB){
  #         OUT_DB <- extract_project_details(DB = DB)
  #         OUT_DB$test_dir <- OUT$test_dir
  #         OUT_DB$test_DB <- OUT$test_DB
  #         OUT_DB$test_RC <- OUT$test_RC
  #         OUT <- OUT_DB
  #       }
  #       projects <- projects[which(projects$short_name!=OUT$short_name),]
  #       projects <- projects %>% dplyr::bind_rows(OUT)
  #     }
  #   }
  #   save_projects_to_cache(projects,silent = F)
  # }
}
blank_project_cols <- function(){
  c(
    "short_name",
    "dir_path",
    "last_save",
    "last_metadata_update",
    "last_data_update",
    "version",
    "token_name",
    "project_id",
    "project_title",
    "id_col",
    "is_longitudinal",
    "has_repeating_forms_or_events",
    "has_multiple_arms",
    "R_object_size",
    "file_size",
    "n_records",
    "redcap_base",
    "redcap_home",
    "redcap_API_playground"
    # "test_dir"
    # "test_DB",
    # "test_RC"
  )
}
blank_project <- function(){
  x <- matrix(data = character(0),ncol = length(blank_project_cols())) %>% as.data.frame()
  colnames(x) <- blank_project_cols()
  return(x)
}
save_projects_to_cache <- function(projects,silent=T){
  projects <- projects[order(projects$short_name),]
  # projects$test_dir <- projects$test_dir %>% as.logical()
  # projects$test_DB <- projects$test_DB %>% as.logical()
  # projects$test_RC <- projects$test_RC %>% as.logical()
  saveRDS(projects, file = cache_path() %>% file.path("projects.rds"))
  if(!silent){
    bullet_in_console(
      bullet_type = "v",
      text = paste0(pkg_name, " saved ",nrow(projects)," project locations to the cache...",paste0(projects$short_name,collapse = ", "))#"   Token: ",projects$token_name,collapse = "\n"))
    )
    bullet_in_console(
      text = paste0("The cache is stored in directory on your computer. It can be found with `",pkg_name,"::cache_path()`, and cleared with `",pkg_name,"::cache_clear()`."),
      url = cache_path()
    )
  }
}
extract_project_details <- function(DB){
  OUT <- data.frame(
    short_name = DB$short_name,
    dir_path = DB$dir_path %>% is.null() %>% ifelse(NA,DB$dir_path),
    last_save = DB$internals$last_data_dir_save %>% is.null() %>% ifelse(NA,DB$internals$last_data_dir_save),
    last_metadata_update = DB$internals$last_metadata_update,
    last_data_update = DB$internals$last_data_update,
    version = DB$redcap$version,
    token_name = DB$redcap$token_name,
    project_id = DB$redcap$project_id,
    project_title = DB$redcap$project_title,
    id_col = DB$redcap$id_col,
    is_longitudinal = DB$redcap$is_longitudinal,
    has_repeating_forms_or_events = DB$redcap$has_repeating_forms_or_events,
    has_multiple_arms = DB$redcap$has_multiple_arms,
    n_records = ifelse(is.null(DB$summary$all_records[[DB$redcap$id_col]]),NA,DB$summary$all_records %>% nrow()),
    R_object_size = NA,
    file_size = NA,
    redcap_base = DB$links$redcap_base ,
    redcap_home = DB$links$redcap_home,
    redcap_API_playground =  DB$links$redcap_API_playground
  ) %>% all_character_cols()
  rownames(OUT) <- NULL
  return(OUT)
}
add_project <- function(DB,silent = T){
  projects <- get_projects()
  projects <- projects[which(projects$short_name!=DB$short_name),]
  OUT <- extract_project_details(DB = DB)
  OUT$R_object_size <- size(DB)
  OUT$file_size <- file.path(DB$dir_path,"R_objects",paste0(DB$short_name,"_RosyREDCap.rdata")) %>% file_size_mb()
  projects <- projects %>% dplyr::bind_rows(OUT)
  save_projects_to_cache(projects,silent = silent)
}
delete_project <- function(short_name){
  projects <- get_projects()
  ROW <- which(projects$short_name==short_name)
  OTHERS <- which(projects$short_name!=short_name)
  if(!is_something(ROW))message("Nothing to delete named: ",short_name) %>% return()
  projects <- projects[OTHERS,]
  message("Deleted: ",short_name)
  save_projects_to_cache(projects)
  return(projects)
}
field_colnames <- function(){
  c(
    "field_name",
    "form_name",
    "section_header",
    "field_type",
    "field_label",
    "select_choices_or_calculations",
    "field_note",
    "text_validation_type_or_show_slider_number",
    "text_validation_min",
    "text_validation_max",
    "identifier",
    "branching_logic",
    "required_field",
    "custom_alignment",
    "question_number",
    "matrix_group_name",
    "matrix_ranking",
    "field_annotation"
  )
}
form_colnames <- function(type){
  if(missing(type))type<- "default"
  if(type =="default"){
    c(
      "form_name",
      "form_label",
      "repeating",
      "repeating_via_events"
    ) %>% return()
  }
  if(type =="redcap"){
    c(
      "form_name",
      "form_label",
      "repeating",
      "repeating_via_events"
    ) %>% return()
  }
}
