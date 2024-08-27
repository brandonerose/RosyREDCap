#' @import RosyUtils
#' @title create_node_edge_REDCap
#' @inheritParams save_DB
#' @return messages for confirmation
#' @export
create_node_edge_REDCap <- function(DB, include_vars = F,type = "DiagrammeR"){
  is_DiagrammeR <- type =="DiagrammeR"
  is_visNetwork <- type =="visNetwork"
  node_df <- NULL
  edge_df <- NULL
  if(DB$redcap$has_arms_that_matter){
  }
  #instruments-----------
  instruments <- DB$redcap$instruments
  node_df <- node_df %>% dplyr::bind_rows(
    data.frame(
      id = NA,
      type = "instrument",
      label = instruments$instrument_name,
      # label = instruments$instrument_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
      tooltip = instruments$instrument_name %>% sapply(function(x){
        paste0("<p><b>",x,"</b><br>",paste0(RosyREDCap:::instruments_to_field_names(x,DB),collapse = "<br>"),"</p>")
      }),
      shape = "rectangle", # entity
      style = "filled",
      color = "#FF474C",
      fillcolor = "#FF474C"
    )
  )
  node_df$id <- 1:nrow(node_df)
  sub_node_df <- node_df[which(node_df$type=="instrument"),]
  # events-----------
  if(DB$redcap$is_longitudinal){
    arms <- DB$redcap$arms
    # arms$name <- arms$arm_num %>% paste0(". ",arms$name)
    events <- DB$redcap$events
    event_mapping <- DB$redcap$event_mapping
    node_df <- node_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        type = "arm",
        label = arms$arm_num,
        # label = instruments$instrument_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
        shape = "rectangle", # entity
        style = "filled",
        color = "black",
        fillcolor = "green"
      )
    )
    node_df$id <- 1:nrow(node_df)
    node_df <- node_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        type = "event",
        label = events$unique_event_name,
        # label = instruments$instrument_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
        shape = "rectangle", # entity
        style = "filled",
        color = "orange",
        fillcolor = "orange"
      )
    )
    node_df$id <- 1:nrow(node_df)
    sub_node_df <- node_df[which(node_df$type=="event"),]
    edge_df <- edge_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        from = event_mapping$unique_event_name %>% sapply(function(x){node_df$id[which(node_df$type=="event"&node_df$label==x)]}),
        to = event_mapping$form %>% sapply(function(x){node_df$id[which(node_df$type=="instrument"&node_df$label==x)]}),
        rel = NA,#"Belongs to",
        style = "filled",
        color = "#FF474C",
        arrowhead = "none",
        arrows = ""
      )
    )
    edge_df$id <- 1:nrow(edge_df)
    edge_df <- edge_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        from = events$unique_event_name %>% sapply(function(x){node_df$id[which(node_df$type=="event"&node_df$label==x)]}),
        to = events$arm_num %>% sapply(function(x){node_df$id[which(node_df$type=="arm"&node_df$label==x)]}),
        rel = NA,#"Belongs to",
        style = "filled",
        color = "#FF474C",
        arrowhead = "none",
        arrows = ""
      )
    )
    edge_df$id <- 1:nrow(edge_df)
  }else{
    #structure ------------
    node_df <- node_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        type = "structure",
        label = c("Not Repeating","Repeating"),
        # label = instruments$instrument_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
        shape = "circle", # attribute
        style = "filled",
        color = "#FF474C",
        fillcolor = "#FF474C"
      )
    )
    node_df$id <- 1:nrow(node_df)
    edge_df <- edge_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        from = sub_node_df$id,
        to = ifelse(instruments$repeating[match(sub_node_df$label,instruments$instrument_name)],"Repeating","Not Repeating")%>%
          sapply(function(x){node_df$id[which(node_df$type=="structure"&node_df$label==x)]}),
        rel = NA,#"Belongs to",
        style = "filled",
        color = "#FF474C",
        arrowhead = "none",
        arrows = ""
      )
    )
    edge_df$id <- 1:nrow(edge_df)
  }
  #variables -----------
  if(include_vars){
    metadata <- DB$redcap$metadata
    node_df <- node_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        type = "variable",
        label = metadata$field_name,
        # label = metadata$field_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
        tooltip = metadata$field_name %>% sapply(function(x){
          ROW <- which(metadata$field_name==x)
          return(paste0("<p><b>",x,"</b><br>",paste0("Field Type: ",metadata$field_type[ROW],collapse = "<br>"),"</p>"))
        }),
        shape = "circle",
        style = "filled",
        color = "#FF474C",
        fillcolor = "#FF474C"
      )
    )
    node_df$id <- 1:nrow(node_df)
    sub_node_df <- node_df[which(node_df$type=="variable"),]
    edge_df <- edge_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        from = sub_node_df$id,
        to = field_names_to_instruments(DB,field_names = sub_node_df$label, only_unique = F) %>% sapply(function(x){node_df$id[which(node_df$type=="instrument"&node_df$label==x)]}),
        rel = NA,#"Belongs to",
        style = "filled",
        color = "#FF474C",
        arrowhead = "none",
        arrows = ""
      )
    )
    edge_df$id <- 1:nrow(edge_df)
  }
  node_df$fixedsize <- F
  # out -----------------
  if(is_visNetwork){
    node_df$shape[which(node_df$shape=="rectangle")] <- "box"
    node_df$shape[which(node_df$shape=="circle")] <- "ellipse"
    colnames(node_df)[which(colnames(node_df)=="tooltip")] <- "title"
  }
  if(is_DiagrammeR){
    node_df$tooltip <-gsub("<br>","\\\n",node_df$tooltip) %>% remove_html_tags()
  }
  OUT <- list(
    node_df = node_df,
    edge_df = edge_df
  )
  return(OUT)
  # node_aes(
  #   shape = NULL,
  #   style = NULL,
  #   penwidth = NULL,
  #   color = NULL,
  #   fillcolor = NULL,
  #   image = NULL,
  #   fontname = NULL,
  #   fontsize = NULL,
  #   fontcolor = NULL,
  #   peripheries = NULL,
  #   height = NULL,
  #   width = NULL,
  #   x = NULL,
  #   y = NULL,
  #   group = NULL,
  #   tooltip = NULL,
  #   xlabel = NULL,
  #   URL = NULL,
  #   sides = NULL,
  #   orientation = NULL,
  #   skew = NULL,
  #   distortion = NULL,
  #   gradientangle = NULL,
  #   fixedsize = NULL,
  #   labelloc = NULL,
  #   margin = NULL
  # )
  # edge_aes(
  #   style = NULL,
  #   penwidth = NULL,
  #   color = NULL,
  #   arrowsize = NULL,
  #   arrowhead = NULL,
  #   arrowtail = NULL,
  #   fontname = NULL,
  #   fontsize = NULL,
  #   fontcolor = NULL,
  #   len = NULL,
  #   tooltip = NULL,
  #   URL = NULL,
  #   label = NULL,
  #   labelfontname = NULL,
  #   labelfontsize = NULL,
  #   labelfontcolor = NULL,
  #   labeltooltip = NULL,
  #   labelURL = NULL,
  #   edgetooltip = NULL,
  #   edgeURL = NULL,
  #   dir = NULL,
  #   headtooltip = NULL,
  #   headURL = NULL,
  #   headclip = NULL,
  #   headlabel = NULL,
  #   headport = NULL,
  #   tailtooltip = NULL,
  #   tailURL = NULL,
  #   tailclip = NULL,
  #   taillabel = NULL,
  #   tailport = NULL,
  #   decorate = NULL
  # )
}
#' @title REDCap_diagram
#' @inheritParams save_DB
#' @return messages for confirmation
#' @export
REDCap_diagram <- function(DB, include_vars = F,type = "visNetwork",render = T){
  OUT <- create_node_edge_REDCap(DB,include_vars = include_vars,type = type)
  if(type == "DiagrammeR")type <- "graph"
  graph <-
    DiagrammeR::create_graph(
      nodes_df =  OUT$node_df,
      edges_df = OUT$edge_df
    )
  rendered_graph <-
    DiagrammeR::render_graph(
      graph,
      title = DB$redcap$project_info$project_title,
      output = type,
      layout = "nicely"
    )
  # rendered_graph$x$options$edges$arrows$to$enabled <- F
  if(render) return(rendered_graph)
  return(graph)
}
