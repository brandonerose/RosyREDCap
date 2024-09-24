#' @import RosyUtils
#' @import RosyDB
#' @import RosyApp
#' @title create_node_edge_REDCap
#' @return messages for confirmation
#' @export
create_node_edge_REDCap <- function(DB, include_vars = F,type = "DiagrammeR", duplicate_forms = T, clean_name = T){
  is_DiagrammeR <- type =="DiagrammeR"
  is_visNetwork <- type =="visNetwork"
  node_df <- NULL
  edge_df <- NULL
  instruments <- DB$metadata$forms
  fontcolor <- "black"
  instrument_color <- "#FF474C"
  if( ! DB$redcap$is_longitudinal){
    node_df <- node_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        type = "instrument",
        label = instruments$instrument_name,
        # label = instruments$instrument_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
        title = instruments$instrument_name %>% sapply(function(x){
          paste0("<p><b>",x,"</b><br>",paste0(RosyREDCap:::instruments_to_field_names(x,DB),collapse = "<br>"),"</p>")
        }),
        shape = "box", # entity
        style = "filled",
        color = instrument_color,
        fillcolor = instrument_color,
        fontcolor = fontcolor
      )
    )
    node_df$id <- 1:nrow(node_df)
  }
  # events-----------
  if(DB$redcap$is_longitudinal){
    arms <- DB$metadata$arms
    # arms$name <- arms$arm_num %>% paste0(". ",arms$name)
    events <- DB$metadata$events
    event_mapping <- DB$metadata$event_mapping
    node_df <- node_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        type = "arm",
        label = arms$arm_num,
        # label = instruments$instrument_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
        shape = "box", # entity
        style = "filled",
        fillcolor = "green",
        fontcolor = fontcolor
      )
    )
    node_df$id <- 1:nrow(node_df)
    node_df <- node_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        type = "event",
        label = events$unique_event_name,
        # label = instruments$instrument_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
        shape = "box", # entity
        style = "filled",
        fillcolor = "orange",
        fontcolor = fontcolor
      )
    )
    node_df$id <- 1:nrow(node_df)
    if(duplicate_forms){
      node_df <- node_df %>% dplyr::bind_rows(
        data.frame(
          id = NA,
          type = "instrument",
          label = event_mapping$form,
          # label = instruments$instrument_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
          title = event_mapping$form %>% sapply(function(x){
            paste0("<p><b>",x,"</b><br>",paste0(RosyREDCap:::instruments_to_field_names(x,DB),collapse = "<br>"),"</p>")
          }),
          shape = "box", # entity
          style = "filled",
          color = instrument_color,
          fillcolor = instrument_color,
          fontcolor = fontcolor
        )
      )
      node_df$id <- 1:nrow(node_df)
      sub_node_df <- node_df[which(node_df$type=="instrument"),]
      if(any(sub_node_df$label != event_mapping$form))stop("event match error! check the diagram function. For now do not duplicate forms.")
      edge_df <- edge_df %>% dplyr::bind_rows(
        data.frame(
          id = NA,
          from = event_mapping$unique_event_name %>% sapply(function(x){node_df$id[which(node_df$type=="event"&node_df$label==x)]}),
          to = sub_node_df$id,
          rel = NA,#"Belongs to",
          style = "filled",
          color = fontcolor,
          arrowhead = "none",
          arrows = ""
        )
      )
      edge_df$id <- 1:nrow(edge_df)
    }else{
      node_df <- node_df %>% dplyr::bind_rows(
        data.frame(
          id = NA,
          type = "instrument",
          label = instruments$instrument_name,
          # label = instruments$instrument_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
          title = instruments$instrument_name %>% sapply(function(x){
            paste0("<p><b>",x,"</b><br>",paste0(RosyREDCap:::instruments_to_field_names(x,DB),collapse = "<br>"),"</p>")
          }),
          shape = "box", # entity
          style = "filled",
          color = instrument_color,
          fillcolor = instrument_color,
          fontcolor = fontcolor
        )
      )
      node_df$id <- 1:nrow(node_df)
      edge_df <- edge_df %>% dplyr::bind_rows(
        data.frame(
          id = NA,
          from = event_mapping$unique_event_name %>% sapply(function(x){node_df$id[which(node_df$type=="event"&node_df$label==x)]}),
          to = event_mapping$form %>% sapply(function(x){node_df$id[which(node_df$type=="instrument"&node_df$label==x)]}),
          rel = NA,#"Belongs to",
          style = "filled",
          color = fontcolor,
          arrowhead = "none",
          arrows = ""
        )
      )
      edge_df$id <- 1:nrow(edge_df)
    }
    edge_df <- edge_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        from = events$unique_event_name %>% sapply(function(x){node_df$id[which(node_df$type=="event"&node_df$label==x)]}),
        to = events$arm_num %>% sapply(function(x){node_df$id[which(node_df$type=="arm"&node_df$label==x)]}),
        rel = NA,#"Belongs to",
        style = "filled",
        color = fontcolor,
        arrowhead = "none",
        arrows = ""
      )
    )
    edge_df$id <- 1:nrow(edge_df)
  }else{
    #structure ------------
    if(DB$redcap$has_repeating_instruments){
      node_df <- node_df %>% dplyr::bind_rows(
        data.frame(
          id = NA,
          type = "structure",
          label = c("Repeating","Not Repeating"),
          # label = instruments$instrument_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
          shape = "circle", # attribute
          style = "filled",
          fillcolor = "green",
          fontcolor = fontcolor
        )
      )
      node_df$id <- 1:nrow(node_df)
      sub_node_df <- node_df[which(node_df$type=="instrument"),]
      edge_df <- edge_df %>% dplyr::bind_rows(
        data.frame(
          id = NA,
          from = sub_node_df$id,
          to = ifelse(instruments$repeating[match(sub_node_df$label,instruments$instrument_name)],"Repeating","Not Repeating")%>%
            sapply(function(x){node_df$id[which(node_df$type=="structure"&node_df$label==x)]}),
          rel = NA,#"Belongs to",
          style = "filled",
          color = fontcolor,
          arrowhead = "none",
          arrows = ""
        )
      )
      edge_df$id <- 1:nrow(edge_df)
    }
  }
  # variables -----------
  if(include_vars){
    metadata <- DB$metadata$fields
    sub_node_df <- node_df[which(node_df$type=="instrument"),]
    x <- NULL
    for (i in 1:nrow(sub_node_df)){#i <- 1:nrow(sub_node_df) %>% sample(1)
      metadata_sub <- metadata$field_name[which(metadata$form_name == sub_node_df$label[i])]
      node_id_sub <- (1:length(metadata_sub)) + max(node_df$id)
      node_df <- node_df %>% dplyr::bind_rows(
        data.frame(
          id = node_id_sub,
          type = "variable",
          label = metadata_sub,
          # label = metadata$field_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
          title = metadata_sub %>% sapply(function(x){
            ROW <- which(metadata$field_name==x)
            return(paste0("<p><b>",x,"</b><br>",paste0("<b>Field Label:</b> ",metadata$field_label[ROW]),"<br>",paste0("<b>Field Type:</b> ",metadata$field_type[ROW]),"</p>"))
          }),
          shape = "ellipse",
          style = "filled",
          fillcolor = "yellow",
          fontcolor = fontcolor
        )
      )
      edge_df <- edge_df %>% dplyr::bind_rows(
        data.frame(
          id = NA,
          from = sub_node_df$id[i],
          to = node_id_sub,
          rel = NA,#"Belongs to",
          style = "filled",
          color = fontcolor,
          arrowhead = "none",
          arrows = ""
        )
      )
    }
    edge_df$id <- 1:nrow(edge_df)
  }
  node_df$fixedsize <- F
  node_df$color.border <- "black"
  # clean names -----------
  if(clean_name){
    types <- node_df$type %>% unique()
    for(type in types){#type <- types %>% sample1()
      ROWS <- which(node_df$type == type)
      if (type == "instrument"){
        node_df$label[ROWS] <- node_df$label[ROWS] %>% sapply(function(x){
          y <- instruments$instrument_label[instruments$instrument_name==x]
          return(ifelse(is.na(y),x,y))
        })
      }
      if (type == "arm"){
        node_df$label[ROWS] <- node_df$label[ROWS] %>% sapply(function(x){
          y <- arms$name[arms$arm_num==x]
          return(ifelse(is.na(y),x,y))
        })
      }
      # if (type == "variable"){
      #   node_df$label[ROWS] <- node_df$label[ROWS] %>% sapply(function(x){
      #     y <- metadata$field_label[metadata$field_name==x]
      #     return(ifelse(is.na(y),x,y))
      #   })
      # }
      if (type == "event"){
        node_df$label[ROWS] <- node_df$label[ROWS] %>% sapply(function(x){
          y <- events$event_name[events$unique_event_name==x]
          return(ifelse(is.na(y),x,y))
        })
      }
    }
  }
  # out -----------------
  # if(is_visNetwork){
  #   node_df$shape[which(node_df$shape=="rectangle")] <- "box"
  #   node_df$shape[which(node_df$shape=="ellipse")] <- "ellipse"
  #   colnames(node_df)[which(colnames(node_df)=="tooltip")] <- "title"
  #   node_df$color <- node_df$fillcolor
  #   node_df$color.highlight <- "gold"
  # }
  if(is_DiagrammeR){
    node_df$shape[which(node_df$shape=="box")] <- "rectangle"
    node_df$shape[which(node_df$shape=="ellipse")] <- "circle"
    colnames(node_df)[which(colnames(node_df)=="title")] <- "tooltip"
    # node_df$color <- node_df$fillcolor
    # node_df$color.highlight <- "gold"
    node_df$color <- node_df$color.border
    node_df$tooltip <-gsub("<br>","\\\n",node_df$tooltip) %>% remove_html_tags()
    colnames(edge_df)[which(colnames(edge_df)=="width")] <- "penwidth"
  }
  OUT <- list(
    node_df = node_df,
    edge_df = edge_df
  )
  return(OUT)
}
#' @title REDCap_diagram
#' @return messages for confirmation
#' @export
REDCap_diagram <- function(DB, render = T, include_vars = F,type = "visNetwork",duplicate_forms = T, clean_name = T){
  OUT <- create_node_edge_REDCap(DB,include_vars = include_vars,type = type, duplicate_forms = duplicate_forms,clean_name = clean_name)
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
