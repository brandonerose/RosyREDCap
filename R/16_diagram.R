#' @import RosyUtils
#' @import RosyDB
#' @import RosyApp
#' @title create_node_edge_REDCap
#' @return messages for confirmation
#' @export
create_node_edge_REDCap <- function(
    DB,
    type = "DiagrammeR",
    duplicate_forms = T,
    clean_name = T,
    include_fields = F,
    include_choices = F
){
  # setup ==========================
  is_DiagrammeR <- type == "DiagrammeR"
  is_visNetwork <- type == "visNetwork"
  node_df <- NULL
  edge_df <- NULL
  bordercolor <- font.color <- "black"
  project_color <- "lightblue"
  arm_color <- "green"
  event_color <- "orange"
  form_color <- "#FF474C"
  field_color <- "yellow"
  attribute_color <- "green"
  choice_color <- "lightblue"
  arrow_type <- "to"
  arms <- DB$metadata$arms
  events <- DB$metadata$events
  event_mapping <- DB$metadata$event_mapping
  forms <- DB$metadata$forms[order(DB$metadata$forms$repeating),]
  fields <- DB$metadata$fields
  choices <- DB$metadata$choices
  # nodes ======================================================================
  # project ---------------------------------------------------------
  level <- 1
  node_df <- node_df %>% dplyr::bind_rows(
    data.frame(
      id = NA,
      type = "project",
      entity_name = DB$short_name,
      entity_label = DB$redcap$project_info$project_title,
      # label = forms$form_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
      level = level,
      shape = "box", # entity
      style = "filled",
      color.background = project_color,
      color.border = bordercolor,
      font.color = font.color
    )
  )
  # arms & events -------------------------
  if(DB$redcap$is_longitudinal){
    level <- level + 1
    node_df <- node_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        type = "arm",
        entity_name = arms$arm_num,
        entity_label = arms$arm_num,
        # label = forms$form_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
        level = level,
        shape = "box", # entity
        style = "filled",
        color.background = arm_color,
        color.border = bordercolor,
        font.color = font.color
      )
    )
    level <- level + 1
    node_df <- node_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        type = "event",
        entity_name = events$unique_event_name,
        entity_label = events$unique_event_name,
        # label = forms$form_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
        level = level,
        shape = "box", # entity
        style = "filled",
        color = event_color,
        color.background = event_color,
        color.border = bordercolor,
        font.color = font.color
      )
    )
  }
  # forms -----------
  # if(DB$redcap$has_repeating_forms){
  #   level <- level + 1
  #   node_df <- node_df %>% dplyr::bind_rows(
  #     data.frame(
  #       id = NA,
  #       type = "structure",
  #       entity_name = c("Repeating","Not Repeating"),
  #       entity_label = c("Repeating","Not Repeating"),
  #       level = level,
  #       # label = forms$form_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
  #       shape = "circle", # attribute
  #       style = "filled",
  #       color.background = attribute_color,
  #       color.border = bordercolor,
  #       font.color = font.color
  #     )
  #   )
  # }
  if(duplicate_forms && DB$redcap$is_longitudinal){
    level <- level + 1
    node_df <- node_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        type = "form",
        entity_name = event_mapping$form,
        entity_label = event_mapping$form,
        level = level,
        # label = forms$form_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
        title = event_mapping$form %>% sapply(function(x){
          paste0("<p><b>",x,"</b><br>",paste0(form_names_to_field_names(x,DB),collapse = "<br>"),"</p>")
        }),
        shape = "box", # entity
        style = "filled",
        color.background = form_color,
        color.border = bordercolor,
        font.color = font.color
      )
    )
  }else{
    level <- level + 1
    node_df <- node_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        type = "form",
        entity_name = forms$form_name,
        entity_label = forms$form_label,
        level = level,
        # label = forms$form_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
        title = forms$form_name %>% sapply(function(x){
          paste0("<p><b>",x,"</b><br>",paste0(form_names_to_field_names(x,DB),collapse = "<br>"),"</p>")
        }),
        shape = "box", # entity
        style = "filled",
        color.background = form_color,
        color.border = bordercolor,
        font.color = font.color
      )
    )
  }
  # fields --------------
  if(include_fields){
    level <- level + 1
    node_df <- node_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        type = "field",
        entity_name = fields$field_name,
        entity_label = fields$field_label,
        level = level,
        # label = DB$fields$fields$field_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
        title = paste0("<p><b>",fields$field_name,"</b><br>",paste0("<b>Field Label:</b> ",fields$field_label),"<br>",paste0("<b>Field Type:</b> ",fields$field_type),"</p>"),
        shape = "ellipse",
        style = "filled",
        color.background = field_color,
        color.border = bordercolor,
        font.color = font.color
      )
    )
    if(include_choices){
      level <- level + 1
      node_df <- node_df %>% dplyr::bind_rows(
        data.frame(
          id = NA,
          type = "choice",
          entity_name = choices$name,
          entity_label = choices$name,
          level = level,
          # label = DB$fields$fields$field_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
          title = NA,
          shape = "ellipse",
          style = "filled",
          color.background = choice_color,
          color.border = bordercolor,
          font.color = font.color
        )
      )
    }
  }
  # # clean names -----------
  # if(clean_name){
  #   types <- node_df$type %>% unique()
  #   for(type in types){#type <- types %>% sample1()
  #     ROWS <- which(node_df$type == type)
  #     if (type == "form"){
  #       node_df$label[ROWS] <- node_df$label[ROWS] %>% sapply(function(x){
  #         y <- forms$form_label[forms$form_name==x]
  #         return(ifelse(is.na(y),x,y))
  #       })
  #     }
  #     if (type == "arm"){
  #       node_df$label[ROWS] <- node_df$label[ROWS] %>% sapply(function(x){
  #         y <- arms$name[arms$arm_num==x]
  #         return(ifelse(is.na(y),x,y))
  #       })
  #     }
  #     # if (type == "field"){
  #     #   node_df$label[ROWS] <- node_df$label[ROWS] %>% sapply(function(x){
  #     #     y <- DB$metadata$fields$field_label[metadata$field_name==x]
  #     #     return(ifelse(is.na(y),x,y))
  #     #   })
  #     # }
  #     if (type == "event"){
  #       node_df$label[ROWS] <- node_df$label[ROWS] %>% sapply(function(x){
  #         y <- events$event_name[events$unique_event_name==x]
  #         return(ifelse(is.na(y),x,y))
  #       })
  #     }
  #   }
  # }
  # final nodes-------------------
  node_df$id <- 1:nrow(node_df)
  node_df$fixedsize <- F
  # node_df$color.highlight <- "gold"
  node_df$label<-node_df$entity_label %>% sapply(function(text){
    wrap_text(text,25)
  })
  rownames(node_df) <- NULL
  # edges ======================
  # edges not longitudinal ---------------
  if( ! DB$redcap$is_longitudinal){
    # project to forms-------------------
    edge_df <- edge_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        from = node_df$id[which(node_df$type=="project")],
        to = node_df$id[which(node_df$type=="form")],
        rel = NA,#"Belongs to",
        style = "filled",
        color = font.color,
        arrowhead = "none",
        arrows = arrow_type
      )
    )

    # if(DB$redcap$has_repeating_forms){
    #   sub_node_df_structure <- node_df[which(node_df$type=="structure"),]
    #   edge_df <- edge_df %>% dplyr::bind_rows(
    #     data.frame(
    #       id = NA,
    #       from = sub_node_df_forms$id,
    #       to = sub_node_df_structure$id[match(ifelse(forms$repeating[match(sub_node_df_forms$entity_name,forms$form_name)],"Repeating","Not Repeating"),sub_node_df_structure$entity_name)],
    #       rel = NA,#"Belongs to",
    #       style = "filled",
    #       color = font.color,
    #       arrowhead = "none",
    #       arrows = arrow_type
    #     )
    #   )
    # }
  }
  # edges is longitudinal ---------------
  if(DB$redcap$is_longitudinal){
    # project to arms-------------------
    edge_df <- edge_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        from = node_df$id[which(node_df$type=="project")],
        to = node_df$id[which(node_df$type=="arm")],
        rel = NA,#"Belongs to",
        style = "filled",
        color = font.color,
        arrowhead = "none",
        arrows = arrow_type
      )
    )
    # arms to events --------------------
    edge_df <- edge_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        from = events$arm_num %>% sapply(function(x){node_df$id[which(node_df$type=="arm"&node_df$label==x)]}),
        to = events$unique_event_name %>% sapply(function(x){node_df$id[which(node_df$type=="event"&node_df$label==x)]}),
        rel = NA,#"Belongs to",
        style = "filled",
        color = font.color,
        arrowhead = "none",
        arrows = ""
      )
    )
    # events to forms ----------------------
    if(duplicate_forms){
      sub_node_df <- node_df[which(node_df$type=="form"),]
      if(any(sub_node_df$label != event_mapping$form))stop("event match error! check the diagram function. For now do not duplicate forms.")
      edge_df <- edge_df %>% dplyr::bind_rows(
        data.frame(
          id = NA,
          from = event_mapping$unique_event_name %>% sapply(function(x){node_df$id[which(node_df$type=="event"&node_df$label==x)]}),
          to = sub_node_df$id,
          rel = NA,#"Belongs to",
          style = "filled",
          color = font.color,
          arrowhead = "none",
          arrows = ""
        )
      )
      edge_df$id <- 1:nrow(edge_df)
    }else{
      edge_df <- edge_df %>% dplyr::bind_rows(
        data.frame(
          id = NA,
          from = event_mapping$unique_event_name %>% sapply(function(x){node_df$id[which(node_df$type=="event"&node_df$label==x)]}),
          to = event_mapping$form %>% sapply(function(x){node_df$id[which(node_df$type=="form"&node_df$label==x)]}),
          rel = NA,#"Belongs to",
          style = "filled",
          color = font.color,
          arrowhead = "none",
          arrows = ""
        )
      )
    }
  }
  # forms to fields --------------
  sub_node_df_forms <- node_df[which(node_df$type=="form"),]
  if(include_fields){
    sub_node_df_fields <- node_df[which(node_df$type=="field"),]
    edge_df <- edge_df %>% dplyr::bind_rows(
      data.frame(
        id = NA,
        from = sub_node_df_forms$id[match(fields$form_name[match(sub_node_df_fields$entity_name,fields$field_name)],sub_node_df_forms$entity_name)],
        to = sub_node_df_fields$id,
        rel = NA,#"Belongs to",
        style = "filled",
        color = font.color,
        arrowhead = "none",
        arrows = arrow_type
      )
    )
    if(include_choices){
      sub_node_df_choices <- node_df[which(node_df$type=="choice"),]
      edge_df <- edge_df %>% dplyr::bind_rows(
        data.frame(
          id = NA,
          from = sub_node_df_fields$id[match(choices$field_name,sub_node_df_fields$entity_name)],
          to = sub_node_df_choices$id,
          rel = NA,#"Belongs to",
          style = "filled",
          color = font.color,
          arrowhead = "none",
          arrows = arrow_type
        )
      )
    }
  }
  # final edges -------------------
  edge_df$id <- 1:nrow(edge_df)
  # visNetwork::visNetwork(nodes = node_df,edges = edge_df) %>% visNetwork::visLayout(hierarchical = F)
  # out -----------------
  # if(is_visNetwork){
  #   node_df$shape[which(node_df$shape=="rectangle")] <- "box"
  #   node_df$shape[which(node_df$shape=="ellipse")] <- "ellipse"
  #   colnames(node_df)[which(colnames(node_df)=="tooltip")] <- "title"
  #   node_df$color <- node_df$fillcolor
  # }
  if(is_DiagrammeR){
    node_df$shape[which(node_df$shape=="box")] <- "rectangle"
    node_df$shape[which(node_df$shape=="ellipse")] <- "circle"
    colnames(node_df)[which(colnames(node_df)=="title")] <- "tooltip"
    colnames(node_df)[which(colnames(node_df)=="color.border")] <- "color"
    # node_df$color <- node_df$fillcolor
    # node_df$color.highlight <- "gold"
    node_df$tooltip <-gsub("<br>","\\\n",node_df$tooltip) %>% remove_html_tags()
    if(is_something(edge_df))colnames(edge_df)[which(colnames(edge_df)=="width")] <- "penwidth"
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
REDCap_diagram <- function(DB, render = T, include_fields = F,type = "visNetwork",duplicate_forms = T, clean_name = T){
  if(is.null(DB$redcap))DB <- update_RosyREDCap(DB, metadata_only = T,save_to_dir = F)
  OUT <- create_node_edge_REDCap(DB,include_fields = include_fields,type = type, duplicate_forms = duplicate_forms,clean_name = clean_name)
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
