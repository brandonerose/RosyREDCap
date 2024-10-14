#' @import RosyUtils
#' @import RosyDB
#' @import RosyApp
#' @title create_node_edge_REDCap
#' @return messages for confirmation
#' @export
create_node_edge_REDCap <- function(
    DB,
    duplicate_forms = T,
    include_fields = F,
    include_choices = F
){
  # setup ==========================
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
      group = "project",
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
        group = "arm",
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
        group = "event",
        entity_name = events$unique_event_name,
        entity_label = events$event_name,
        # label = forms$form_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
        level = level,
        shape = "box", # entity
        style = "filled",
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
  #       group = "structure",
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
        group = "form",
        entity_name = event_mapping$form,
        entity_label = forms$form_label[match(event_mapping$form,forms$form_name)],# turn to function
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
        group = "form",
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
        group = "field",
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
          group = "choice",
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
        from = node_df$id[which(node_df$group=="project")],
        to = node_df$id[which(node_df$group=="form")],
        rel = NA,#"Belongs to",
        style = "filled",
        color = font.color,
        arrowhead = "none",
        arrows = arrow_type
      )
    )
    # if(DB$redcap$has_repeating_forms){
    #   sub_node_df_structure <- node_df[which(node_df$group=="structure"),]
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
        from = node_df$id[which(node_df$group=="project")],
        to = node_df$id[which(node_df$group=="arm")],
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
        from = events$arm_num %>% sapply(function(x){node_df$id[which(node_df$group=="arm"&node_df$entity_name==x)]}),
        to = events$unique_event_name %>% sapply(function(x){node_df$id[which(node_df$group=="event"&node_df$entity_name==x)]}),
        rel = NA,#"Belongs to",
        style = "filled",
        color = font.color,
        arrowhead = "none",
        arrows = arrow_type
      )
    )
    # events to forms ----------------------
    if(duplicate_forms){
      sub_node_df <- node_df[which(node_df$group=="form"),]
      if(any(!sub_node_df$entity_name %in% event_mapping$form))stop("event match error! check the diagram function. For now do not duplicate forms.")
      edge_df <- edge_df %>% dplyr::bind_rows(
        data.frame(
          id = NA,
          from = event_mapping$unique_event_name %>% sapply(function(x){node_df$id[which(node_df$group=="event"&node_df$entity_name==x)]}),
          to = sub_node_df$id,
          rel = NA,#"Belongs to",
          style = "filled",
          color = font.color,
          arrowhead = "none",
          arrows = arrow_type
        )
      )
    }else{
      edge_df <- edge_df %>% dplyr::bind_rows(
        data.frame(
          id = NA,
          from = event_mapping$unique_event_name %>% sapply(function(x){node_df$id[which(node_df$group=="event"&node_df$entity_name==x)]}),
          to = event_mapping$form %>% sapply(function(x){node_df$id[which(node_df$group=="form"&node_df$entity_name==x)]}),
          rel = NA,#"Belongs to",
          style = "filled",
          color = font.color,
          arrowhead = "none",
          arrows = arrow_type
        )
      )
    }
  }
  # forms to fields --------------
  sub_node_df_forms <- node_df[which(node_df$group=="form"),]
  if(include_fields){
    sub_node_df_fields <- node_df[which(node_df$group=="field"),]
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
      sub_node_df_choices <- node_df[which(node_df$group=="choice"),]
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
  OUT <- list(
    node_df = node_df,
    edge_df = edge_df
  )
  return(OUT)
}
#' @title REDCap_diagram
#' @return messages for confirmation
#' @export
REDCap_diagram <- function(DB,static = F,render = T,duplicate_forms = T, clean_names = T,include_fields = F,include_choices = F,hierarchical){
  if(is.null(DB$redcap))DB <- update_RosyREDCap(DB, metadata_only = T,save_to_dir = F)
  if(missing(hierarchical)) hierarchical <- !include_fields
  OUT <- create_node_edge_REDCap(DB,duplicate_forms = duplicate_forms,include_fields = include_fields,include_choices = include_choices)
  if(!clean_names){OUT$node_df$label <- OUT$node_df$entity_name}
  if(static){
    OUT$node_df$shape[which(OUT$node_df$shape=="box")] <- "rectangle"
    OUT$node_df$shape[which(OUT$node_df$shape=="ellipse")] <- "circle"
    colnames(OUT$node_df)[which(colnames(OUT$node_df)=="title")] <- "tooltip"
    colnames(OUT$node_df)[which(colnames(OUT$node_df)=="group")] <- "type"
    colnames(OUT$node_df)[which(colnames(OUT$node_df)=="color.border")] <- "color"
    node_df$fillcolor <- OUT$node_df$color.background
    # node_df$color.highlight <- "gold"
    OUT$node_df$tooltip <-gsub("<br>","\\\n",OUT$node_df$tooltip) %>% remove_html_tags()
    if(is_something(OUT$edge_df))colnames(OUT$edge_df)[which(colnames(OUT$edge_df)=="width")] <- "penwidth"
  }else{
    OUT$node_df$type <- OUT$node_df$group
  }
  graph <-
    DiagrammeR::create_graph(
      nodes_df =  OUT$node_df,
      edges_df = OUT$edge_df
    )
  OUT$node_df$group %>% unique()
  rendered_graph <-
    DiagrammeR::render_graph(
      graph,
      title = DB$redcap$project_info$project_title,
      output = ifelse(static,"graph","visNetwork")
    )
  if(!static){
    rendered_graph <- rendered_graph %>%
      visNetwork::visOptions(highlightNearest = TRUE, nodesIdSelection = TRUE) %>%
      visNetwork::visLegend(main = DB$redcap$project_info$project_title) %>%
      visNetwork::visLayout(hierarchical = hierarchical)
    # if(include_fields){
    #   groups <- "field"
    #   if(include_choices) groups <- groups %>% append("choice")
    #   rendered_graph <- rendered_graph %>% visNetwork::visClusteringByGroup(groups = groups)
    # }
    rendered_graph$x$options$groups <- rendered_graph$x$groups %>% sapply(function(group){
      list(
        shape=OUT$node_df$shape[which(OUT$node_df$group==group)[[1]]],
        color = list(
          background = OUT$node_df$color.background[which(OUT$node_df$group==group)[[1]]],
          border = OUT$node_df$color.border[which(OUT$node_df$group==group)[[1]]]
        )
      )
    },simplify = F)
  }
  if(render) return(rendered_graph)
  return(graph)
}
