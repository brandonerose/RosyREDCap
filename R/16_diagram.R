#' @import RosyUtils
#' @title create_node_edge_REDCap
#' @inheritParams save_DB
#' @return messages for confirmation
#' @export
create_node_edge_REDCap <- function(DB){
  node_df <- NULL
  edge_df <- NULL
  if(DB$redcap$has_arms){
  }
  if(DB$redcap$is_longitudinal){
  }
  #instruments-----------
  instruments <- DB$redcap$instruments
  x_nrow<- nrow(instruments)
  node_df <- node_df %>% dplyr::bind_rows(
    data.frame(
      id = NA,
      type = "instrument",
      raw = instruments$instrument_name,
      label = instruments$instrument_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
      shape = "rectangle",
      style = "filled",
      color = "#FF474C"
    )
  )
  node_df$id <- 1:nrow(node_df)
  #variables -----------
  metadata <- DB$redcap$metadata
  x_nrow<- nrow(metadata)
  node_df <- node_df %>% dplyr::bind_rows(
    data.frame(
      id = NA,
      type = "variable",
      raw = metadata$field_name,
      label = metadata$field_label %>% stringr::str_replace_all( "[^[:alnum:]]", ""),
      shape = "circle",
      style = "filled",
      color = "#FF474C"
    )
  )
  node_df$id <- 1:nrow(node_df)
  sub_node_df <- node_df[which(node_df$type=="variable"),]
  edge_df <- edge_df %>% dplyr::bind_rows(
    data.frame(
      id = NA,
      from = sub_node_df$id,
      to = field_names_to_instruments(DB,field_names = sub_node_df$raw, only_unique = F) %>% sapply(function(ins){node_df$id[which(node_df$type=="instrument"&node_df$raw==ins)]}),
      rel = "Belongs to",
      style = "filled",
      color = "#FF474C"
    )
  )
  edge_df$id <- 1:nrow(edge_df)
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
