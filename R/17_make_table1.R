#' @import RosyUtils
#' @title make_table1
#' @inheritParams save_DB
#' @return messages for confirmation
#' @export
make_table1<-function(DB,df,group="no_choice",variables ,render.missing = F){
  variables<-variables[variables%in%colnames(df)]
  # if(any(!x))warning(paste0(x,collapse = ", ")," <- not in the form you specified")
  if(length(variables)==0)stop("Provide variable names of at least length 1!")
  forumla <- paste0(variables, collapse = " + ")
  # caption  <- "Basic stats"
  # footnote <- "ᵃ Also known as Breslow thickness"
  if(group!="no_choice"){
    x<- DB$metadata$field_label[which(DB$metadata$field_name==group)]
    df$group <-  df[[group]] %>% factor()
    df <- df[which(!is.na(df$group)),]
    table1::label(df$group)       <- ifelse(is.na(x),group,x)
    forumla <- paste0(forumla, " | group")
  }
  forumla <- as.formula(paste0("~",forumla))
  if(render.missing){
    table1::table1(forumla,data=df,render.missing=NULL)
  }else{
    table1::table1(forumla,data=df)
  }
}
