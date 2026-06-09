#' @title plotly_bar
#' @param DF data.frame
#' @param x_col column to be plotted for x
#' @param y_col column to be plotted for y
#' @param name title
#' @return plotly
#' @export
plotly_bar <- function(DF, x_col, y_col, name) {
  fig <- plotly::plot_ly(
    DF,
    x = x_col,
    y = y_col,
    type = "bar",
    name = name,
    text = x_col,
    textposition = "inside",
    insidetextanchor = "middle",
    insidetextfont = list(color = "black")
  ) |> plotly::layout(
    xaxis = list(
      title = list(
        # text=paste0(name," (",prettyNum(sum(b$n_applicants),","),")"),
        font = list(size = 12L, color = "black")
      ),
      tickfont = list(size = 12L, color = "black")
    ),
    yaxis = list(
      title = "",
      tickfont = list(size = 10L, color = "black")
    ),
    barmode = "stack",
    annotations = list(
      x = x_col,
      y = y_col,
      xanchor = "left",
      xref = "x",
      yref = "y",
      text = paste0((x_col / sum(x_col) * 100L) |> round(1L), "%"),
      showarrow = FALSE,
      arrowhead = NULL,
      arrowsize = NULL,
      font = list(size = 12L, color = "black"),
      textangle = 0L
    )
  ) |>
    plotly::config(
      scrollZoom = FALSE,
      displaylogo = FALSE,
      modeBarButtonsToRemove = c(
        "zoom2d",
        "pan2d",
        "select2d",
        "lasso2d",
        # "zoomIn2d",
        # "zoomOut2d",
        "autoScale2d",
        # "resetScale2d",
        "hoverclosest",
        "hoverCompareCartesian",
        "toggleHover"
      )
    ) |>
    plotly::layout(
      showlegend = FALSE,
      xaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = FALSE
      ),
      yaxis = list(
        showgrid = FALSE,
        zeroline = FALSE,
        showticklabels = TRUE
      )
    ) |>
    plotly::style(hoverinfo = "none")
  fig
}
#' @title plotly_parcats
#' @param DF data.frame
#' @param remove_missing logical for removing missing/NA data
#' @param line_shape_curved logical for having curved lines
#' @param numeric_to_factor logical for making numerics factors using quantiles
#' @param quantiles integer for quantiles
#' @param more_descriptive_label logical for having more descriptive labels
#' @return plotly
#' @export
plotly_parcats <- function(DF,
                           remove_missing = TRUE,
                           line_shape_curved = TRUE,
                           numeric_to_factor = TRUE,
                           quantiles = 5L,
                           more_descriptive_label = FALSE) {
  line_shape <- "linear"
  the_labels <- get_labels(DF)
  if (line_shape_curved)
    line_shape <- "hspline"
  if (remove_missing) {
    DF <- stats::na.omit(DF)
  } else {
    DF <- DF |> lapply(function(col) {
      OUT <- col
      if (is.factor(OUT))
        levels(OUT) <- c(levels(OUT), "*Missing*")
      OUT[which(is.na(OUT))] <- "*Missing*"
      OUT
    }) |>
      as.data.frame()
  }
  if (nrow(DF) == 0L) {
    return(plotly::plotly_empty())
  }
  DF <- DF |> lapply(function(col) {
    OUT <- col
    if (!is.factor(OUT)) {
      if (is.numeric(OUT)) {
        if (numeric_to_factor) {
          OUT <- OUT |> numeric_to_cats(
            method = "quantile",
            quantiles = quantiles,
            more_descriptive_label = more_descriptive_label
          )
        }
      } else {
        OUT <- as.factor(OUT)
      }
    }
    OUT
  }) |>
    as.data.frame()
  adj_margins_l <- adjust_margins(max(nchar(as.character(DF[[1L]]))))
  adj_margins_r <- adjust_margins(max(nchar(as.character(DF[[ncol(DF)]]))))
  color_palette <- RColorBrewer::brewer.pal(12L, "Paired")  # Using 3 colors for "High", "Medium", "Low"
  color_palette_vec <- color_palette |> sample(size = length(levels(DF[[1L]])), replace =  length(levels(DF[[1L]])))
  fig <- plotly::plot_ly(
    # data = DF,
    # x = x_col,
    # y = y_col,
    type = "parcats",
    dimensions = lapply(colnames(DF), function(col) {
      categoryarray <- as.character(unique(DF[[col]]))
      if (is.factor(DF[[col]])) {
        categoryarray <- categoryarray[match(levels(DF[[col]]), categoryarray)] |> drop_nas()
      }
      out_list <-  list(
        values = DF[[col]],
        label = the_labels[which(colnames(DF) == col)],
        categoryorder = "array",
        categoryarray = categoryarray |> rev()
      )
      out_list
    }),
    line = list(
      shape = line_shape,
      color = color_palette_vec[as.integer(DF[[1L]])]  # Use the numeric representation for colors
      # colorscale = colorscale,  # Define the new colorscale
      # cmin =min(as.numeric(DF[[1]])),  # Set min for color scaling
      # cmax = max(as.numeric(DF[[1]]))   # Set max for color scaling
    ),
    arrangement = "freeform",
    # name = name,
    # text=x_col,
    # textposition="inside",
    # insidetextanchor="middle",
    # insidetextfont=list(color="black"),
    labelfont = list(size = 14L, color = "black"),
    tickfont = list(size = 12L, color = "black")
  ) |>
    plotly::config(
      # scrollZoom=TRUE,
      displaylogo = FALSE
    ) |>
    plotly::layout(
      showlegend = FALSE,
      margin = list(l = adj_margins_l, r = adj_margins_r)
      #l = 100, r = 100)
    ) |>
    plotly::style(hoverinfo = "none")
  fig
}
#' @noRd
numeric_to_cats <- function(vec,
                            method = "quantile",
                            quantiles = 5L,
                            more_descriptive_label = FALSE) {
  if (method == "sd") {
    med <- stats::median(vec)
    sd_val <- stats::sd(vec)
    # Define the actual value ranges rounded to 1 decimal place
    low_threshold <- round(med - 2L * sd_val, 1L)
    mid_low_threshold <- round(med - 1L * sd_val, 1L)
    mid_high_threshold <- round(med + 1L * sd_val, 1L)
    high_threshold <- round(med + 2L * sd_val, 1L)
    # Define the labels with both SD and actual values
    labels <- c("Very Low", "Low", "Moderate", "High", "Very High")
    if (more_descriptive_label) {
      labels <- c(
        paste0("Very Low (<=", low_threshold, ", median - 2 SD)"),
        paste0(
          "Low (",
          low_threshold,
          "to",
          mid_low_threshold,
          ", median - 2 to - 1 SD)"
        ),
        paste0(
          "Middle (",
          mid_low_threshold,
          "to",
          mid_high_threshold,
          ", median +/- 1 SD)"
        ),
        paste0(
          "High (",
          mid_high_threshold,
          "to",
          high_threshold,
          ", median + 1 to 2 SD)"
        ),
        paste0("Very High (>", high_threshold, ", median + 2 SD)")
      )
      # labels <- c(
      #   paste0("Very Low (<=", low_threshold, ", median - 1.5 SD)"),
      #   paste0("Low (", low_threshold, "to", mid_low_threshold, ", median - 1.5 to -0.5 SD)"),
      #   paste0("Middle (", mid_low_threshold, "to", mid_high_threshold, ", median +/- 0.5 SD)"),
      #   paste0("High (", mid_high_threshold, "to", high_threshold, ", median + 0.5 to 1.5 SD)"),
      #   paste0("Very High (>", high_threshold, ", median + 1.5 SD)")
      # )
    }
    # Create a factor column with 5 categories and descriptive labels
    data_category <- cut(
      vec,
      breaks = c(
        -Inf,
        low_threshold,
        mid_low_threshold,
        mid_high_threshold,
        high_threshold,
        Inf
      ),
      labels = labels,
      right = TRUE
    )
  } else if (method == "sd_mod") {
    # Standard deviation-based binning
    med <- stats::median(vec)
    sd_val <- stats::sd(vec)
    # Define the actual value ranges rounded to 1 decimal place
    low_threshold <- round(med - 1.5 * sd_val, 1L)
    mid_low_threshold <- round(med - 0.5 * sd_val, 1L)
    mid_high_threshold <- round(med + 0.5 * sd_val, 1L)
    high_threshold <- round(med + 1.5 * sd_val, 1L)
    # Define the labels with both SD and actual values
    labels <- c("Very Low", "Low", "Moderate", "High", "Very High")
    if (more_descriptive_label) {
      labels <- c(
        paste0("Very Low (<=", low_threshold, ", median - 1.5 SD)"),
        paste0(
          "Low (",
          low_threshold,
          " to ",
          mid_low_threshold,
          ", median - 1.5 to - 0.5 SD)"
        ),
        paste0(
          "Moderate (",
          mid_low_threshold,
          " to ",
          mid_high_threshold,
          ", median +/- 0.5 SD)"
        ),
        paste0(
          "High (",
          mid_high_threshold,
          " to ",
          high_threshold,
          ", median + 0.5 to 1.5 SD)"
        ),
        paste0("Very High (>", high_threshold, ", median + 1.5 SD)")
      )
    }
    # Create SD-based binning
    data_category <- cut(
      vec,
      breaks = c(
        -Inf,
        low_threshold,
        mid_low_threshold,
        mid_high_threshold,
        high_threshold,
        Inf
      ),
      labels = labels,
      right = TRUE
    )
  } else if (method == "quantile") {
    # Quantile-based binning
    quantile_cutoffs <- stats::quantile(vec,
                                        probs = seq(0L, 1L, length.out = quantiles + 1L),
                                        na.rm = TRUE)
    # Generate labels for the specified number of quantiles
    labels <- paste0("Q", 1L:quantiles)
    if (more_descriptive_label) {
      labels <- paste0(
        "Quantile ",
        1L:quantiles,
        " (",
        round(quantile_cutoffs[-length(quantile_cutoffs)], 1L),
        " to ",
        round(quantile_cutoffs[-1L], 1L),
        ")"
      )
    }
    # Create quantile-based binning
    data_category <- cut(
      vec,
      breaks = quantile_cutoffs,
      labels = labels,
      include.lowest = TRUE
    )
  } else {
    stop("Invalid method. Choose either 'sd' or 'quantile'.")
  }
  data_category <- factor(data_category,
                          levels = levels(data_category),
                          ordered = TRUE)
  data_category
}
#' @noRd
adjust_margins <- function(max_label_length,
                           tick_font_size = 12L,
                           base_margin = 20L) {
  extra_margin <- max_label_length * tick_font_size * 0.4  # Adjust multiplier as needed
  out <- base_margin + extra_margin
  out
}
#' @title make_parcats
#' @param DF data.frame
#' @param remove_missing Logical for removing missing/NA data
#' @param marginal_histograms Logical for including marginal_histograms
#' @export
make_parcats <- function(DF,
                         remove_missing = FALSE,
                         marginal_histograms = TRUE) {
  if (remove_missing) {
    DF <- stats::na.omit(DF)
  }
  colnames(DF) <-  colnames(DF) |>
    lapply(function(col) {
      x <- attr(DF[[col]], "label")
      ifelse(is.null(x), col, x)
    }) |>
    unlist()
  parcats::parcats(
    easyalluvial::alluvial_wide(DF, NA_label = "*Missing*"),
    marginal_histograms = marginal_histograms,
    data_input = DF,
    labelfont = list(size = 12L, color = "black"),
    tickfont = list(size = 12L, color = "black"),
    arrangement = "freeform"
  )
}
