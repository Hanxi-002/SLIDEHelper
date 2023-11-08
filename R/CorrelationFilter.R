#' Filter highly correlated variables out of input data. Examines number of correlations above correlation_threshold
#' that each feature has and then filters by correlation_percentile_filter across all features.
#'
#' @param data the data matrix
#' @param correlation_threshold double for correlation absolute value; correlations above this value will be examined for filtering.
#' Should be 0 < correlation_threshold < 1
#' @param correlation_percentile_filter double for percentile used to filter correlated features; Should be 0 < correlation_threshold < 1.
#' For each feature, number of other features this feature is correlated with (correlation >= correlation_threshold,
#' by absolute value) is counted. This filters based on percentile for this count across all features.
#' @param use_hist_to_filter_percentile logical to determine whether to bin correlation counts using a histogram and filter using the
#' percentile of the histogram breaks, rather than the percentile of the counts directly. Using this option can help smooth the distribution
#' of counts for finer grained filtering, but usually isn't necessary.
#' @export


correlation_filter = function(data, correlation_threshold = 0.8,
                              correlation_percentile_filter = 0.9,
                              use_hist_to_filter_percentile = F) {

  if (any(correlation_threshold, correlation_percentile_filter) >= 1 | any(correlation_threshold,correlation_percentile_filter) <= 0) {
    stop("correlation_threshold should be between 0 and 1; 0 < correlation_threshold < 1")
  }
  # scale data
  scaled_df = apply(data, 2, function(x) scale(x, T, F))

  # get unsigned correlation matrix
  df_cor = abs(cor(as.matrix(scaled_df)))

  # make upper triangular
  df_cor[lower.tri(df_cor, diag = T)] = 0

  # for each column, figure out how many variables are correlated with this variable
  # e.g. for correlation_threshold = 0.8, how many variables have a correlation > 0.8
  # with this variable
  df_cor_count = apply(df_cor, 2, function(x) length(which(x > correlation_threshold)))

  # from this, filter out the top percentile (variables with most correlations)
  # from the original dataframe

  if (use_hist_to_filter_percentile) {
    cor_hist = hist(df_cor_count, plot = F)

    df_filtered = data[, -which(df_cor_count > quantile(cor_hist$breaks, correlation_percentile_filter))]

  } else {

    df_filtered = data[, -which(df_cor_count > quantile(df_cor_count, correlation_percentile_filter))]
  }

  return(df_filtered)
}
