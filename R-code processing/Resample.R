# resample function
Resamp <- function(df, column) {
  inds = sample(1:nrow(df))  
  df   = df[inds, ]
  
  dups = duplicated(df[, column])
  df   = df[!dups, ]
  inds = inds[!dups]
  
  df[sort(inds, index=T)$ix, ]
}