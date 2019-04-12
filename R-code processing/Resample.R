# resample function

# taken from John Colby (https://stackoverflow.com/questions/8041720/randomly-select-on-data-frame-for-unique-rows)
Resamp <- function(df, column) {
  inds = sample(1:nrow(df))  
  df   = df[inds, ]
  
  dups = duplicated(df[, column])
  df   = df[!dups, ]
  inds = inds[!dups]
  
  df[sort(inds, index=T)$ix, ]
}