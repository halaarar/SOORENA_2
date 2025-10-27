# Test data loading
df <- read.csv("data/predictions_for_app.csv", stringsAsFactors = FALSE)
print(paste("Rows:", nrow(df)))
print("Columns:")
print(colnames(df))
print("\nSample:")
print(head(df, 2))