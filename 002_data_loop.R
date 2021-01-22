#Bring in libraries
library(parallel)
library(data.table)

#Find all csv files in your folder
csv.list <- list.files("data/", full.names = T, recursive = T, pattern = ".*.csv")


#Create function to read in data and perform fft on each column
read.fft <- function(x) {
  data <- fread(x)
  result <- aggregate_output(data)
  return(result)
}

#Apply function using multiple cores
all.results <- mclapply(csv.list,read.fft)

df <- bind_rows(all.results, .id = "column_label") %>% pivot_longer(3:11) %>% mutate(ST = as.character(ST))

df <- df %>% left_join(state, by = c("ST" = "val_min")) %>% select(-column_label)

df %>% group_by(name) %>% summarise(value = sum(value))

write_csv(df, "pums_data_aggregates.csv")
