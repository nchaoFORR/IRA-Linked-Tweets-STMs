source('helper.R')

########################################## Fit initial model

# run test model using using anchor words in t-sne embedding

message("Training STM using Mimno's Heuristic...")

tic("using Mimmo's algorithm to avoid searching for an optimal K")

k_first <- stm(
  
  documents = docs,
  vocab = vocab,
  # when k is 0, Mimmo's algorithm is run to find convex hull within
  # anchor words after doing a t-sne embedding
  K = 0,
  content = ~ month,
  data = meta,
  init.type = "Spectral"
  
)

toc()

save(k_first, file = 'final_model3_stm.RData')
message("Model trained.")
