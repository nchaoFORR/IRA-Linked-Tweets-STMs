# MAIN

require(tidyverse)
require(stm)
require(quanteda)
require(here)
require(lubridate)
require(tidytext)
require(tictoc)


# This file will create three documents in the directory;

# 1. csv containing top frex words in topics
# 2. csv containing top frex words in topics, specific to a particular month
# 3. document-topic proportions

# loads data and pre-processes data
source('helper.r')
# builds structural topic model
source('stm.R')
# extracts data for tableau
source("final_model_extract.r")