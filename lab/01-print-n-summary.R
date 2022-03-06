# TODO:   Test print and summary functions
# 
# Author: Miguel Alvarez
################################################################################

library(specimens)

Spec <- readRDS("lab/specimens.rds")

Spec

summary(Spec, 1777)
summary(Spec, spec_id = 1181)
