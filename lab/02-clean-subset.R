# TODO:   Test clean and subset functions
# 
# Author: Miguel Alvarez
################################################################################

library(specimens)

Spec <- readRDS("lab/specimens.rds")

Spec

Spec@collections <- Spec@collections[1:50, ]

Spec

validObject(Spec)

Spec <- clean(Spec)

Spec

validObject(Spec)


Spec <- readRDS("lab/specimens.rds")

x = Spec
slot = "collections"

subset <- substitute(field_nr > 50)
