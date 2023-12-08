library(tidyverse)
library(fixest)
library(haven)
library(glue)
library(gt)
library(scales)
library(modelsummary)

df <- read_dta("data/graduation_pooled_hh.dta")

# NOTE: in pset 10 "rand_unit" should be used for clustering standard errors.
#       "geo_cluster" is available for use as a fixed effect.
#       In the pset you will want to include all of the "control_" variables.

# this model has no clustering or FEs

mod1 <- feols(asset_index_fup ~ assignment + asset_index_bsl + m_asset_hh_index_bsl + control_livestockindex, data = df)

# this model has a FE at the geo_cluster level and uses these groups for cluster robust standard errors by default

mod2 <- feols(asset_index_fup ~ assignment + asset_index_bsl + m_asset_hh_index_bsl + control_livestockindex | geo_cluster, data = df)

# this model has FEs at the geo_cluster level and correctly manually specifies the cluster robust s.e. at the "rand_unit" level

mod3 <- feols(asset_index_fup ~ assignment + asset_index_bsl + m_asset_hh_index_bsl + control_livestockindex | geo_cluster,
             data = df, cluster = "rand_unit")

# Note that mod2 and mod3 have the same coefficient estimates but different standard errors.

summary(mod1)
summary(mod2)
summary(mod3)
