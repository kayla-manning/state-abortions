
# this script includes the neighborhood weight matrix code & model-fitting code
# (using the functions created in model_helper.R)

source('model_helper.R')

# creating neighborhood weight matrices
{
  {
    {
      # getting spatial data for US state boundaries & subsetting out Alaska & Hawaii
      # (using tutorial at https://mhallwor.github.io/_pages/basics_SpatialPolygons)
      
      usa <- raster::getData('GADM', country='USA', level=1)
      usa <- usa[!usa$NAME_1 %in% c('Alaska', 'Hawaii'),]
      
      # merging with data
      
      usa <- merge(usa, nonspatial_df,
                   by.x = 'NAME_1', by.y = 'state',
                   duplicateGeoms = TRUE, all.x = FALSE)
      
      # ordering within-between categories so that the reference grouping makes sense
      
      usa@data$within_between <- fct_relevel(usa@data$within_between,
                                             'low-low', 'med-low', 'high-low',
                                             'low-med', 'med-med', 'high-med',
                                             'low-high', 'med-high', 'high-high')
      usa@data$within_between <- fct_relevel(usa@data$within_between,
                                             'high-high')
      
    }
    
    # using poly2nb to create a neighbors list & from that a neighbors matrix
    # https://rspatial.org/raster/rosu/Chapter7.html
    
    # usa_contig <- poly2nb(usa, queen=FALSE)
    
  }
  
  # writing the nb object to a file so that I don't have to recreate it every time
  # (it takes forever) & can just read it in directly
  
  # write.nb.gal(usa_contig, 'raw-data/helper/usa_contig_nb.gal')
  usa_contig <- read.gal('raw-data/helper/usa_contig_nb.gal')
  
  # check for symmetric relationships (since if TX is a neighbor of OK, OK should
  # also be a neighbor of TX)
  
  is.symmetric.nb(usa_contig)
  
  # build binary weight matrix
  
  weights.contig.B <- nb2listw(usa_contig, style = "B")
  print(weights.contig.B)
  
  # rescaling each row of B to sum to 1
  
  weights.contig.W <- nb2listw(usa_contig)
  
  # creating weights based on the inverse distance
  
  inv.dist <- lapply(nbdists(usa.dist.range, coordinates(usa)),
                     function(x) ifelse(x!=0, 1e3/(x), x))
  weights.inv.dist <- nb2listw(usa.dist.range, 
                               glist = inv.dist, # specify weights
                               style = "B")
  
  # note style B uses the weights specified by inv.dist, 
  # setting style = W will renormalize the weights to sum to one
  
  print(weights.inv.dist)
  image(listw2mat(weights.inv.dist)[,281:1],
        axes = FALSE)
  
  # also created weights based on inverse distance squared, which will give a
  # greater penalty to states with larger distances (similar to the construction
  # of the interstate scores)
  
  inv.dist2 <- lapply(nbdists(usa.dist.range, coordinates(usa)),
                      function(x) ifelse(x!=0, 1e3/(x^2), x))
  weights.inv.dist2 <- nb2listw(usa.dist.range, 
                                glist = inv.dist2, # specify weights
                                style = "B")
  
  # note style B uses the weights specified by inv.dist, 
  # setting style = W will renormalize the weights to sum to one
  
  print(weights.inv.dist2)
  image(listw2mat(weights.inv.dist2)[,281:1],
        axes = FALSE)
}

# fitting models
{
  # rate models
  {
    rates_inv_dist <- make_models(weights.inv.dist, 'rate',)
    rates_inv_dist2 <- make_models(weights.inv.dist2, 'rate')
    rates_contig <- make_models(weights.contig.W, 'rate')
  }
  
  # IE models
  {
    ie_inv_dist <- make_models(weights.inv.dist, 'ie')
    ie_inv_dist2 <- make_models(weights.inv.dist2, 'ie')
    ie_contig <- make_models(weights.contig.W, 'ie')
  }
  
  # late-early models
  {
    late_early_inv_dist <- make_models(weights.inv.dist, 'late_early')
    late_early_inv_dist2 <- make_models(weights.inv.dist2, 'late_early')
    late_early_contig <- make_models(weights.contig.W, 'late_early')
  }
}

# extra (can probably get rid of this.. it's nonresident-resident ratio)

{
  # generating the models and diagnostics for the three different weights matrices
  
  nonres_res_inv_dist <- make_nonres_res_models(weights.inv.dist)
  nonres_res_inv_dist2 <- make_nonres_res_models(weights.inv.dist2)
  nonres_res_contig <- make_nonres_res_models(weights.contig.W)
  
  
  # producing table as I did before... lowest AIC observed in SAR model with
  # inv_dist weights
  
  get_model_comparisons(nonres_res_inv_dist, nonres_res_inv_dist2, nonres_res_contig) %>% 
    kable(caption = 'Model selection for nonresident-to-resident ratios',
          booktabs = TRUE)
  
  
  # printing out coefficients for policy categories on our chosen model
  
  nonres_res_sar <- nonres_res_inv_dist[[2]][3][[1]]
  summary(nonres_res_sar)
  data.frame(coef(nonres_res_sar)) %>% 
    rownames_to_column('term') %>% 
    filter(str_detect(term, 'within_between')) %>% 
    arrange(coef.nonres_res_sar.)
  
  # printing moran test.. both this & the LR in the model summary have p-values <
  # 0.05... figure out what that means again
  
  nonres_res_inv_dist[[3]][3][[1]]
  
  # printing diagnostic plots
  
  nonres_res_inv_dist[[4]][3][[1]]
  
  # log(nonres/res - 1) = log((nonres-res) / res), so exponentiating coefficients
  # yields the multiplicative increase in the difference between nonresident and
  # resident abortions as a ratio of resident abortions... larger values means
  # that there are relatively more out-of-state abortions & smaller values mean
  # that there are relatively fewer out-of-state abortions
  
  data.frame(coef(nonres_res_sar)) %>% 
    rownames_to_column('term') %>% 
    filter(str_detect(term, 'within_between')) %>% 
    mutate(coef.nonres_res_sar. = exp(coef.nonres_res_sar.)) %>% 
    arrange(desc(coef.nonres_res_sar.)) %>% 
    rename(exp_coef = coef.nonres_res_sar.)
  
  # creating my plot of coefficients
  
  get_coef_plot(nonres_res_sar) +
    geom_vline(xintercept = log(1),
               color = 'red', 
               linetype = 'dotted') +
    labs(title = 'States with less strict surroundings have significantly fewer people traveling in \nand more people traveling out',
         subtitle = 'Relative to high-high reference group')
}
