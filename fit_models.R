
# this script includes the neighborhood weight matrix code & model-fitting code
# (using the functions created in model_helper.R)

source('model_helper.R')

# creating neighborhood weight matrices
{
  {
    
    # using poly2nb to create a neighbors list & from that a neighbors matrix
    # https://rspatial.org/raster/rosu/Chapter7.html
    
    # usa_contig <- poly2nb(usa, queen=FALSE)
    
  }
  
  # writing the nb object to a file so that I don't have to recreate it every time
  # (it takes forever) & can just read it in directly
  
  #write.nb.gal(usa_contig, 'raw-data/helper/usa_contig_nb.gal')
  usa_contig <- read.gal('raw-data/helper/usa_contig_nb.gal')
  
  # check for symmetric relationships (since if TX is a neighbor of OK, OK should
  # also be a neighbor of TX)
  
  is.symmetric.nb(usa_contig)
  
  # build binary weight matrix
  
  weights.contig.B <- nb2listw(usa_contig, style = "B")
  
  # rescaling each row of B to sum to 1
  
  weights.contig.W <- nb2listw(usa_contig)
  
  # creating weights based on the inverse distance
  
  usa.dist.range <- dnearneigh(coordinates(usa),
                               d1 = 0, # specify lower bound of range
                               d2 = 2e4 # specify upper bound of range
  )
  inv.dist <- lapply(nbdists(usa.dist.range, coordinates(usa)),
                     function(x) ifelse(x!=0, 1e3/(x), x))
  
  # note style B uses the weights specified by inv.dist, 
  # setting style = W will renormalize the weights to sum to one
  
  weights.inv.dist <- nb2listw(usa.dist.range, 
                               glist = inv.dist, # specify weights
                               style = "W")
  
  # also created weights based on inverse distance squared, which will give a
  # greater penalty to states with larger distances (similar to the construction
  # of the interstate scores)
  
  inv.dist2 <- lapply(nbdists(usa.dist.range, coordinates(usa)),
                      function(x) ifelse(x!=0, 1e3/(x^2), x))
  weights.inv.dist2 <- nb2listw(usa.dist.range, 
                                glist = inv.dist2, # specify weights
                                style = "W")

}

# fitting models
{
  # rate models
  {
    rates_inv_dist <- make_models(weights.inv.dist, 'rate')
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


