

# Generate splitting points
ds.gen_spp <- function(mode = "percentile"){
  if (mode == "exact"){
    spp <- exact_sppDS()
  }
  
  if (mode == "percentile"){
    spp <- percentile_sppDS()
  }
  
  return spp
}