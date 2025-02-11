g3_suitability_blueling <- function(){
    ~1/(1+exp(-0.23*(stock__midlen - 91.5856))) +  
      0.5/(1+exp(0.1668*(stock__midlen - 82.8539)))  
}
  