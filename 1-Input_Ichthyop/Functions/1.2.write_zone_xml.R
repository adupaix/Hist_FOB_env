

write_zone_xml <- function(filename, template, df_points){
  
  filer <- file(filename, open = "r")
  filew <- file(template, open = "w")
  
  header <- readLines(filer, n = 46)
  writeLines(header, filew)
  
  writeLines("<zones>", filew)
  
  delta = 10^-4
  
  for (i in 1:dim(df_points)[1]){
  cat("  <zone>",
               paste0("    <key>",i,"</key>"),
               "    <type>release</type>",
               "    <enabled>true</enabled>",
               "    <color>[r=255,g=0,b=255]</color>",
               "    <polygon>",
               "      <point>",
               paste0("        <index>",0,"</index>"),
               paste0("        <lon>",df_points$x[i],"</lon>"),
               paste0("        <lat>",df_points$y[i],"</lat>"),
               "      </point>",
               paste0("        <index>",1,"</index>"),
               paste0("        <lon>",df_points$x[i]+delta,"</lon>"),
               paste0("        <lat>",df_points$y[i],"</lat>"),
               "      </point>",
               paste0("        <index>",2,"</index>"),
               paste0("        <lon>",df_points$x[i]+delta,"</lon>"),
               paste0("        <lat>",df_points$y[i]+delta,"</lat>"),
               "      </point>",
               paste0("        <index>",3,"</index>"),
               paste0("        <lon>",df_points$x[i],"</lon>"),
               paste0("        <lat>",df_points$y[i]+delta,"</lat>"),
               "      </point>",
               "    </polygon>",
               "    <bathy_mask>",
               "      <enabled>false</enabled>",
               "      <line_inshore>0.0</line_inshore>",
               "      <line_offshore>12000.0</line_offshore>",
               "    </bathy_mask>",
               "    <thickness>",
               "      <enabled>true</enabled>",
               "      <lower_depth>15.0</lower_depth>",
               "      <upper_depth>0.0</upper_depth>",
               "    </thickness>",
               "  </zone>",
             
             file = filew, sep="\n", append = T)
  }
  writeLines("</zones>", filew)
  
  close(filer)
  close(filew)
  
}