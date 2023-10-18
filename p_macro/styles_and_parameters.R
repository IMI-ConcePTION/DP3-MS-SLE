# create styles and default values in multiple datasets (some parameter might be eliminated altogether, for example arrow width)
create_page_styles_df <- function() {
  temp_df <- tibble::tribble(
    ~name_style,   ~dx,  ~dy,~grid,~gridSize,~guides,~tooltips,~connect,~arrows,~fold,~page,~pageScale,~pageWidth,~pageHeight,~math,~shadow,
    #----------|------|-----|-----|---------|-------|---------|--------|-------|-----|-----|----------|----------|-----------|-----|-------
    "default"  ,"1086","846",  "1",     "10",    "1",      "1",     "1",    "1",  "1",  "1",       "1",     "850",     "1100",  "0",    "0"
  )
}

create_arrow_styles_df <- function() {
  temp_df <- tibble::tribble(
    ~name_style   ,~style,~parent,~edge,~html,~verticalAlign,~startArrow,~startFill,~endArrow,~startSize,~exitX,~exitY,~exitDx,~exitDy,~entryX,~entryY,~entryDx,~entryDy,           ~edgeStyle,    ~elbow,~curved,~width,~relative,      ~as,~rounded,
    #-------------|------|-------|-----|-----|--------------|-----------|----------|---------|----------|------|------|-------|-------|-------|-------|--------|--------|---------------------|----------|-------|------|---------|----------
    # direct arrow
    "circle arrow",    "",    "1",  "1",  "1",      "bottom",     "oval",         1,  "block",         8,     1,   0.5,      0,      0,      0,    0.5,       0,       0,"orthogonalEdgeStyle","vertical",      1,  "60",      "1","geometry",1,
    # curved arrow
    "curved arrow",    "",    "1",  "1",  "1",      "bottom",     "oval",         1,  "block",         8,     1,   0.5,      0,      0,      0,    0.5,       0,       0,"orthogonalEdgeStyle","vertical",      1,  "60",      "1","geometry",0,
  )
  return(temp_df)
}

create_cell_styles_df <- function() {
  temp_df <- tibble::tribble(
    ~name_style ,   ~style,~parent,~vertex,~html,~rounded,~whiteSpace,~fillColor,~strokeColor,~strokeWidth,~dashed,~shape,~width,~height,       ~as,
    #-----------|---------|-------|-------|-----|--------|-----------|----------|------------|------------|-------|------|------|-------|-----------
    "empty"     ,       "",     "",     "",   "",      "",         "",        "",          "",          "",     "",    "",    "",     "",        "",
    "start"     ,       "",    "0",     "",   "",      "",         "",        "",          "",          "",     "",    "",    "",     "",        "",
    "circle"    ,"ellipse",    "1",    "1",  "1",      "",     "wrap",        "",          "",          "",     "","oval",  "80",   "80","geometry",
    "white"     ,       "",    "1",    "1",  "1",     "0",     "wrap", "#FFFFFF",   "#36393d",          "",     "","oval", "120",   "60","geometry",
    "green"     ,       "",    "1",    "1",  "1",     "1",     "wrap", "#008000",   "#36393d",          "",     "","oval", "120",   "60","geometry",
    "dark blue" ,       "",    "1",    "1",  "1",     "1",     "wrap", "#0000FF",   "#36393d",          "",     "","oval", "120",   "60","geometry",
    "light blue",       "",    "1",    "1",  "1",     "1",     "wrap", "#0080FF",   "#36393d",          "",     "","oval", "120",   "60","geometry",
    "orange"    ,       "",    "1",    "1",  "1",     "0",     "wrap", "#ffcc99",   "#36393d",          "",     "","oval", "120",   "60","geometry",
    "yellow"    ,       "",    "1",    "1",  "1",     "0",     "wrap", "#ffff88",   "#36393d",          "",     "","oval", "120",   "60","geometry"
  )
  return(temp_df)
}
