
# build the local site (that can be browsed immediately from ./g_website/static/diagram_local.html)
serve_site()

# Create/update DAG
generate_DAG()

# Create or update codebooks
generate_Rmd()
generate_website()


# commit updated DAG, index and codebooks (after the committed changes are pushed, the site will be updated by GitHub within some minutes)
stage_website()
