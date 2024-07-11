# Generally saved in the .Renviron file 
readRenviron(".Renviron")
SHEET_ID <- Sys.getenv("BRAINVR_ISLANDS_ITEMS_SHEET_ID")

item_translations <- googlesheets4::read_sheet(SHEET_ID, sheet = "ITEM_TRANSLATIONS")
item_locations <- googlesheets4::read_sheet(SHEET_ID, sheet = "ITEM_LOCATIONS")

ITEM_CODES <- data.frame(item_translations)
LOCATION_ITEM <- data.frame(item_locations)

save(ITEM_CODES, file = "data/ITEM_CODES.rda")
save(LOCATION_ITEM, file = "data/LOCATION_ITEM.rda")
