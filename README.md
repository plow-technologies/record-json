Aeson instances for Vinyl records
=================================

This package exports the RecJSON newtype, which is a simple wrapper around the Rec type from Vinyl.
RecJSON restricts the kind of fields to Symbol `RecJSON :: (Symbol -> *) -> [Symbol] -> *` and uses the field Symbols as field names for the JSON encoding.
It requires only that all the fields be instances of KnownSymbol (i.e. defined at compile time) and that the field types be instances of (To,From)JSON.
