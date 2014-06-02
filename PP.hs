class PPNative a where pp :: a -> Doc
instance PPNative Int where pp = int
instance PPNative Bool where pp x = text $ if x then "true" else "false"
instance PPNative Float where pp = float
instance PPNative a => PPNative [a] where
    pp = sep . punctuate comma . map pp


