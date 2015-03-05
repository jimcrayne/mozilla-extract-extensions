diff --git a/Text/PrintMozillaINI.hs b/Text/PrintMozillaINI.hs
index e8b47e3..9670105 100644
--- a/Text/PrintMozillaINI.hs
+++ b/Text/PrintMozillaINI.hs
@@ -19,7 +19,7 @@ doc = (:)
 render :: Doc -> String
 render d = rend 0 (map ($ "") $ d []) "" where
   rend i ss = case ss of
-    "["      :ts -> showChar '[' . rend i ts
+    "["      :ts -> newnew i . showChar '[' . rend i ts
     "("      :ts -> showChar '(' . rend i ts
     "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
     "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
@@ -31,6 +31,7 @@ render d = rend 0 (map ($ "") $ d []) "" where
     t        :ts -> space t . rend i ts
     _            -> id
   new i   = showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
+  newnew i = showChar '\n' . showChar '\n' . replicateS (2*i) (showChar ' ') . dropWhile isSpace
   space t = showString t . (\s -> if null s then "" else (' ':s))
 
 parenth :: Doc -> Doc
@@ -97,7 +98,7 @@ instance Print Grp where
 
 instance Print KeyPair where
   prt i e = case e of
-    INIKV moztext value -> prPrec i 0 (concatD [prt 0 moztext, doc (showString "="), prt 0 value])
+    INIKV moztext value -> prPrec i 0 (concatD [doc (showString "\n"), prt 0 moztext, doc (showString "="), prt 0 value])
   prtList es = case es of
    [] -> (concatD [])
    x:xs -> (concatD [prt 0 x, prt 0 xs])
