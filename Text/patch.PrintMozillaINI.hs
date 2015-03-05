diff --git a/Text/PrintMozillaINI.hs b/Text/PrintMozillaINI.hs
index e8b47e3..058f588 100644
--- a/Text/PrintMozillaINI.hs
+++ b/Text/PrintMozillaINI.hs
@@ -19,7 +19,7 @@ doc = (:)
 render :: Doc -> String
 render d = rend 0 (map ($ "") $ d []) "" where
   rend i ss = case ss of
-    "["      :ts -> showChar '[' . rend i ts
+    "["      :ts -> new i . showChar '[' . rend i ts
     "("      :ts -> showChar '(' . rend i ts
     "{"      :ts -> showChar '{' . new (i+1) . rend (i+1) ts
     "}" : ";":ts -> new (i-1) . space "}" . showChar ';' . new (i-1) . rend (i-1) ts
@@ -97,7 +97,7 @@ instance Print Grp where
 
 instance Print KeyPair where
   prt i e = case e of
-    INIKV moztext value -> prPrec i 0 (concatD [prt 0 moztext, doc (showString "="), prt 0 value])
+    INIKV moztext value -> prPrec i 0 (concatD [doc (showString "\n"), prt 0 moztext, doc (showString "="), prt 0 value])
   prtList es = case es of
    [] -> (concatD [])
    x:xs -> (concatD [prt 0 x, prt 0 xs])
