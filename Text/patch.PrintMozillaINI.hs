diff --git a/Text/PrintMozillaINI.hs b/Text/PrintMozillaINI.hs
index e8b47e3..142e9fc 100644
--- a/Text/PrintMozillaINI.hs
+++ b/Text/PrintMozillaINI.hs
@@ -97,7 +97,7 @@ instance Print Grp where
 
 instance Print KeyPair where
   prt i e = case e of
-    INIKV moztext value -> prPrec i 0 (concatD [prt 0 moztext, doc (showString "="), prt 0 value])
+    INIKV moztext value -> prPrec i 0 (concatD [doc (showString "\n"), prt 0 moztext, doc (showString "="), prt 0 value])
   prtList es = case es of
    [] -> (concatD [])
    x:xs -> (concatD [prt 0 x, prt 0 xs])
