all: patches

mozilla-extract-extensions: 
	cabal build

bnfstuff: need-bnfc distclean
	bnfc --haskell -p Text mozillaINI.cf

need-bnfc: ;
	@which bnfc > /dev/null

parser: bnfstuff
	happy -gca Text/ParMozillaINI.y
	alex -g Text/LexMozillaINI.x
	ghc --make Text/TestMozillaINI.hs -o Text/TestMozillaINI

clean:
	-rm -f Text/*.log Text/*.aux Text/*.hi Text/*.o Text/*.dvi

distclean: clean
	-rm -f Text/DocMozillaINI.* Text/LexMozillaINI.* Text/ParMozillaINI.* Text/LayoutMozillaINI.* Text/SkelMozillaINI.* Text/PrintMozillaINI.* Text/TestMozillaINI.* Text/AbsMozillaINI.* Text/TestMozillaINI Text/ErrM.* Text/SharedString.* Text/ComposOp.* Text/mozillaINI.dtd Text/XMLMozillaINI.* 
	-rmdir -p --ignore-fail-on-non-empty Text/

patches:  parser
	patch Text/PrintMozillaINI.hs Text/patch.PrintMozillaINI.hs

.PHONY: clean distclean patches parser all bnfstuff
