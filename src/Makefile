#
# Makefile for Deimos
#

### variables #################################################################

# what system are we running  on?
PLATFORMCPU = $(shell uname -p)
PLATFORMOS = $(shell uname -s)
PLATFORM = $(PLATFORMCPU)-$(PLATFORMOS)

# where the ABR libs are originally sourced from
LIBSRC = ../../ABRHLibs

# where the ABR libs are copied to locally
LIBDIR = .

# where the ABR LaTeX packages are originally sourced from
TEXSRC = ../../ABRLaTeX

# where the ABR LaTeX packages are copied to locally
TEXDIR = .

# destination for binaries
BINDIR = ../bin/$(PLATFORM)

# destination for documentation
DOCDIR = ../doc

# where the example files are
EXDIR = ../theories

# destination for compiler object and interface files
OBJDIR = ./objects/$(PLATFORM)

# destination for the tool
INSTALLDIR = ~/bin

# GHC compilation options
GHC = ghc --make -i$(LIBDIR) -odir $(OBJDIR) -hidir $(OBJDIR)

# File copying command 
COPY = cp

# File deleting command
DELETE = rm -f

# PDF viewing command
VIEWPDF = open

# Documentation products
DOCS = ${DOCDIR}/Deimos-long.pdf ${DOCDIR}/Deimos-short.pdf

# All executatble products
BINS = ${BINDIR}/DefeasibleParser ${BINDIR}/DProver ${BINDIR}/ODProver \
       ${BINDIR}/DTScale ${BINDIR}/Defeasible.cgi
PC_BINS = ${BINDIR}/DefeasibleParser.exe ${BINDIR}/DProver.exe \
          ${BINDIR}/ODProver.exe ${BINDIR}/DTScale.exe 

# Junk files made for and by Latex
TEX_CRUD = *.aux *.log *.out *.toc *.bbl \
           *.blg *.brf *.ind *.idx *.ilg

# Junk files made by GHC
GHC_CRUD = $(OBJDIR)/*.hi \
           $(OBJDIR)/*.o \
           $(OBJDIR)/ABR/*.hi \
           $(OBJDIR)/ABR/*.o

# Modules for Deimos
MODULES = DInference.lhs \
          DProve.lhs \
          DProver.lhs \
          DRule.lhs \
          DRunFile.lhs \
          DTScale.lhs \
          DTestTheories.lhs \
          DTheory.lhs \
          Defeasible.cgi.lhs \
          DefeasibleLexer.lhs \
          DefeasibleParser.lhs \
          History.lhs \
          Label.lhs \
          Literal.lhs \
          ODProver.lhs \
          ODTheory.lhs \
          Priority.lhs \
          ProofResult.lhs \
          ThreadedTest.lhs

# ABRHLib modules used and stored locally
LIBMODULES = $(LIBDIR)/ABR/DeepSeq.lhs \
             $(LIBDIR)/ABR/List.lhs \
             $(LIBDIR)/ABR/Control/List.lhs \
             $(LIBDIR)/ABR/SparseSet.lhs \
             $(LIBDIR)/ABR/Data/BSTree.lhs \
             $(LIBDIR)/ABR/Showing.lhs \
             $(LIBDIR)/ABR/Parser.lhs \
             $(LIBDIR)/ABR/Parser/Pos.lhs \
             $(LIBDIR)/ABR/Parser/Lexers.lhs \
             $(LIBDIR)/ABR/Parser/Checks.lhs \
             $(LIBDIR)/ABR/Control/Check.lhs \
             $(LIBDIR)/ABR/DebugArray.lhs \
             $(LIBDIR)/ABR/Text/String.lhs \
             $(LIBDIR)/ABR/Text/Markup.lhs \
             $(LIBDIR)/ABR/Data/Queue.lhs \
             $(LIBDIR)/ABR/Text/Configs.lhs \
             $(LIBDIR)/ABR/Args.lhs \
             $(LIBDIR)/ABR/HaskellLexer.lhs \
             $(LIBDIR)/ABR/Graph.lhs \
             $(LIBDIR)/ABR/CGI.lhs

# Files to include when typesetting          
INCLUDES = tex-includes/DProver.tex \
           tex-includes/DSyntax.tex \
           tex-includes/DTScale.tex \
           tex-includes/DTagLitSyntax.tex \
           tex-includes/DTestTheories.tex \
           tex-includes/Defeasible.cgi.tex \
           tex-includes/DefeasibleParser.tex \
           tex-includes/ODProver.tex \
           tex-includes/comment1Syntax.tex \
           tex-includes/comment2Syntax.tex \
           tex-includes/labelSyntax.tex \
           tex-includes/literalSyntax.tex \
           tex-includes/minus--d.tex \
           tex-includes/minus--da.tex \
           tex-includes/minus--dt.tex \
           tex-includes/minus-D.tex \
           tex-includes/minus-S.tex \
           tex-includes/name1Syntax.tex \
           tex-includes/name2Syntax.tex \
           tex-includes/plus--d.tex \
           tex-includes/plus--da.tex \
           tex-includes/plus--dt.tex \
           tex-includes/plus-D.tex \
           tex-includes/plus-S.tex \
           tex-includes/prioritySyntax.tex \
           tex-includes/prologLiteralSyntax.tex \
           tex-includes/prologRuleSyntax.tex \
           tex-includes/ruleSyntax.tex \
           tex-includes/theorySyntax.tex

# Packages needed when typesetting          
TEXPACKAGES = $(TEXDIR)/CrampedCode.cls \
              $(TEXDIR)/Handbook.cls \
              $(TEXDIR)/HaskellVerb.sty

# Root name of the distribution file
DISTNAME = Deimos

# Where to put the distrbution file
DISTDEST = ..

# full path of the temporary directory
DISTDIR = $(DISTDEST)/$(DISTNAME)

# Full name of the distrbution file
DISTFILE = $(DISTNAME).tar.gz

### fake targets ##############################################################

# Make everyting
.PHONY : all
all : doc bin

# make the documentation
.PHONY : doc
doc : $(DOCS)

# Make the executable
.PHONY : bin
bin : $(BINS)
        
# Make the executable
.PHONY : pc_bin
pc_bin : $(PC_BINS)
        
# Clean up intermediate products -- leaves some for distribution
.PHONY : clean
clean :
	rm *.o *.hi *.aux *.bbl *.brf *.log *.out *.blg *.toc

# Package for distribution
.PHONY : distribute
distribute : $(DISTDEST)/$(DISTFILE)

### updating library modules and LaTeX packages ###############################

# fetch up-to-date library modules iff ABR defined
ifdef ABR

$(LIBDIR)/ABR/DeepSeq.lhs : $(LIBSRC)/ABR/DeepSeq.lhs
	mkdir -p $(LIBDIR)/ABR
	$(COPY) $(LIBSRC)/ABR/DeepSeq.lhs $(LIBDIR)/ABR/DeepSeq.lhs

$(LIBDIR)/ABR/List.lhs : $(LIBSRC)/ABR/List.lhs
	mkdir -p $(LIBDIR)/ABR
	$(COPY) $(LIBSRC)/ABR/List.lhs $(LIBDIR)/ABR/List.lhs

$(LIBDIR)/ABR/Control/List.lhs : $(LIBSRC)/ABR/Control/List.lhs
	mkdir -p $(LIBDIR)/ABR/Control
	$(COPY) $(LIBSRC)/ABR/Control/List.lhs $(LIBDIR)/ABR/Control/List.lhs

$(LIBDIR)/ABR/SparseSet.lhs : $(LIBSRC)/ABR/SparseSet.lhs
	mkdir -p $(LIBDIR)/ABR
	$(COPY) $(LIBSRC)/ABR/SparseSet.lhs $(LIBDIR)/ABR/SparseSet.lhs

$(LIBDIR)/ABR/Data/BSTree.lhs : $(LIBSRC)/ABR/Data/BSTree.lhs
	mkdir -p $(LIBDIR)/ABR/Data
	$(COPY) $(LIBSRC)/ABR/Data/BSTree.lhs $(LIBDIR)/ABR/Data/BSTree.lhs

$(LIBDIR)/ABR/Showing.lhs : $(LIBSRC)/ABR/Showing.lhs
	mkdir -p $(LIBDIR)/ABR
	$(COPY) $(LIBSRC)/ABR/Showing.lhs $(LIBDIR)/ABR/Showing.lhs

$(LIBDIR)/ABR/Parser.lhs : $(LIBSRC)/ABR/Parser.lhs
	mkdir -p $(LIBDIR)/ABR
	$(COPY) $(LIBSRC)/ABR/Parser.lhs $(LIBDIR)/ABR/Parser.lhs

$(LIBDIR)/ABR/Parser/Pos.lhs : $(LIBSRC)/ABR/Parser/Pos.lhs
	mkdir -p $(LIBDIR)/ABR/Parser
	$(COPY) $(LIBSRC)/ABR/Parser/Pos.lhs $(LIBDIR)/ABR/Parser/Pos.lhs

$(LIBDIR)/ABR/Parser/Lexers.lhs : $(LIBSRC)/ABR/Parser/Lexers.lhs
	mkdir -p $(LIBDIR)/ABR/Parser
	$(COPY) $(LIBSRC)/ABR/Parser/Lexers.lhs $(LIBDIR)/ABR/Parser/Lexers.lhs

$(LIBDIR)/ABR/Parser/Checks.lhs : $(LIBSRC)/ABR/Parser/Checks.lhs
	mkdir -p $(LIBDIR)/ABR/Parser
	$(COPY) $(LIBSRC)/ABR/Parser/Checks.lhs $(LIBDIR)/ABR/Parser/Checks.lhs

$(LIBDIR)/ABR/Control/Check.lhs : $(LIBSRC)/ABR/Control/Check.lhs
	mkdir -p $(LIBDIR)/ABR/Control
	$(COPY) $(LIBSRC)/ABR/Control/Check.lhs $(LIBDIR)/ABR/Control/Check.lhs

$(LIBDIR)/ABR/DebugArray.lhs : $(LIBSRC)/ABR/DebugArray.lhs
	mkdir -p $(LIBDIR)/ABR
	$(COPY) $(LIBSRC)/ABR/DebugArray.lhs $(LIBDIR)/ABR/DebugArray.lhs

$(LIBDIR)/ABR/Text/String.lhs : $(LIBSRC)/ABR/Text/String.lhs
	mkdir -p $(LIBDIR)/ABR/Text
	$(COPY) $(LIBSRC)/ABR/Text/String.lhs $(LIBDIR)/ABR/Text/String.lhs

$(LIBDIR)/ABR/Text/Markup.lhs : $(LIBSRC)/ABR/Text/Markup.lhs
	mkdir -p $(LIBDIR)/ABR/Text
	$(COPY) $(LIBSRC)/ABR/Text/Markup.lhs $(LIBDIR)/ABR/Text/Markup.lhs

$(LIBDIR)/ABR/QuineMcClusky.lhs : $(LIBSRC)/ABR/QuineMcClusky.lhs
	mkdir -p $(LIBDIR)/ABR
	$(COPY) $(LIBSRC)/ABR/QuineMcClusky.lhs $(LIBDIR)/ABR/QuineMcClusky.lhs

$(LIBDIR)/ABR/Data/Queue.lhs : $(LIBSRC)/ABR/Data/Queue.lhs
	mkdir -p $(LIBDIR)/ABR/Data
	$(COPY) $(LIBSRC)/ABR/Data/Queue.lhs $(LIBDIR)/ABR/Data/Queue.lhs

$(LIBDIR)/ABR/Text/Configs.lhs : $(LIBSRC)/ABR/Text/Configs.lhs
	mkdir -p $(LIBDIR)/ABR/Text
	$(COPY) $(LIBSRC)/ABR/Text/Configs.lhs $(LIBDIR)/ABR/Text/Configs.lhs

$(LIBDIR)/ABR/Args.lhs : $(LIBSRC)/ABR/Args.lhs
	mkdir -p $(LIBDIR)/ABR
	$(COPY) $(LIBSRC)/ABR/Args.lhs $(LIBDIR)/ABR/Args.lhs

$(LIBDIR)/ABR/HaskellLexer.lhs : $(LIBSRC)/ABR/HaskellLexer.lhs
	mkdir -p $(LIBDIR)/ABR
	$(COPY) $(LIBSRC)/ABR/HaskellLexer.lhs $(LIBDIR)/ABR/HaskellLexer.lhs

$(LIBDIR)/ABR/Graph.lhs : $(LIBSRC)/ABR/Graph.lhs
	mkdir -p $(LIBDIR)/ABR
	$(COPY) $(LIBSRC)/ABR/Graph.lhs $(LIBDIR)/ABR/Graph.lhs

$(LIBDIR)/ABR/CGI.lhs : $(LIBSRC)/ABR/CGI.lhs
	mkdir -p $(LIBDIR)/ABR
	$(COPY) $(LIBSRC)/ABR/CGI.lhs $(LIBDIR)/ABR/CGI.lhs

$(TEXDIR)/CrampedCode.cls : $(TEXSRC)/CrampedCode/CrampedCode.cls
	mkdir -p $(TEXDIR)
	$(COPY) $(TEXSRC)/CrampedCode/CrampedCode.cls $(TEXDIR)/CrampedCode.cls

$(TEXDIR)/Handbook.cls : $(TEXSRC)/Handbook/Handbook.cls
	mkdir -p $(TEXDIR)
	$(COPY) $(TEXSRC)/Handbook/Handbook.cls $(TEXDIR)/Handbook.cls

$(TEXDIR)/HaskellVerb.sty : $(TEXSRC)/HaskellVerb/HaskellVerb.sty
	mkdir -p $(TEXDIR)
	$(COPY) $(TEXSRC)/HaskellVerb/HaskellVerb.sty $(TEXDIR)/HaskellVerb.sty

endif

### building ##################################################################

${BINDIR}/DefeasibleParser : $(MODULES) $(LIBMODULES)
	mkdir -p $(OBJDIR)
	mkdir -p $(BINDIR)
	$(GHC) DefeasibleParser.lhs -o ${BINDIR}/DefeasibleParser
	
${BINDIR}/DProver : $(MODULES) $(LIBMODULES)
	mkdir -p $(OBJDIR)
	mkdir -p $(BINDIR)
	$(GHC) DProver.lhs -o ${BINDIR}/DProver
	
${BINDIR}/ODProver : $(MODULES) $(LIBMODULES)
	mkdir -p $(OBJDIR)
	mkdir -p $(BINDIR)
	$(GHC) ODProver.lhs -o ${BINDIR}/ODProver
	
${BINDIR}/DTScale : $(MODULES) $(LIBMODULES)
	mkdir -p $(OBJDIR)
	mkdir -p $(BINDIR)
	$(GHC) DTScale.lhs -o ${BINDIR}/DTScale
	
${BINDIR}/Defeasible.cgi : $(MODULES) $(LIBMODULES)
	mkdir -p $(OBJDIR)
	mkdir -p $(BINDIR)
	$(GHC) Defeasible.cgi.lhs -o ${BINDIR}/Defeasible.cgi
	
${BINDIR}/DefeasibleParser.exe : $(MODULES) $(LIBMODULES)
	mkdir -p $(OBJDIR)
	mkdir -p $(BINDIR)
	$(GHC) DefeasibleParser.lhs -o ${BINDIR}/DefeasibleParser.exe
	
${BINDIR}/DProver.exe : $(MODULES) $(LIBMODULES)
	mkdir -p $(OBJDIR)
	mkdir -p $(BINDIR)
	$(GHC) DProver.lhs -o ${BINDIR}/DProver.exe
	
${BINDIR}/ODProver.exe : $(MODULES) $(LIBMODULES)
	mkdir -p $(OBJDIR)
	mkdir -p $(BINDIR)
	$(GHC) ODProver.lhs -o ${BINDIR}/ODProver.exe
	
${BINDIR}/DTScale.exe : $(MODULES) $(LIBMODULES)
	mkdir -p $(OBJDIR)
	mkdir -p $(BINDIR)
	$(GHC) DTScale.lhs -o ${BINDIR}/DTScale.exe

### typesetting ###############################################################

${DOCDIR}/Deimos-long.pdf : Deimos-long.tex Defeasible.tex \
                            ${MODULES} ${INCLUDES}
	mkdir -p ${DOCDIR}
	pdflatex Deimos-long.tex
	bibtex Deimos-long
	pdflatex Deimos-long.tex
	pdflatex Deimos-long.tex
	mv Deimos-long.pdf ${DOCDIR}

${DOCDIR}/Deimos-short.pdf : Deimos-short.tex Defeasible.tex \
                             ${MODULES} ${INCLUDES}
	mkdir -p ${DOCDIR}
	pdflatex Deimos-short.tex
	bibtex Deimos-short
	pdflatex Deimos-short.tex
	pdflatex Deimos-short.tex
	mv Deimos-short.pdf ${DOCDIR}

### packaging for distribution ##############################################

$(DISTDEST)/$(DISTFILE) : doc $(LIBMODULES) $(MODULES) $(INCLUDES) $(EXAMPLES)
	mkdir $(DISTDIR)
	mkdir $(DISTDIR)/bin
	mkdir $(DISTDIR)/doc
	$(COPY) $(DOCS) $(DISTDIR)/doc
	mkdir $(DISTDIR)/theories
	$(COPY) $(EXDIR)/*.t $(DISTDIR)/theories
	mkdir $(DISTDIR)/src
	$(COPY) Makefile *.bib *.lhs $(DISTDIR)/src
	mkdir $(DISTDIR)/src/ABR
	mkdir $(DISTDIR)/src/ABR/Control
	mkdir $(DISTDIR)/src/ABR/Data
	mkdir $(DISTDIR)/src/ABR/Parser
	mkdir $(DISTDIR)/src/ABR/Text
	$(COPY) ABR/*.lhs $(DISTDIR)/src/ABR
	$(COPY) ABR/Control/*.lhs $(DISTDIR)/src/ABR/Control
	$(COPY) ABR/Data/*.lhs $(DISTDIR)/src/ABR/Data
	$(COPY) ABR/Parser/*.lhs $(DISTDIR)/src/ABR/Parser
	$(COPY) ABR/Text/*.lhs $(DISTDIR)/src/ABR/Text
	mkdir $(DISTDIR)/src/tex-includes
	$(COPY) tex-includes/*.tex $(DISTDIR)/src/tex-includes
	tar -cf $(DISTDIR).tar $(DISTDIR)
	gzip -f $(DISTDIR).tar
	rm -r $(DISTDIR)
