###############################################################################
### USER-DEPENDENT VARIABLES
### USE ENVIRONMENT VARIABLES WHENEVER POSSIBLE

# NOTE: All environment variables need to be exported PRIOR to starting the
# Emacs server as EDITOR in your shell startup files; otherwise, they will not
# be available to Emacs.
# When I moved from using Bash to Zsh, I inadvertently changed the order of
# import, and started the Emacs server before importing, and caused a horrible
# bug which caused the program to work on one computer but fail on another.

# The absolute path to this Template file
TEMPLATE := $(SYNC_ORG_TEMPLATE)


### TOOLS & RESOURCES
# tools is a directory holding tangled scripts, such as cmprpl
# resources is a directory holding static resources for the project
# images is a directory holding jpg and png image files
TOOLS	:= tools
CMPRPL	:= $(TOOLS)/cmprpl
RESOURCES := resources
IMAGES  := $(RESOURCES)/images

# Use emacsclient as $EDITOR; make sure it is set in a shell startup file and
# the server has been started.
EMACS	  := $(EMACS)
EDITOR	  := $(EDITOR)

# User’s personal GitHub token for authentication to GitHub
# DO NOT HARD-CODE THIS VALUE
GITHUB_TOKEN := $(GITHUB_TOKEN)

# The AWS Command Line Interface (AWS CLI) is an open source tool
# that enables you to interact with AWS services using commands in
# your command-line shell.  It must be present on your system.  Run the 'make'
# command 'install-aws-cli' to install it if you do not have it.  Be sure to
# run 'aws configure' after installing it.  This will place your AWS
# credentials into ~/.aws/credentials.
AWS := aws
S3  := $(AWS) s3

# The AWS region of choice; this can also be in .aws/config
REGION := --region us-west-2

### END OF USER-DEPENDENT VARIABLES
###############################################################################
### MAKE-GENERATED VARIABLES

### PROJ AND ORG
# ORG is the name of this Org file with extension .org
# PROJ is the project name---the Org file name without extension.

### NOTE: there can be only one Org file in the project directory;
# so far this has not been a problem, but it might be.

PWD  := $(shell pwd)
ORG  := $(shell ls *.org)
PROJ := $(basename $(ORG))

### NOTE: S is needed only for the Template file because of the way it is nested
# one level deep in the Templates GitHub repo, which uses the plural form
# of Templates, whereas this file uses the singular form, Template.  So when
# the homepage link is updated, the curl command must be told to use the plural
# form.	 This is obviously a hack only for my own use and can be removed once
# I clean up this anomaly.

ifeq ($(PROJ),$(basename $(notdir $(TEMPLATE))))
S := s
endif

# The AWS S3 bucket to use to store the html source file; it is found at the
# key #+bucket towards the beginning of the file and should include the appropriate
# suffix (.com, .net, .org, etc)
BUCKET       := $(shell $(EDITOR) --eval \
	       '(with-current-buffer (find-file-noselect "$(ORG)") \
		  (save-excursion \
		    (goto-char (point-min)) \
		    (re-search-forward "^\#[+]bucket:\\(.*\\)$$" nil t) \
		    (match-string-no-properties 1)))')
S3_BUCKET    := s3://$(BUCKET)
HTTPS_BUCKET := https://$(BUCKET)

### DIR, SRC
# DIR is the .info name found at '#+texinfo_filename:<DIR>.info' (at
# the bottom of this file in the export configuration settings)
# without its extension, used as the INFO filename and the name of the
# HTML export directory; this code uses the lowercased PROJ name if
# there is no '#+texinfo_filename'.
# SRC is HTML directory based upon the DIR name

#DIR := $(shell $(EDITOR) --eval \
#	'(with-current-buffer (find-file-noselect "$(ORG)") \
#		(save-excursion \
#		(goto-char (point-min)) \
#		(re-search-forward "^\#[+]\\(?:texinfo_filename\\|TEXINFO_FILENAME\\):\\(.*\\).info$$" nil t) \
#		(match-string-no-properties 1)))')

DIR := $(shell sed -E -n "/^\#\+texinfo_filename/s/^.*:(.*)\.info$$/\1/p" $(ORG))
ifeq ($(DIR),$(EMPTY))
	DIR := $(shell echo $(PROJ) | tr "[:upper:]" "[:lower:]")
endif

SRC := $(DIR)/

### VERS: v1.2.34/
# VERS is the version number of this Org document.
# When sync is run after the version number has been updated, then VERS
# picks up the newly-changed value.  VERS used to be staticly imbedded
# when the Makefile was tangled, but it needs to be dynamic for
# development.

# QUERY: should this number be formatted like this, or should it be just the numbers?
# The reason it includes them is the S3PROJ obtains the name from the S3 bucket, and
# it includes them.  But it only includes them because I have made it so.  Not a good
# reason just by itself.  The ending slash is not actually a part of the version, but
# comes from the way the 'aws2 ls' command returns its values.	So VERS should probably
# not include the trailing slash, although it doesn’t hurt anything.

VERS := v$(shell $(EDITOR) --eval \
	'(with-current-buffer (find-file-noselect "$(ORG)") \
		(save-excursion \
		  (goto-char (point-min)) \
		  (re-search-forward "^\#[+]\\(?:macro\\|MACRO\\):version Version \\(\\(?:[[:digit:]]+[.]?\\)\\{3\\}\\)") \
		  (match-string-no-properties 1)))')/

### AWS
# PROJ_LIST contains the list of projects currently uploaded to
# the S3 bucket; each item contains the name of the project and its
# current version.

# Created function using elisp instead of the shell.
# This variable contains an elisp list of strings of the form '("proj1-v1.2.3/" "proj2-v4.5.6/" ...)'
# However, when it prints to the shell, the quotes are lost.
# Need to make sure elisp's variable 'exec-path contains the proper $PATH instead of adding to 'exec-path.

PROJ_LIST := $(shell $(EDITOR) --eval \
	"(progn \
		(require (quote seq)) (add-to-list (quote exec-path) (quote \"/usr/local/bin\")) \
		(seq-map (lambda (s) (replace-regexp-in-string \"^\s+PRE \" \"\" s)) \
			(seq-filter (lambda (s) (string-match-p (regexp-quote \" PRE \") s)) \
			(process-lines \"$(AWS)\" \"s3\" \"ls\" \"$(S3_BUCKET)\"))))")

### S3PROJ
# The name of the current project as obtained from S3: 'proj-v1.2.34/'
# If there is no current project in the S3 bucket, then assign a value equal to
# the Org project and version instead.  It is set to the project if found, and
# NO if not found, then updated in the ifeq block below.
S3PROJ := $(shell $(EDITOR) --eval \
		'(let ((proj (seq-find (lambda (s) (string-match-p "$(DIR)" s)) (quote $(PROJ_LIST))))) \
		   (or proj (quote NO)))')

### PROJINS3
# is used by make sync; this allows the index.html file to be generated the first
# time the project is synced.  It is set to NO if this project is not currently in an
# S3 bucket, and it is set to YES if it is.
PROJINS3 :=

### S3VERS
# The version of this project currently installed in the S3 bucket: 'v1.2.34/'
# If there is no current version in the S3 bucket, then assign the version from
# this Org file instead.
S3VERS   :=

# Update S3PROJ, S3VERS, and PROJINS3
ifeq ($(S3PROJ), NO)
	S3PROJ := $(DIR)-$(VERS)
	S3VERS := $(VERS)
	PROJINS3 := NO
else
	S3VERS := $(subst $(DIR)-,,$(S3PROJ))
	PROJINS3 := YES
endif

### GITHUB
# USER is the current user's GitHub login name.

# The user name used to be statically embedded into the Makefile
# during tangle, but in an effort to make the Makefile dynamically
# indepedent, dynamic code has replaced the static code.  The code
# that placed the static name in the Makefile was a 'node' script that
# ran in a separate Org process during tangle.	An unfortunate fact of
# 'make' is that 'make' strips the quote marks from the string
# obtained from the 'curl' command when the 'make shell' command
# returns the string.	 This makes the string malformed JSON and
# unparsable by most JSON parsers, including 'node’.	However,
# 'perl'’s core module JSON::PP (but not JSON::XS) has facilities to
# parse very malformed JSON strings.	Therefore, this dynamic code
# uses 'perl' and the core module JSON::PP to parse the 'curl' string
# into a 'perl' JSON object which can return the login name.	This
# code should work with any version of 'perl' without having to
# install any modules.

USER := $(shell \
	  curl -sH "Authorization: token $(GITHUB_TOKEN)" https://api.github.com/user \
	  | \
	  perl -MJSON::PP -e \
	      '$$/ = ""; \
	       my $$json = JSON::PP->new->loose->allow_barekey->decode(<STDIN>); \
	       print $$json->{login};' \
	  )
SAVE	:= resources

### TEXINFO
TEXI	:= $(PROJ).texi
INFO	:= $(DIR).info
INFOTN	:= $(shell $(EDITOR) --eval "(file-truename \"$(INFO)\")")
PDF	:= $(PROJ).pdf
INDEX	:= index.html
HTML	:= $(DIR)/$(INDEX)
DIR_OLD	:= $(DIR)-old

### AWS S3
DST_OLD	:= $(S3_BUCKET)/$(S3PROJ)
DST_NEW	:= $(S3_BUCKET)/$(DIR)-$(VERS)
EXCL_INCL   := --exclude "*" --include "*.html"
INCL_IMAGES := --exclude "*" --include "*.jpg" --include "*.png"
GRANTS	:= --grants read=uri=http://acs.amazonaws.com/groups/global/AllUsers
S3SYNC	:= $(S3) sync --delete $(EXCL_INCL) $(SRC) $(DST_OLD) $(REGION) $(GRANTS)
S3MOVE	:= $(S3) mv --recursive $(DST_OLD) $(DST_NEW) $(REGION) $(GRANTS)
S3COPY	:= $(S3) cp $(INDEX) $(S3_BUCKET) $(REGION) $(GRANTS)
S3REMOVE:= $(S3) rm $(S3_BUCKET)/$(S3PROJ) --recursive
S3IMAGESYNC := $(S3) sync $(INCL_IMAGES) $(IMAGES) $(S3_BUCKET)/$(IMAGES) $(REGIONS) $(GRANTS)

###############################################################################

default: check texi info html pdf

PHONY: default all check values boot \
	  texi info html pdf \
	  open-org open-texi open-html open-pdf \
	  clean dist-clean wiped-clean \
	  help sync update delete-proj \
	  install-aws-cli \
	  index-html upload-index-html

values: check
	  @printf "$${BLUE}Values...$${CLEAR}\n"
	  @echo TEMPLATE:	$(TEMPLATE)
	  @echo EDITOR:	$(EDITOR)
	  @echo USER:		$(USER)
	  @echo PWD:		$(PWD)
	  @echo ORG:		$(ORG)
	  @echo TEXI:		$(TEXI)
	  @echo INFO:		$(INFO)
	  @ECHO INFOTN:	$(INFOTN)
	  @echo BUCKET:	$(BUCKET)
	  @echo PROJ:		$(PROJ) $S
	  @echo S3_BUCKET:	$(S3_BUCKET)
	  @echo HTTPS_BUCKET:	$(HTTPS_BUCKET)
	  @echo VERS:		$(VERS)
	  @echo S3PROJ:	$(S3PROJ)
	  @echo S3VERS:	$(S3VERS)
	  @echo DIR:		$(DIR)
	  @echo DIR_OLD:	$(DIR_OLD)
	  @echo SRC:		$(SRC)
	  @echo DST_OLD:	$(DST_OLD)
	  @echo DST_NEW:	$(DST_NEW)
	  @echo PROJ_LIST:	"$(PROJ_LIST)"
	  @echo PROJINS3:	$(PROJINS3)

check:
	  @printf "$${BLUE}Checking dependencies...$${CLEAR}\n"

	  @[[ -z $(BUCKET) ]] && \
	     { printf "$${RED}$(BUCKET) $${CYAN}must be set.$${CLEAR}\n"; exit 1; } || \
	     printf "$${CYAN}BUCKET: $${GREEN}$(BUCKET)$${CLEAR}\n";

	  @[[ -z $${GITHUB_TOKEN} ]] && \
	     { printf "$${RED}GITHUB_TOKEN $${CYAN}must be set.$${CLEAR}\n"; exit 1; } || \
	     printf "$${CYAN}GITHUB_TOKEN: $${GREEN}SET$${CLEAR}\n";

	  @[[ (-d ~/.aws) && (-f ~/.aws/credentials) && (-f ~/.aws/config) ]] && \
	     printf "$${CYAN}AWS credentials and config: $${GREEN}SET$${CLEAR}\n" || \
	     { printf "$${RED}~/.aws 'credentials' and 'config' must be set.$${CLEAR}\n"; exit 1; }

	  @[[ "$(shell $(EDITOR) --eval '(member (quote texinfo) org-export-backends)')" = "(texinfo)" ]] && \
		printf "$${CYAN}Texinfo backend: $${GREEN}INSTALLED.$${CLEAR}\n" || \
		{ printf "$${YELLOW}Texinfo backend:$${CLEAR} $${RED}NOT INSTALLED; it must be installed.$${CLEAR}\n"; exit 1; }

	  @[[ $(shell $(EDITOR) --eval '(symbol-value org-confirm-babel-evaluate)') == "t" ]] && \
		{ printf "$${YELLOW}org-confirm-babel-evaluate:$${CLEAR} $${RED}T; set to NIL.$${CLEAR}\n"; exit 1; } || \
		printf "$${CYAN}org-confirm-babel-evaluate: $${GREEN}OFF.$${CLEAR}\n\n"

open-org: $(ORG)
	  @$(EDITOR) -n $(ORG)
$(ORG):
	  @echo 'THERE IS NO $(ORG) FILE!!!'
	  exit 1

texi: $(TEXI)
$(TEXI): $(ORG)
	 @echo Making TEXI...
	 @$(EDITOR) -u --eval \
		"(with-current-buffer (find-file-noselect \"$(ORG)\" t) \
			(save-excursion \
			(org-texinfo-export-to-texinfo)))"
	 @echo Done making TEXI.
open-texi: texi
	 @$(EDITOR) -n $(TEXI)

info: $(INFO)
$(INFO): $(TEXI)
	 @echo Making INFO...
	 @makeinfo -o $(INFO) $(TEXI)
	 @$(EDITOR) -u -eval \
		"(when (get-buffer \"$(INFO)\") \
			(with-current-buffer (get-buffer \"$(INFO)\") \
				(revert-buffer t t t)))"
	 @echo Done making INFO.

open-info: info
	 @$(EDITOR) -u -eval \
		"(if (get-buffer \"*info*\") \
			(with-current-buffer (get-buffer \"*info*\") \
			      (when (not (string= \"(symbol-value (quote Info-current-file))\" \"$(INFOTN)\")) \
				      (info \"$(INFOTN)\")) \
			      (revert-buffer t t t)) \
		    (info \"$(INFOTN)\"))"

html: $(HTML)
$(HTML): $(TEXI)
	 @echo Making HTML INFO..
	 @makeinfo --html -o $(DIR) $(TEXI)
	 @echo Done making HTML.
	 $(CMPRPL) $(DIR) $(DIR_OLD)
open-html: html
	 @open $(HTML)

# If pdftexi2dvi produces an error, it may still produce a viable PDF;
# therefore, use --tidy.  If it produces an error, try to link the PDF;
# if it does not produce an error, the PDF will be added to the top dir
# and there will be no attempt to link.
pdf:	$(PDF)
$(PDF): $(TEXI)
	@echo Making PDF INFO...
	@-pdftexi2dvi --quiet --build=tidy $(TEXI) || ln -s $(PROJ).t2d/pdf/build/$(PDF) $(PDF)
	@echo Done making PDF.
open-pdf:pdf
	 @open $(PDF)

sync:   $(HTML)
	@echo Syncing version $(VERS) onto $(S3VERS)...
	$(S3SYNC)
	$(S3IMAGESYNC)
	@echo Done syncing.
	[[ $(VERS) != $(S3VERS) ]] && { echo Moving...; $(S3MOVE); echo Done moving.;  make homepage; } || :
	[[ $(PROJINS3) = "NO" ]] && make homepage || :

# This is a target-specific variable for updating the “description”
# key on the GitHub repo page with the current version number.  It
# first makes a curl call to the GitHub project repo, finds the
# “description” line, pulls out the description only (leaving the old
# version) and then prints the value with the current version number.
# This value is used by the “homepage:” target in the PATCH call.
# This method is arguably harder to code but faster to run than using
# Perl with the JSON::PP module.

homepage: description = $(shell \
	curl -s \
		-H "Authorization: token $(GITHUB_TOKEN)" \
		https://api.github.com/repos/$(USER)/$(PROJ)$S | \
		(perl -ne 'if (/^\s*\"description\":\s*\"(.*): v(?:(?:[[:digit:]]+[.]?){3})/) {print $$1}'))

### NOTE the use of the S variable at the end of PROJ; this is to handle
# the singular case of the GitHub repo using the plural form, Templates
# whereas the the Template.org file uses the singular form.
homepage: $(ORG) upload-index-html
	  @echo Updating homepage...
	  @echo DESCRIPTION: $(description)
	  @echo VERS: $(VERS)
	  @curl -i \
		-H "Authorization: token $(GITHUB_TOKEN)" \
		-H "Content-Type: application/json" \
		-X PATCH \
		-d "{\"homepage\":\"$(HTTPS_BUCKET)/$(DIR)-$(VERS)\",\
		     \"description\":\"$(description): $(VERS)\"}" \
		https://api.github.com/repos/$(USER)/$(PROJ)$S
	  @echo Done updating homepage.

delete-proj:
	@echo Deleting project $(PROJ)...
	@curl -i \
		-H "Authorization: token $(GITHUB_TOKEN)" \
		-H "Accept: application/vnd.github.v3+json" \
		-X DELETE \
		https://api.github.com/repos/$(USER)/$(PROJ)$S
	@$(S3REMOVE)
	@make dist-clean
	@make upload-index-html
	@$(EDITOR) -u --eval "(kill-buffer \"$(ORG)\")"
	@rm -rf "../$(PROJ)"
	@echo Done deleting project.

index-html: $(INDEX)
$(INDEX): $(ORG)
	@echo making index.html...
	$(EDITOR) --eval \
	"(with-current-buffer (find-file-noselect \"$(ORG)\") \
		(save-excursion \
		  (org-link-search \"#project-index-title\") \
		  (org-export-to-file (quote html) \"index.html\" nil t)))"
	@echo Done making index.html.

upload-index-html: $(INDEX)
	 @echo Uploading index.html...
	 $(S3COPY)
	 @echo Done uploading index.html

install-aws-cli:
	  curl "https://awscli.amazonaws.com/AWSCLIV2.pkg" -o "AWSCLIV2.pkg" && \
	  sudo installer -pkg AWSCLIV2.pkg -target / && \
	  which aws && aws --version
	  rm -rf AWSCLIV2.pkg

clean:
	@echo Cleaning...
	  -@rm *~ 2>/dev/null
	  -@for file in *.??*; \
	  do \
		  ext=$${file#$(PROJ).}; \
		  [[ ! $${ext} =~ org|texi|info|pdf|html ]] && rm -rv $${file}; \
	  done

dist-clean: clean
	@echo Dist Cleaning...
	  @${EDITOR} -u --eval \
	    "(kill-buffer \"$(ORG)\")"
	  -@rm -rf *.{texi*,info*,html*,pdf*} $(DIR) $(TOOLS)
	  -@for dir in *; \
	      do \
		  [ -d $$dir -a $$dir != "$(DIR_OLD)" -a $$dir != $(SAVE) ] && \
		  rm -vr $$dir; \
	      done

wipe-clean: dist-clean
	@echo Wipe Clean...
	  -@rm -rf Makefile Readme.md $(DIR_OLD)
	  @git checkout Makefile README.md

git-ready: dist-clean
	  git checkout Makefile
	  git checkout README.md
	  git status

help:
	  @echo '"make boot" tangles all of the files in Template'
	  @echo '"make default" makes the .texi file, the .info file, \
	  the html files, and the .pdf file.'
	  @echo

	  @echo '"make check" checks for prerequistes'
	  @echo '"make values" runs check and prints variable values'
	  @echo

	  @echo '"make texi" makes the .texi file'
	  @echo '"make info" makes the .info file'
	  @echo '"make html" makes the html distribution in a subdirectory'
	  @echo '"make pdf" makes the .pdf file'
	  @echo

	  @echo '"make open-org" opens the ORG program using emacsclient for editing'
	  @echo '"make open-texi" opens the .texi file using emacsclient for review'
	  @echo '"make open-html" opens the distribution index.html file \
	  in the default web browser'
	  @echo '"make open-pdf" opens the .pdf file'
	  @echo

	  @echo '"make sync" syncs the html files in the AWS S3 bucket BUCKET; \
	  you must have your AWS S3 bucket name in the env var AWS_S3_BUCKET; \
	  You must have your AWS credentials installed in ~/.aws/credentials'
	  @echo

	  @echo '"make install-aws-cli" installs the "aws cli v2" command-line tools'
	  @echo 'You also need to run "aws configure" and supply your Access Key and Secret Access Key'
	  @echo

	  @echo '"make clean" removes the .texi, .info, and backup files ("*~")'
	  @echo '"make dist-clean" cleans, removes the html distribution, \
	  and removes the build directory'
	  @echo '"make wipe-clean" wipes clean the directory, including old directories'
	  @echo

	  @echo '"make delete-proj" deletes the project from the file system, GitHub and AWS'

boot:
	$(EDITOR) -u --eval \
		"(with-current-buffer (car (find-file-noselect \"./*.org\" nil nil t)) \
			(goto-char (point-min)) \
			(re-search-forward \"^#[+]name:preprocess.el$$\") \
			(org-babel-tangle (quote (4))) \
                        (save-buffer) \
			(kill-buffer))"
	./tools/preprocess.el
