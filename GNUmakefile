PROJECT_NAME=DOTEMACS

default: help

Cask:
	@if [ ! -e "Cask" ]; then echo "Cask file does not exist"; exit 1; fi

.PHONY: cask_install
cask_install : ## Install packages in Cask file
cask_install: Cask
	cask install

.PHONY: cask_ubuntu
cask_ubuntu : ## Generate Cask file for Ubuntu
cask_ubuntu:
	cp ./configs/Cask .
	@./bin/scripts/generate_cask_file.sh Ubuntu Cask

.PHONY: cask_ubuntu_current_emacs
cask_ubuntu_current_emacs : ## Generate Cask file for Ubuntu with current emacs version
cask_ubuntu_current_emacs:
	cp ./configs/Cask .
	@./bin/scripts/generate_cask_file.sh Ubuntu_current_emacs Cask

.PHONY: cask_freebsd
cask_freebsd : ## Generate Cask file for FreeBSD
cask_freebsd:
	cp ./configs/Cask ./Cask
	@./bin/scripts/generate_cask_file.sh FreeBSD Cask

.PHONY: clean
clean : ## Clean up
clean:
	@echo "Cleaning..."
	rm Cask

.PHONY: help
help           : ## @Help Show this help menu
	@echo "-----------------------------------------------------------------"
	@echo "|                         $(PROJECT_NAME)                              |"
	@echo "-----------------------------------------------------------------"
	@echo "  Available make targets"
	@echo "-----------------------------------------------------------------"
	@fgrep -h "##" $(MAKEFILE_LIST) | fgrep -v fgrep | sed -rn 's/([a-zA-Z]+)[ ]+:[ ]##[ ]?(@[a-zA-Z]+)?[ ]*(.+)/\1 "\3"/p' | xargs printf " %-25s    : %5s\n"
	@echo
