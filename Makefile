# Copyright (c) 2023 Simon Dobson <simoninireland@gmail.com>

# Author: Simon Dobson <simoninireland@gmail.com>
# Maintainer: Simon Dobson <simoninireland@gmail.com>

# This file is NOT part of GNU Emacs.
#
# GNU Emacs is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.

# GNU Emacs is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.

# You should have received a copy of the GNU General Public License
# along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

# Package metadata
PACKAGE = remarkable
VERSION = 0.1.1
SHORT_DESCRIPTION = Link from Org mode to the reMarkable tablet

# Source files
SOURCES_FILES = \
	remarkable.el \
	jwt.el \
	remarkable-utils.el \
	remarkable-cache.el \
	remarkable-cloud-sync15.el \
	remarkable-mode.el \
	remarkable-org.el

# Unit tests
SOURCES_TESTS =

# Other files
SOURCES_OTHER = \
	Makefile \
	LICENCE \
	Cask.in

# Tools
EMACS = emacs
CASK = cask
RM = rm -fr
SED = sed
CAT = cat

# Virtual environment
VENV = .cask
CASK_FILES = $(patsubst %.el, "%.el", $(SOURCES_FILES))

# Constructed tools
RUN_EMACS = $(CASK) exec $(EMACS) -Q -batch -L "."

# Top-level targets

# Build virtual environment
.PHONY: env
env: $(VENV)

$(VENV): Cask
	$(CASK) install

.PHONY: test
test: env
	$(RUN_EMACS) \
	$(SOURCES_TESTS:%=-l %) \
	--eval "(let ((ert-quiet t)) (ert-run-tests-batch-and-exit))"

.PHONY: lint
lint: env
	$(RUN_EMACS) \
	--eval "(progn (require 'package-lint)(package-lint-batch-and-exit))" \
	$(SOURCES_FILES)

# Clean up the build
.PHONY: clean
clean:
	$(RM) $(VENV) Cask

# Constructed files
Cask: Cask.in Makefile
	$(CAT) Cask.in | $(SED) -e 's/PACKAGE/$(PACKAGE)/g' \
				-e 's/VERSION/$(VERSION)/g' \
				-e 's/SHORT_DESCRIPTION/$(SHORT_DESCRIPTION)/g' \
				-e 's/FILES/$(CASK_FILES)/g' >Cask
