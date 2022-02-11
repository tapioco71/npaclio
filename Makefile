# -*- mode:makefile-gmake; coding:utf-8; indent-tabs-mode:t -*-

NAME=cl-network-programming
PDFS=$(NAME).pdf

all: documents
documents: $(PDFS)

include tools/common.make

.PHONY:: all

-include $(NAME).d
clean::
	@if [ 'x${VERBOSE}' = x ];		\
	then					\
		echo " [ clean ] $(NAME).d ";	\
	else					\
		echo " -rm $(NAME).d ";		\
	fi
	@rm $(NAME).d

