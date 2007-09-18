### --------------------------------------------------------------------
### Created:  16 Oct 1998 by tobbe@serc.rmit.edu.au
###
### $Id: Makefile,v 1.1.1.1 1999/09/05 09:54:01 tobbe Exp $
### --------------------------------------------------------------------

# Change path if not the default compiler shall be used
ERLC = erlc

ERL_FILES = eppp.erl eppp_lcp.erl eppp_lcp_gen.erl eppp_hdlc.erl \
	eppp_chat.erl eppp_serial.erl eppp_pred.erl eppp_log.erl \
	test.erl

ALL_FILES  = $(ERL_FILES) Makefile Makefile.serial TODO
JAM_FILES  = $(ERL_FILES:.erl=.jam)
BEAM_FILES = $(ERL_FILES:.erl=.beam)

.SUFFIXES: .3 .html .sgml .jam .beam .hrl .erl

.erl.jam:
	-$(ERLC) -W -b jam $<

.erl.beam:
	-$(ERLC) -W -b beam $< 

beam: $(BEAM_FILES)
	@echo " *** Compilation finished ***"

jam: $(JAM_FILES)
	@echo " *** Compilation finished ***"

serial:
	$(MAKE) -f Makefile.serial

sgml: 
	-sgml_transform -m 3 m.sgml
	-sgml_transform -html m.sgml
	@echo " *** SGML processing finished ***"

clean:
	rm -f *.jam *.beam *sgmls*
