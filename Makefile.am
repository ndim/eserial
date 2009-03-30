epppbindir          = $(ERLANG_INSTALL_LIB_DIR_eserial)/bin
epppebindir         = $(ERLANG_INSTALL_LIB_DIR_eserial)/ebin
epppsrcdir          = $(ERLANG_INSTALL_LIB_DIR_eserial)/src

CLEANFILES          =
EXTRA_DIST          =
EXTRA_DIST         += dot_eppp.conf
EXTRA_DIST         += INSTALL
EXTRA_DIST         += LICENSE
EXTRA_DIST         += README.rst

AM_ERLCFLAGS        =
AM_ERLCFLAGS       += -W
AM_ERLCFLAGS       += -b beam
AM_ERLCFLAGS       += -pa src

epppbin_PROGRAMS    =
epppebin_DATA       =
epppsrc_DATA        =
dist_epppsrc_DATA   =
noinst_DATA         =

epppbin_PROGRAMS   += eppp_serial
eppp_serial_SOURCES = c_src/eppp_serial.c c_src/eppp_serial.h

CLEANFILES         += src/eppp_serial.beam
epppebin_DATA      += src/eppp_serial.beam
dist_epppsrc_DATA  += src/eppp_serial.erl

if !EPPP_SERIAL_ONLY

CLEANFILES         += src/eppp_log.beam
epppebin_DATA      += src/eppp_log.beam
dist_epppsrc_DATA  += src/eppp_log.erl

CLEANFILES         += src/eppp_conf.beam
epppebin_DATA      += src/eppp_conf.beam
dist_epppsrc_DATA  += src/eppp_conf.erl

CLEANFILES         += src/eppp.beam
epppebin_DATA      += src/eppp.beam
dist_epppsrc_DATA  += src/eppp.erl

CLEANFILES         += src/eppp_lcp.beam
epppebin_DATA      += src/eppp_lcp.beam
dist_epppsrc_DATA  += src/eppp_lcp.erl

CLEANFILES         += src/eppp_lcp_gen.beam
epppebin_DATA      += src/eppp_lcp_gen.beam
dist_epppsrc_DATA  += src/eppp_lcp_gen.erl

CLEANFILES         += src/eppp_hdlc.beam
epppebin_DATA      += src/eppp_hdlc.beam
dist_epppsrc_DATA  += src/eppp_hdlc.erl

CLEANFILES         += src/echat.beam
epppebin_DATA      += src/echat.beam
dist_epppsrc_DATA  += src/echat.erl

CLEANFILES         += src/echat.beam
epppebin_DATA      += src/echat.beam
dist_epppsrc_DATA  += src/echat.erl

CLEANFILES         += src/eppp_pred.beam
epppebin_DATA      += src/eppp_pred.beam
dist_epppsrc_DATA  += src/eppp_pred.erl

CLEANFILES         += src/test.beam
noinst_DATA        += src/test.beam

endif

EXTRA_DIST         += src/chat.erl
EXTRA_DIST         += src/epap.erl
EXTRA_DIST         += src/euppp.erl
EXTRA_DIST         += src/test.erl

EXTRA_DIST         += src/Makefile
EXTRA_DIST         += src/Makefile.serial


SUFFIXES = .beam .erl

.erl.beam:
	@mkdir -p src
	$(ERLC) -o src $(AM_ERLCFLAGS) $(ERLCFLAGS) $<
