include guile.am

SOURCES =

EXTRA_DIST =					\
	README.org				\
	LICENSE					\
	bootstrap				\
	.dir-locals.el

assetsdir = $(datadir)/@PACKAGE@
nobase_dist_assets_DATA =			\
	assets/default/main.css			\
	librepoll.json

install-exec-hook:
	mkdir $(DESTDIR)$(bindir) -p
	$(INSTALL) librepoll $(DESTDIR)$(bindir)/librepoll

all-local: librepoll
	$(AM_V_GEN)$(GUILD) compile $(GUILD_COMPILE_FLAGS)	\
	  -L "$(top_srcdir)" -L "$(top_builddir)" librepoll
