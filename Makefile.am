include guile.am

SOURCES =					\
	librepoll/chart.scm			\
	librepoll/sql.scm			\
	librepoll/create-db.scm			\
	librepoll/tools.scm

EXTRA_DIST =					\
	README.org				\
	LICENSE					\
	bootstrap				\
	.dir-locals.el

assetsdir = $(datadir)/@PACKAGE@
nobase_dist_assets_DATA =			\
	assets/default/main.css			\
	assets/blue/main.css 			\
	assets/purple/main.css 			\
	assets/none/main.css 			\
	librepoll.json

install-exec-hook:
	mkdir $(DESTDIR)$(bindir) -p
	$(INSTALL) run $(DESTDIR)$(bindir)/run-librepoll

all-local: run
	$(AM_V_GEN)$(GUILD) compile $(GUILD_COMPILE_FLAGS)	\
	  -L "$(top_srcdir)" -L "$(top_builddir)"		\
	  -o "$(godir)/run-librepoll.go"			\
	  run
