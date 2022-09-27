# main default rule
#
BETA=1
PROD=0
RELEASE=$(PROD)
c: c_side c_incognito

date.inc:
	date -r `git show --format=%ct | head -1 ` +'%d.%m.%Y' | tr -d $$'\n' >$@

c_side: date.inc
	dir=bin/side/pokey ;\
	mkdir -p $$dir;\
	mads movplay.s -bc -d:CODE=1 -d:RELEASE=$(RELEASE) -d:COVOX=0 -o:$$dir/MOVPLAY.XEX
	for covox in D280 D300 D500 D600 D700 ; do  \
	dir=bin/side/covox_$$covox;  \
	mkdir -p $$dir;\
	mads movplay.s -bc -d:CODE=1 -d:RELEASE=$(RELEASE) -d:COVOX=0x$$covox -o:$$dir/MOVPLAY.XEX ;\
	done

c_incognito: date.inc
	dir=bin/incognito/pokey ;\
	mkdir -p $$dir;\
	mads movplay.s -bc -d:CODE=2 -d:RELEASE=$(RELEASE) -d:COVOX=0 -o:$$dir/MOVPLAY.XEX
	for covox in D280 D300 D500 D600 D700 ; do  \
	dir=bin/incognito/covox_$$covox;  \
	mkdir -p $$dir;\
	mads movplay.s -bc -d:CODE=2 -d:RELEASE=$(RELEASE) -d:COVOX=0x$$covox -o:$$dir/MOVPLAY.XEX ;\
	done

# general rule, sometimes helpful, so here it is.
%.xex: %.s
	mads -l $< -bc -o:$@

build: c
	exomizer sfx 0x2800 -C -n -t 168 MOVPLAY -o MOVPLAY
	# exomizer sfx sys -Di_load_addr=0xc00 -Datari_init=1 ai.xex -t 168 -n -o aic.xex

# local helpers
cp: build
	while ! [ -d /Volumes/ATARI ] ; do sleep 1 ; done
	sleep 1
	cp MOVPLAY /Volumes/ATARI/ATARI/MOVPLAY.XEX
	eject || true
cpb: 
	while ! [ -d /Volumes/ATARI ] ; do sleep 1 ; done
	sleep 1
	cp MOVPLAY /Volumes/ATARI/ATARI/MOVPLAY.XEX
	eject || true

atr: c
	cp bin/MOVPLAY_SIDE.XEX bin/MOVPLAY.XEX
	unix2atr -p 720 MOVPLAY.ATR bin
	atari800 "Dos II+ D 6.4.atr" MOVPLAY.ATR

