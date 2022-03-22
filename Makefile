# main default rule
c: c_side c_incognito

c_side:
	mads movplay.s -d:CODE=1 -o:bin/MOVPLAY_SIDE.XEX

c_incognito:
	mads movplay.s -d:CODE=2 -o:bin/MOVPLAY_INCOGNITO.XEX

# general rule, sometimes helpful, so here it is.
%.xex: %.s
	mads -l $< -o:$@

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
