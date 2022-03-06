.SUFFIXES: .s
.SUFFIXES: .xex
c:
	mads movplay.s -o:MOVPLAY

%.xex: %.s
	mads -l $< -o:$@

build: c
	exomizer sfx 0x2800 -C -n -t 168 MOVPLAY -o MOVPLAY
	# exomizer sfx sys -Di_load_addr=0xc00 -Datari_init=1 ai.xex -t 168 -n -o aic.xex

cp: build
	while ! [ -d /Volumes/UNTITLED ] ; do sleep 1 ; done
	sleep 1
	cp AVFPLAY /Volumes/ATARI/ATARI/MOVPLAY
	eject || true
cpb: 
	while ! [ -d /Volumes/UNTITLED ] ; do sleep 1 ; done
	sleep 1
	cp AVFPLAY /Volumes/ATARI/ATARI/MOVPLAY
	eject || true
