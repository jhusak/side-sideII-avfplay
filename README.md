MOVPLAY

This is the player for SIDE/SIDEII cartridges for Atari 8-bit Computers.

The player replays avf files written block by block to CF card.

The player originally written by Phaeron (Avery Lee), link: https://atariage.com/forums/topic/211689-60-fps-video-using-side-2/page/4/?tab=comments#comment-2796714
has been tweaked by me to play sound samples exactly in the same cycle of frame. Sound replay method is PCM (Pulse Code Modulation), which gives 108 allowable levels of sound, which is almost 7 bit (128 levels). However, the timing is crucial, because shifting one sample one cycle forward gives two levels distortion (so it is only merely 6 bits signal-noise distance).

The cycling was made on AVGCart version of player, then cycling was transferred to this code. Sounds well, but I have had no tools to be sure in 100% the cycles are right.

The Altirra Atari Emulator by Avery Lee was used to check every sample plays at the right time. Then "silent" samples were played and amplified to check irregularities. The digital noise of Atari (mainly coming from ANTIC) may have influence on sound and narrow the dynamics to 5-6 bits when high contrasts are used.

But we have a little possibilities to fix this, because Atari was not designed to produce such low noise when so many things are going on its digital side.

Happy testing (feel free to raise bugs)

Jakub Husak

