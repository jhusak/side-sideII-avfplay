# MOVPLAY for SIDE/SIDEII cartridges

This is the player for SIDE/SIDEII cartridges for Atari 8-bit Computers. Additionaly, after changing the destination compatible IDE device, the player will play movies from CF put in INCOGNITO board (for binaries look into "bin" catalog)

The player replays avf files written block by block to CF card.

## New features:
- sound quality best achievable on POKEY ANTIC and GTIA turned on the same time
- fast 120x forward and rewind
- shortcut to go to beginning
- volume up/down
- pause - reads whole frame to buffer, then displays int from RAM.
- COVOX under addressees $D280, $D500, $D600, $D700 (binaries with apriopriate names, look into bin catalog), mono, both channels left and right.
- DOS friendly, may be launched from practical any DOS (tested on 6.4 and Sparta), load address $2800, exits cleanly do DOS.
- ESC key returns to the place movplay was started in (to DOS, selftest, etc)

but you must prepare CF card as before - write raw data to it.

## Varia and technical information

The player originally written by Phaeron (Avery Lee), link: https://atariage.com/forums/topic/211689-60-fps-video-using-side-2/page/4/?tab=comments#comment-2796714 playing samples on POKEY chip with fairly new method PWM (Pulse Width Modulation) has been tweaked by me to play sound samples exactly in the same cycle of frame. PWM method gives about 106 allowable levels of sound, which is almost 7 bit (128 levels). However, the timing is crucial, because shifting one sample one cycle forward gives two levels distortion (so it is only merely 6 bits signal-noise distance) - theoretically this should be not noticable in PWM, but in fact it is.

The cycling was made on AVGCart version of player, then cycling was transferred to this code. 

The Altirra Atari Emulator by Avery Lee was used to check every sample plays at the right time. Then "silent" samples were played and amplified to check irregularities (none found). The digital noise of Atari (mainly coming from ANTIC) may have influence on sound and narrow the dynamics to 5-6 bits when high contrasts are used.

The advantage of PWM method is that it is somehow prone to electrical hum produced by moving electrons and electric/magnetic fields inside the computer. When you switch to the COVOX version, the electrical noise is more audible due to more "analog" nature of COVOX. The second is that you get programatically free volume adjustment by setting the static volume of POKEY channel. The disadvantage important to our children and pets is that 15.6 khz whistle is audible for them and annoying.

Well, TV ingeneers were too selfish - when THEY do not hear anythig from TV set, it is good.

Happy testing (feel free to raise issues:)

Jakub Husak

