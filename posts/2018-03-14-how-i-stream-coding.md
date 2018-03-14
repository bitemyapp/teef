---
title: How I stream myself coding
tags: programming streaming
---

Alright so I've been uploading streamed videos of myself working on programming projects for [awhile now](https://youtube.com/c/bitemyapp). I occasionally get asked about my setup for this, so I thought I would explain the tools I use. This might be particularly valuable as I am primarily a Linux user where some of the kit for this can be kinda rough. Further, I am very picky about my tools and ergonomics so I didn't really want to make any sacrifices there in order to stream.

<!--more-->

## The tools

- OBS Studio (open broadcaster): I always live stream to Twitch.tv and save to a file simultaneously.

- Hangouts: I pull people into Hangouts that want to ask questions and to be able to see my stream without a time delay. They are not seen nor heard in the actual stream. I repeat their questions for the rest of the stream when I'm going to answer it.

- VLC: I use this to slice out my second monitor only for the Hangout stream. Hangouts doesn't know how to "see" a single monitor, but it can see individual applications. With that, I can just pick VLC in order to share a single monitor. [Here's my VLC script for grabbing my middle monitor](https://github.com/bitemyapp/dotfiles/blob/master/.vlc-snd-monitor.sh).

- Slack: Announce the stream, occasionally get comments from people.

- Twitch.tv: live streams go here. Their live streaming and tools around it were better than YouTube's.

- YouTube: my video uploads go here. I process the file upload.

- Three 23" 1080p monitors: This is actually necessary for how I do this. Here's why:

* Left monitor (not streamed): Monitoring Twitch.tv stream and chat in case any viewers ask questions. OBS Studio. Slack. Odds and ends.

* Middle monitor (this is what gets streamed): usually my browser workspace (the first workspace in my XMonad configuration) or my Emacs + terminal workspace (the second workspace).

* Right monitor (not streamed): This is devoted to VLC because I need a whole monitor to recap the middle monitor for Hangouts. This can be ditched if I'm not doing Hangouts as OBS knows how to grab a single monitor. However, I often find a friend or colleague will jump into Hangouts mid-stream so I habitually maintain the same setup so there's minimal delay mid-stream getting them in.

- XMonad: absolutely necessary for me to work efficiently. Lets me instantly switch workspaces on my middle monitor without perturbing my VLC workspace or my 'dashboard' (OBS/Hangouts/Twitch/etc.) workspace.

- Heil PR-40 microphone and a Xenyx Q802. Not strictly necessary but stay the hell away from Blue microphones, they're all irredeemable trash. You're better off using a built-in mic or a webcam mic. A good middle option is a solid dynamic mic like a Shure SM58 with a USB mixer like my Xenyx. I have not actually tested a Shure SM57 or SM58 with my recording setup but a dynamic mic in general should be much better for your typical home streamer. Condenser mics seem to pick up a lot of extraneous noise that is hard to filter out or control for in a home environment. The Blue Yeti would pick up even relatively subtle background noises like the hum of my air conditioning. I had similar issues with other cheap or "cheap" condenser mics until I did some research and found out about dynamic microphones.

- Turn on mic monitoring. Hear yourself speak. It'll help train you out of bad vocal tics and mouth sounds. If you've ever pair-programmed with somebody that makes obnoxious mouth sounds then you know how important this is for your streams.

- Play any background music into your headphones quietly and _DO NOT_ rebroadcast it onto the stream. Almost all of my headphones are open-air cans. This means they aggressively leak sound. I've had YouTube yank my videos for copyright claims because a 15 second snippet of a song leaked into the mic recording. Listening to music from speakers is obviously a hard-no. Don't annoy your viewers.

- If possible, use a carpeted room. My home office (I exclusively work from home) is carpeted.

After the stream is done, I process the file saved by OBS with some denoising scripts to smooth out the audio. Here's a dump of a couple of scripts I've kicked around:

`normalize.sh`

```bash
VIDEO_FILE=$1
VIDEO_FILE_FIXED=${VIDEO_FILE%.*}-fixed.${VIDEO_FILE##*.}

rm -f audio.wav
avconv -i $VIDEO_FILE -c:a pcm_s16le -vn audio.wav
# normalize-audio -a -9dbFS audio.wav
normalize-audio -a -12dbFS audio.wav
avconv -i $VIDEO_FILE -i audio.wav -map 0:0 -map 1:0 -c:v copy -c:a libvo_aacenc \
   $VIDEO_FILE_FIXED
```

`noise_reduction.sh`

```bash
VIDEO_FILE=$1
VIDEO_FILE_VIDEO=${VIDEO_FILE%.*}-video.${VIDEO_FILE##*.}
VIDEO_FILE_AUDIO=${VIDEO_FILE%.*}-audio.wav
# VIDEO_FILE_AUDIO_CLEAN=${VIDEO_FILE%.*}-audio-clean.wav
VIDEO_FILE_AUDIO_CLEAN=${VIDEO_FILE%.*}-audio-noisered.wav
VIDEO_FILE_DENOISED=${VIDEO_FILE%.*}-denoised.${VIDEO_FILE##*.}
QSCALE=10

# ffmpeg -i $VIDEO_FILE -af "highpass=f=200, lowpass=f=3000" $VIDEO_FILE_DENOISED
rm -f noise_audio.wav

# ffmpeg -i $VIDEO_FILE -qscale:v $QSCALE -an $VIDEO_FILE_VIDEO
# ffmpeg -i $VIDEO_FILE -qscale:v $QSCALE $VIDEO_FILE_AUDIO
# ffmpeg -i $VIDEO_FILE -vn -ss 0:00:00 -t 0:00:02 noise_audio.wav
# ffmpeg -i $VIDEO_FILE -vn -ss 0:33:30 -t 0:33:33 noise_audio.wav
# sox noise_audio.wav -n noiseprof noise.prof
# sox $VIDEO_FILE_AUDIO $VIDEO_FILE_AUDIO_CLEAN noisered noise.prof 0.21
# sox $VIDEO_FILE_AUDIO $VIDEO_FILE_AUDIO_CLEAN noisered noise.prof 0.15
# sox $VIDEO_FILE_AUDIO $VIDEO_FILE_AUDIO_CLEAN noisered noise.prof 0.10
ffmpeg -i $VIDEO_FILE_AUDIO_CLEAN -i $VIDEO_FILE_VIDEO -qscale:v $QSCALE $VIDEO_FILE_DENOISED
```

If you've got better kit/scripts than I've mentioned here, please speak up! I'm always up for improving this stuff. Please contact me if you need clarification on something I've mentioned here or if you have questions.

### Comment on microphones from my friend Tyler on Twitter

I've lightly edited his words here:

>I take issue with your statement about blue microphones. I had a Shure SM58 and a blue spark and I liked the blue much better than the SM58. I prefer condenser microphones over dynamic microphones, I just run the gain as low as I can to make it not pick up the background. USB mics are trash.

>I never messed with USB mics. I've exclusively had XLR microphones since I got started back in 2011. My little brother has a Blue Icicle and that is not the best experience. Constantly picking up his keyboard.

>If you can pick you a Shure SM58 for 50 bucks like I did, the thing is nigh indestructible, great deal. The only reason mine doesn't work is because I broke a solder point and I didn't want to repair it.

>It looks like they refreshed the Blue Spark with a high pass filter and a -20db pad.

My reply:

>FWIW I tried setting the gain low on my Blue Yeti, it wasn't enough. I had to scream at the mic when the gain was low enough not to pick up noise.

~At time of writing, it doesn't seem like the Blue Spark is available any more.~ (See above about the model refresh)

It's possible an XLR condenser mic with a mixer could be okay for your purposes, but most of the Blue mics are USB now which severely limits their usefulness. Not my bag of chips but I wanted to include a different viewpoint.
