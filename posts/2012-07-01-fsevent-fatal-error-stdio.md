---
title: OS X fsevent fatal error 'stdio.h' file not found
---

Getting the following error on Mac OS X?

```
fsevent/fsevent_watch.c:1:10: fatal error: 'stdio.h' file not found
```

XCode 4.3 and later does not install unix tools, so the usual header files are missing.

The following bit of magic is needed:

```
sudo xcode-select -switch /Applications/Xcode.app/Contents/Developer/Platforms/MacOSX.platform/Developer/
```

Re-run your gem install / bundle and it should work now.
