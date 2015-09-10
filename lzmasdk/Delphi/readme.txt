LZMA (Lempel–Ziv–Markov chain algorithm) compression algorithm was first used in 7Zip archiver.  

For more information about the LZMA SDK. Visit: http://www.7-zip.org/sdk.html

Compile with Embarcadero RAD Studio C++ Builder Win32:

  bcc32.exe -c -q -D_LZMA_PROB32 -D_WIN32 Threads.c LzFind.c LzFindMt.c LzmaDec.c LzmaEnc.c

To compile Win64 platform, use XE6 and above:

  bcc64.exe -c -q -D_LZMA_PROB32 -D_WIN64 Threads.c LzFind.c LzFindMt.c LzmaDec.c LzmaEnc.c