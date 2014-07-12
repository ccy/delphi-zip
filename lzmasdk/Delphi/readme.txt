LZMA (Lempel–Ziv–Markov chain algorithm) compression algorithm was first used in 7Zip archiver.  

For more information about the LZMA SDK. Visit: http://www.7-zip.org/sdk.html

Compile Embarcadero RAD Studio with C++ Builder:

  bcc32.exe -c -D_LZMA_PROB32 -D_WIN32 -v -y Threads.c LzFind.c LzFindMt.c LzmaDec.c LzmaEnc.c

Known problem: The Pascal version is not ready for Win64 platform yet.