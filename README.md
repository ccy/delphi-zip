delphi-zip
==========
This is an enhancement or patch to original [Embarcadero RAD Studio](http://www.embarcadero.com/products/rad-studio) [System.Zip.pas](http://docwiki.embarcadero.com/Libraries/XE6/en/System.Zip) unit.

The original *System.Zip.pas* lacks the following features:
- No support of progressing event during compress and decompress operations.
- No support of Zip64 and files beyond 4GiB boundary shall fail.
- No support of NTFS time stamp in ZIP archive
- No support of [LZMA](http://en.wikipedia.org/wiki/Lempel%E2%80%93Ziv%E2%80%93Markov_chain_algorithm)  compressing algorithm

The above features has been addressed, enhance and patch.

To avoid duplicate naming conflict with original *System.Zip.pas* unit, a new unit *System.Zip2.pas* is defined for the enhancements.
