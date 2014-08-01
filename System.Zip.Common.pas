unit System.Zip.Common;

interface

type
  TCompressEvent = reference to procedure(Sender: TObject; const aPosition, aSize, aCompressedSize: UInt64);
  TDecompressEvent = reference to procedure(Sender: TObject; const aPosition, aSize: UInt64);

implementation

end.
