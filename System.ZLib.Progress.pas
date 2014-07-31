unit System.ZLib.Progress;

interface

uses
  System.Classes, System.ZLib, System.Zip.Common;

type
  TZCompressionStream2 = class(TZCompressionStream)
  private
    FDest: TStream;
    FProgress: TCompressEvent;
    FSource: TStream;
    procedure DoProgress(Sender: TObject); reintroduce;
  public
    constructor Create(dest: TStream; compressionLevel: TZCompressionLevel;
        windowBits: Integer; const aProgress: TCompressEvent); overload;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  TZDecompressionStream2 = class(TZDecompressionStream)
  private
    FProgress: TDecompressEvent;
    FSource: TStream;
    procedure DoProgress(Sender: TObject); reintroduce;
  public
    constructor Create(source: TStream; WindowBits: Integer; const aProgress:
        TDecompressEvent); overload;
  end;

implementation

constructor TZCompressionStream2.Create(dest: TStream; compressionLevel:
    TZCompressionLevel; windowBits: Integer; const aProgress: TCompressEvent);
begin
  inherited Create(dest, compressionLevel, windowBits);
  FDest := dest;
  FProgress := aProgress;

  OnProgress := DoProgress;
end;

procedure TZCompressionStream2.DoProgress(Sender: TObject);
begin
  if Assigned(FProgress) and Assigned(FSource) then
    FProgress(Sender, FSource.Position, FSource.Size, FDest.Size);
end;

function TZCompressionStream2.Write(const Buffer; Count: Integer): Longint;
begin
  FSource := TStream(Buffer);
  try
    Result := CopyFrom(FSource, FSource.Size);
  finally
    FSource := nil;
  end;
end;

constructor TZDecompressionStream2.Create(source: TStream; WindowBits:
    Integer; const aProgress: TDecompressEvent);
begin
  inherited Create(source, WindowBits);
  FSource := source;
  FProgress := aProgress;

  OnProgress := DoProgress;
end;

procedure TZDecompressionStream2.DoProgress(Sender: TObject);
begin
  if Assigned(FProgress) and Assigned(FSource) then
    FProgress(Sender, FSource.Position, FSource.Size);
end;

end.
