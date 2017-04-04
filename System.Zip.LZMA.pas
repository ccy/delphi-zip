unit System.Zip.LZMA;

interface

uses
  System.SysUtils, System.Classes, System.Zip2, LzmaDec, LzmaEnc;

type
  TLZMAEncoderStream = class(TStream)
  private
    FStream: TStream;
    FEncoderHandle: TCLzmaEncHandle;
    FProgress: TZipProgressEvent;
    FZipHeader: TZipHeader;
  public
    constructor Create(const Stream: TStream; aZipHeader: TZipHeader; const
        aProgress: TZipProgressEvent); reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Write(const Buffer; Count: Longint): Longint; override;
  end;

  TLZMADecoderStream = class(TStream)
  private
    FCurrentDataLen: UInt32;
    FData: TBytes;
    FDataLen: UInt32;
    FLzmaState: TCLzmaDec;
    FStream: TStream;
    FProgress: TZipProgressEvent;
    FZipHeader: TZipHeader;
    FDecompressSize: Int64;
  public
    constructor Create(const Stream: TStream; aZipHeader: TZipHeader; const
        aProgress: TZipProgressEvent); reintroduce;
    procedure AfterConstruction; override;
    procedure BeforeDestruction; override;
    function Read(Buffer: TBytes; Offset, Count: Longint): Longint; override;
  end;

implementation

uses Winapi.Windows, LzmaTypes;

type
  PLzmaEncoderRead = ^TLzmaEncoderRead;
  TLzmaEncoderRead = record
    Proc: TInStreamReadProc;
    Stream: TStream;
  end;

  PzmaEncoderWrite = ^TLzmaEncoderWrite;
  TLzmaEncoderWrite = record
    Proc: TOutStreamWriteProc;
    Stream: TStream;
  end;

  PLzmaCompressProgress = ^TLzmaCompressProgress;
  TLzmaCompressProgress = record
    Proc: TCompressProgressProc;
    ZipHeader: TZipHeader;
    UncompressedSize: UInt64;
    Progress: TZipProgressEvent;
  end;

function LzmaReadProc(p: PISeqInStream; buf: PByte; var size: SIZE_T): TSRes; cdecl;
var R: PLzmaEncoderRead;
begin
  try
    R := PLzmaEncoderRead(p);
    size := R.Stream.Read(buf^, size);
    Result := SZ_OK;
  except
    Result := SZ_ERROR_DATA;
  end;
end;

function LzmaWriteProc(p: PISeqOutStream; const buf: Pointer; size: SIZE_T): SIZE_T;
    cdecl;
var R: PzmaEncoderWrite;
begin
  R := PzmaEncoderWrite(p);
  Result := R.Stream.Write(buf^, size);
end;

function LzmaProgressProc(p: PICompressProgress; inSize: UInt64; outSize: UInt64):
    TSRes; cdecl;
var R: PLzmaCompressProgress;
begin
  R := PLzmaCompressProgress(p);
  if Assigned(R.Progress) then
    R.Progress(nil, TEncoding.Default.GetString(R.ZipHeader.FileName), R.ZipHeader, inSize);
  Result := SZ_OK;
end;

procedure TLZMAEncoderStream.AfterConstruction;
var R: TISzAlloc;
    Props: TCLzmaEncProps;
    PropData: TBytes;
    Version, WPropDataLen: Word;
    PropDataLen: SIZE_T;
begin
  inherited;
  Version := 9 + 20 shl 8;
  FStream.Write(Version, SizeOf(Version));

  R.Init;
  FEncoderHandle := LzmaEnc_Create(R);

  LzmaEncProps_Init(Props);

  CheckLzma(LzmaEnc_SetProps(FEncoderHandle, Props));

  PropDataLen := LZMA_PROPS_SIZE;
  SetLength(PropData, PropDataLen);
  CheckLzma(LzmaEnc_WriteProperties(FEncoderHandle, PropData, PropDataLen));
  Assert(PropDataLen = LZMA_PROPS_SIZE);

  WPropDataLen := PropDataLen;

  FStream.Write(WPropDataLen, SizeOf(WPropDataLen));
  FStream.Write(PropData, PropDataLen);
end;

procedure TLZMAEncoderStream.BeforeDestruction;
var R: TISzAlloc;
begin
  inherited;
  R.Init;
  LzmaEnc_Destroy(FEncoderHandle, R, R);
end;

constructor TLZMAEncoderStream.Create(const Stream: TStream; aZipHeader:
    TZipHeader; const aProgress: TZipProgressEvent);
begin
  inherited Create;
  FStream := Stream;
  FZipHeader := aZipHeader;
  FProgress := aProgress;
end;

function TLZMAEncoderStream.Seek(const Offset: Int64;
  Origin: TSeekOrigin): Int64;
begin
  if (Offset = 0) and (Origin = soCurrent) then
    Result := FZipHeader.UncompressedSize;
end;

function TLZMAEncoderStream.Write(const Buffer; Count: Longint): Longint;
var A: TISzAlloc;
    R: TLzmaEncoderRead;
    W: TLzmaEncoderWrite;
    P: TLzmaCompressProgress;
begin
  A.Init;
  R.Proc := LzmaReadProc;
  R.Stream := TStream(Buffer);

  W.Proc := LzmaWriteProc;
  W.Stream := FStream;

  P.Proc := LzmaProgressProc;
  P.ZipHeader := FZipHeader;
  P.UncompressedSize := R.Stream.Size;
  P.Progress := FProgress;

  CheckLzma(LzmaEnc_Encode(FEncoderHandle, @W, @R, @P, A, A));
  Result := Count;
end;

constructor TLZMADecoderStream.Create(const Stream: TStream; aZipHeader:
    TZipHeader; const aProgress: TZipProgressEvent);
begin
  inherited Create;
  FStream := Stream;
  FZipHeader := aZipHeader;
  FProgress := aProgress;
  FDecompressSize := 0;
end;

procedure TLZMADecoderStream.AfterConstruction;
var PropData: TBytes;
    PropDataLen: Word;
    R: TISzAlloc;
begin
  inherited;
  FStream.Seek(2, soFromCurrent);  // Skip 2 bytes. Lzma library version

  FStream.Read(PropDataLen, 2);    // Properties size
  Assert(PropDataLen = LZMA_PROPS_SIZE);

  SetLength(PropData, PropDataLen);
  FStream.Read(PropData, PropDataLen);

  FLzmaState.Construct;
  R.Init;

  CheckLzma(LzmaDec_Allocate(FLzmaState, PropData[0], PropDataLen, R));

  LzmaDec_Init(FLzmaState);

  FCurrentDataLen := 0;

  FDataLen := $F000;
  SetLength(FData, FDataLen);
end;

procedure TLZMADecoderStream.BeforeDestruction;
var R: TISzAlloc;
begin
  R.Init;
  LzmaDec_Free(FLzmaState, R);
  FillChar(FLzmaState, SizeOf(FLzmaState), 0);
  inherited;
end;

function TLZMADecoderStream.Read(Buffer: TBytes; Offset, Count: Longint):
    Longint;
var Status: ELzmaStatus;
    OutLen, InLen: SIZE_T;
    BufferPos: LongInt;
begin
  BufferPos := 0;
  repeat
    if FCurrentDataLen = 0 then begin
      FCurrentDataLen := FStream.Read(FData[0], FDataLen);
      FDataLen := FCurrentDataLen;
    end;

    OutLen := Count - BufferPos;
    InLen := FCurrentDataLen;

    CheckLzma(LzmaDec_DecodeToBuf(FLzmaState, Buffer[BufferPos], OutLen, FData[FDataLen - FCurrentDataLen], InLen, LZMA_FINISH_ANY, Status));

    Dec(FCurrentDataLen, InLen);

    Inc(BufferPos, OutLen);
    Inc(FDecompressSize, OutLen);
  until Status <> LZMA_STATUS_NEEDS_MORE_INPUT;

  if Assigned(FProgress) then
    FProgress(Self, TEncoding.Default.GetString(FZipHeader.FileName), FZipHeader, FDecompressSize);

  Result := BufferPos;
end;

procedure RegisterLZMA;
begin
  TZipFile.RegisterCompressionHandler(zcLZMA,
    function(InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader): TStream
    begin
      Result := TLZMAEncoderStream.Create(InStream, Item, ZipFile.OnProgress);
    end,
    function(InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader): TStream
    begin
      Result := TLZMADecoderStream.Create(InStream, Item, ZipFile.OnProgress);
    end
  );
end;

procedure UnregisterLZMA;
begin
  TZipFile.UnregisterCompressionHandler(zcLZMA);
end;

initialization
  RegisterLZMA;
finalization
  UnregisterLZMA;
end.
