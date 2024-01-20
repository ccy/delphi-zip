unit System.Zip.Patch.LZMA;

interface

implementation

uses System.SysUtils, DDetours, System.Zip, System.Classes, System.Generics.Collections, System.RTLConsts, System.Zlib;

type
  {$HINTS OFF}
  TZipFileAccess = class
  private
    FMode: TZipMode;
    FStream: TStream;
    FFileStream: TFileStream;
    FStartFileData: Int64;
    FEndFileData: Int64;
    FFiles: TArray<TZipHeader>;
    FComment: TBytes;
    FEncoding: TEncoding;
    FUTF8Support: Boolean;
    FOnProgress: TZipProgressEvent;
    FCurrentFile: string;
    FCurrentHeader: TZipHeader;
    FPassword: string;
    FCryptor: IZipCryptor;
  end;
  {$HINTS ON}

  TZipFileHelper = class helper for TZipFile
    procedure Add_Patch(Data: TStream; LocalHeader: TZipHeader; CentralHeader: PZipHeader);
    class function Add_Address: Pointer;
    procedure DoZLibProgress_Private(Sender: TObject);
  end;

  TZipCryptStream = class(TStream)
  private
    FCryptor: IZipCryptor;
    FStream: TStream;
    FStart: Int64;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(AStream: TStream; ACRC32: Cardinal;
      const ACryptor: IZipCryptor; const APassword: string);
    function Write(const Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

procedure VerifyWrite(Stream: TStream; Buffer: TBytes; Count: Integer); overload;
begin
  try
    Stream.WriteBuffer(Buffer, 0, Count);
  except
    on E: EZipException do
      raise
    else
      Exception.RaiseOuterException(EZipException.CreateRes(@SZipErrorWrite));
  end;
end;

procedure VerifyWrite(Stream: TStream; const Buffer; Count: Integer); overload;
begin
  try
    Stream.WriteBuffer(Buffer, Count);
  except
    on E: EZipException do
      raise
    else
      Exception.RaiseOuterException(EZipException.CreateRes(@SZipErrorWrite));
  end;
end;

constructor TZipCryptStream.Create(AStream: TStream; ACRC32: Cardinal;
  const ACryptor: IZipCryptor; const APassword: string);
var
  Header: array[0..11] of Byte;
  I: Integer;
begin
  inherited Create;
  FStream := AStream;
  FStart := FStream.Position;
  FCryptor := ACryptor;
  FCryptor.Init(APassword, True);
  for I := 0 to 10 do
    Header[I] := Random(256);
  Header[11] := ACRC32 shr 24;
  VerifyWrite(Self, Header, Sizeof(Header));
end;

function TZipCryptStream.GetSize: Int64;
begin
  Result := FStream.Position - FStart;
end;

function TZipCryptStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (Origin = soCurrent) and (Offset = 0) then
    Result := GetSize // required to Get Current Position
  else
    raise EStreamError.CreateResFmt(@sSeekNotImplemented, [Classname]);
end;

function TZipCryptStream.Write(const Buffer; Count: Longint): Longint;
var
  LBuffer: TBytes;
begin
  if Count <= 0 then
    Exit(0);
  SetLength(LBuffer, Count);
  Move(Buffer, LBuffer[0], Count);
  FCryptor.Encrypt(LBuffer);
  VerifyWrite(FStream, LBuffer, Length(LBuffer));
  Result := Count;
end;

class function TZipFileHelper.Add_Address: Pointer;
var p: procedure(Data: TStream; LocalHeader: TZipHeader; CentralHeader: PZipHeader) of object;
begin
  p := TZipFile(nil).Add;
  Result := @p;
end;

procedure TZipFileHelper.Add_Patch(Data: TStream; LocalHeader: TZipHeader; CentralHeader: PZipHeader);
var
  LFileStart: Int64;
  LDataStart: Int64;
  LDataEnd: Int64;
  LCompressStream: TStream;
  LSignature: UInt32;
  LStartPos: Int64;
  LBuffer: TBytes;
  LReaded: Integer;
  LCompression: TZipCompression;
  L: Integer;
  LStream: TStream;
begin
  var Z := TZipFileAccess(Self);
  // Seek to End of zipped data
  LFileStart := Z.FEndFileData + Z.FStartFileData;
  if Data <> nil then
  begin
    LDataStart := Data.Position;
    LDataEnd := Data.Size;
  end
  else
  begin
    LDataStart := 0;
    LDataEnd := 0;
  end;
  LocalHeader.UncompressedSize64 := LDataEnd - LDataStart;
  LocalHeader.LocalHeaderOffset64 := LFileStart;
  // Require at least version 2.0
  if Lo(LocalHeader.MadeByVersion) < 20 then
    LocalHeader.MadeByVersion := Word(LocalHeader.MadeByVersion and $FF00) + 20;
  if LocalHeader.RequiredVersion < 20 then
    LocalHeader.RequiredVersion := 20;

  // Trust the length of the strings over the Length members
  LocalHeader.FileNameLength   := Length(LocalHeader.FileName);
  LocalHeader.ExtraFieldLength := Length(LocalHeader.ExtraField);
  if CentralHeader = nil then
    CentralHeader := @LocalHeader
  else
  begin // Trust the length of the strings over the Length members
    CentralHeader^.IsEncrypted      := Z.FPassword <> '';
    CentralHeader^.FileNameLength   := Length(CentralHeader^.FileName);
    CentralHeader^.ExtraFieldLength := Length(CentralHeader^.ExtraField);
  end;
  CentralHeader^.FileCommentLength  := Length(CentralHeader^.FileComment);

  // Write Signature, Header, and FileName
  Z.FStream.Position := LFileStart;
  LSignature := SIGNATURE_LOCALHEADER;
  VerifyWrite(Z.FStream, LSignature, SizeOf(LSignature));
  VerifyWrite(Z.FStream, LocalHeader.RequiredVersion, LOCALHEADERSIZE);
  VerifyWrite(Z.FStream, LocalHeader.FileName, LocalHeader.FileNameLength);
  if LocalHeader.ExtraFieldLength > 0 then
    VerifyWrite(Z.FStream, LocalHeader.ExtraField, LocalHeader.ExtraFieldLength);
  // Save position to calculate Compressed Size
  LStartPos := Z.FStream.Position;
  if Data <> nil then
  begin
    // Crypt
    if Z.FPassword = '' then
      LStream := Z.FStream
    else
    begin
      if not Assigned(Cryptor) then
        raise EZipException.CreateRes(@SZipCryptorNotAssigned);
      LocalHeader.IsEncrypted := True;
      // we need to know the data CRC for encryption
      SetLength(LBuffer, $4000);
      while Data.Position < LDataEnd do
      begin
        LReaded := Data.Read(LBuffer, Length(LBuffer));
        LocalHeader.CRC32 := crc32(LocalHeader.CRC32, @LBuffer[0], LReaded);
      end;
      Data.Position := LDataStart;
      LStream := TZipCryptStream.Create(Z.FStream, LocalHeader.CRC32, Cryptor, Z.FPassword);
    end;
    // Write Compressed data
    Z.FCurrentHeader := LocalHeader;
    LCompression := TZipCompression(LocalHeader.CompressionMethod);
    CheckCompressionSupported(LCompression);
    LCompressStream := TZipFile.FCompressionHandler[LCompression].Key(LStream, Self, LocalHeader);
    if LCompressStream is TZCompressionStream then
      TZCompressionStream(LCompressStream).OnProgress := DoZLibProgress_Private;
    try
      if Z.FPassword <> '' then
        // Don't need to recompute CRC
        LCompressStream.CopyFrom(Data, LDataEnd - LDataStart)
      (* patch *)
      else if TZipCompression(LocalHeader.CompressionMethod) = zcLZMA then
        LCompressStream.Write(Data, 0)
      else
      (* patch *)
      begin
        SetLength(LBuffer, $4000);
        // Calculate Uncompressed data's CRC while copying Data
        while Data.Position < LDataEnd do
        begin
          LReaded := Data.Read(LBuffer, Length(LBuffer));
          LCompressStream.Write(LBuffer, LReaded);
          LocalHeader.CRC32 := crc32(LocalHeader.CRC32, @LBuffer[0], LReaded);
        end;
      end;
      if Assigned(Z.FOnProgress) then
        Z.FOnProgress(Self, Z.FCurrentFile, Z.FCurrentHeader, LCompressStream.Position);
    finally
      LCompressStream.Free;
      if LStream <> Z.FStream then
        LStream.Free;
      Z.FCurrentHeader := Default(TZipHeader);
    end;
  end;
  // Calculate CompressedSize
  LocalHeader.CompressedSize64 := Z.FStream.Position - LStartPos;

  CentralHeader.UnCompressedSize := LocalHeader.UnCompressedSize;
  CentralHeader.CompressedSize := LocalHeader.CompressedSize;
  CentralHeader.CRC32 := LocalHeader.CRC32;
  CentralHeader.ExtraFieldLength := LocalHeader.ExtraFieldLength;
  CentralHeader.ExtraField := LocalHeader.ExtraField;
  // Save new End of zipped data mark
  Z.FEndFileData := Z.FStream.Position;
  // Move to beginning of Local Header offset and rewrite header
  // with correct CompressedSize and CRC32
  Z.FStream.Position := LocalHeader.LocalHeaderOffset64 + SizeOf(UInt32);
  VerifyWrite(Z.FStream, LocalHeader.RequiredVersion, LOCALHEADERSIZE);
  if LocalHeader.ExtraFieldLength > 0 then
  begin
    VerifyWrite(Z.FStream, LocalHeader.FileName, LocalHeader.FileNameLength);
    VerifyWrite(Z.FStream, LocalHeader.ExtraField, LocalHeader.ExtraFieldLength);
  end;
  L := Length(Z.FFiles);
  SetLength(Z.FFiles, L + 1);
  Z.FFiles[L] := CentralHeader^;
end;

procedure TZipFileHelper.DoZLibProgress_Private(Sender: TObject);
asm
  jmp TZipFile.DoZLibProgress;
end;

var TZipFile_Add: procedure(Self: TZipFile; Data: TStream; LocalHeader: TZipHeader; CentralHeader: PZipHeader);

initialization
  TZipFile_Add := InterceptCreate(TZipFile.Add_Address, @TZipFile.Add_Patch);
finalization
  InterceptRemove(@TZipFile_Add);
end.
