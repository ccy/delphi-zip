{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2013 Embarcadero Technologies, Inc. }
{                                                       }
{   Copyright and license exceptions noted in source    }
{                                                       }
{*******************************************************}

{*******************************************************}
{ Utility for creating and extracting Zip Files         }
{                                                       }
{See .ZIP File Format Specification at                  }
{http://www.pkware.com/documents/casestudies/APPNOTE.TXT}
{for more information on the .ZIP File Format.          }
{                                                       }
{ Support for Compression modes 0(store) and 8(deflate) }
{ Are implemented in this unit.                         }
{*******************************************************}

                        

unit System.Zip;

interface

uses
  System.SysUtils,
  System.IOUtils,
  System.Generics.Collections,
  System.Classes;

type
  /// <summary> Zip Compression Method Enumeration </summary>
  TZipCompression = (
    zcStored    = 0,
    zcShrunk,
    zcReduce1,
    zcReduce2,
    zcReduce3,
    zcReduce4,
    zcImplode,
    zcTokenize,
    zcDeflate,
    zcDeflate64,
    zcPKImplode,
    {11 RESERVED}
    zcBZIP2    = 12,
    {13 RESERVED}
    zcLZMA     = 14,
    {15-17 RESERVED}
    zcTERSE    = 18,
    zcLZ77,
    zcWavePack = 97,
    zcPPMdI1
  );

/// <summary> Converts ZIP compression method value to string </summary>
function TZipCompressionToString(Compression: TZipCompression): string;

const
  SIGNATURE_ZIPENDOFHEADER: UInt32 = $06054B50;
  SIGNATURE_CENTRALHEADER:  UInt32 = $02014B50;
  SIGNATURE_LOCALHEADER:    UInt32 = $04034B50;

  LOCALHEADERSIZE = 26;
  CENTRALHEADERSIZE = 42;

type
  /// <summary> Final block written to zip file</summary>
  TZipEndOfCentralHeader = packed record
    DiskNumber:          UInt16;
    CentralDirStartDisk: UInt16;
    NumEntriesThisDisk:  UInt16;
    CentralDirEntries:   UInt16;
    CentralDirSize:      UInt32;
    CentralDirOffset:    UInt32;
    CommentLength:       UInt16;
    {Comment: RawByteString}
  end;
  /// <summary> TZipHeader contains information about a file in a zip archive.
  /// </summary>
  /// <remarks>
  /// <para>
  /// This record is overloaded for use in reading/writing ZIP
  /// [Local file header] and the Central Directory's [file header].
  /// </para>
  /// <para> See PKZIP Application Note section V. General Format of a .ZIP file
  ///  sub section J. Explanation of fields for more detailed description
  //   of each field's usage.
  /// </para>
  /// </remarks>
  TZipHeader = packed record
    MadeByVersion:      UInt16; // Start of Central Header
    RequiredVersion:    UInt16; // Start of Local Header
    Flag:               UInt16;
    CompressionMethod:  UInt16;
    ModifiedDateTime:   UInt32;
    CRC32:              UInt32;
    CompressedSize:     UInt32;
    UncompressedSize:   UInt32;
    FileNameLength:     UInt16;
    ExtraFieldLength:   UInt16; // End of Local Header
    FileCommentLength:  UInt16;
    DiskNumberStart:    UInt16;
    InternalAttributes: UInt16;
    ExternalAttributes: UInt32;
    LocalHeaderOffset:  UInt32; // End of Central Header
    FileName: TBytes;
    ExtraField: TBytes;
    FileComment: TBytes;
  end;

  PZipHeader = ^TZipHeader;

  /// <summary> Exception type for all Zip errors. </summary>
  EZipException = class( Exception );

  TZipMode = (zmClosed, zmRead, zmReadWrite, zmWrite);

  TZipFile = class;
  /// <summary> Function to Create a Compression/Decompression stream </summary>
  /// <remarks>
  ///  Call <c>RegisterCompressionHandler</c> to register a compression type that
  ///  can Compress/Decompress a stream. The output stream reads/write from/to InStream.
  /// </remarks>
  TStreamConstructor = reference to function(InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader): TStream;

  /// <summary> Class for creating and reading .ZIP files.
  /// </summary>
  TZipFile = class
  private type
    TCompressionDict = TDictionary< TZipCompression , TPair<TStreamConstructor, TStreamConstructor > >;
  private class var
    FCompressionHandler: TCompressionDict;
  private
    FMode: TZipMode;
    FStream: TStream;
    FFileStream: TFileStream;
    FStartFileData: Int64;
    FEndFileData: Int64;
    FFiles: TList<TZipHeader>;
    FComment: TBytes;
    FUTF8Support: Boolean;
    function TBytesToString(B: TBytes): string;
    function StringToTBytes(S: string): TBytes;
    function GetFileComment(Index: Integer): string;
    function GetFileCount: Integer;
    function GetFileInfo(Index: Integer): TZipHeader;
    function GetFileInfos: TArray<TZipHeader>;
    function GetFileName(Index: Integer): string;
    function GetFileNames: TArray<string>;
    function GetComment: string;
    procedure ReadCentralHeader;
    procedure SetFileComment(Index: Integer; Value: string);
    procedure SetComment(Value: string);
    procedure SetUTF8Support(const Value: Boolean);
    function LocateEndOfCentralHeader(var Header: TZipEndOfCentralHeader): Boolean;
  public
    class constructor Create;
    class destructor Destroy;

    /// <remarks>
    ///  Call <c>RegisterCompressionHandler</c> to register a compression type that
    ///  can Compress/Decompress a stream. The output stream reads/write from/to InStream.
    /// </remarks>
    class procedure RegisterCompressionHandler(Compression: TZipCompression;
      CompressStream, DecompressStream: TStreamConstructor);

    /// <param name="ZipFileName">Path to Zip File</param>
    /// <returns>Is the .ZIP file valid</returns>
    class function IsValid(const ZipFileName: string): Boolean; static;

    /// <summary> Extract a ZipFile</summary>
    /// <param name="ZipFileName">File name of the ZIP file</param>
    /// <param name="Path">Path to extract to disk</param>
    class procedure ExtractZipFile(const ZipFileName: string; const Path: string); static;

    /// <summary> Zip the contents of a directory </summary>
    /// <param name="ZipFileName">File name of the ZIP file</param>
    /// <param name="Path">Path of directory to zip</param>
    /// <param name="Compression">Compression mode.</param>
    class procedure ZipDirectoryContents(const ZipFileName: string; const Path: string;
      Compression: TZipCompression = zcDeflate); static;

    /// <summary> Create a TZipFile</summary>
    constructor Create;

    /// <remarks> Destroy will close an open zipfile before disposing of it</remarks>
    destructor Destroy; override;

    /// <summary> Opens a ZIP file for reading or writing.</summary>
    /// <param name="ZipFileName">Path to ZipFile</param>
    /// <param name="OpenMode"> File Mode to open file.
    ///   <c>zmWrite</c> Creates a new ZIP file for writing.
    ///   <c>zmReadWrite</c> Opens the file for reading and allows adding
    ///      additional new files.
    ///   <c>zmRead</c> Opens the file for reading.
    ///</param>
    procedure Open(const ZipFileName: string; OpenMode: TZipMode); overload;
    procedure Open(ZipFileStream: TStream; OpenMode: TZipMode); overload;

    /// <remarks>
    ///   Closing is required to write the ZipFile's
    ///   Central Directory to disk. Closing a file that is open for writing
    ///   writes additonal metadata that is required for reading the file.
    /// </remarks>
    procedure Close;

    /// <summary> Extract a single file </summary>
    /// <remarks>
    ///  <c>FileName</c> specifies a file in the ZIP file. All slashes
    ///  in ZIP file names should be '/'.
    ///   The overload that takes an Integer may be useful when a ZIP file
    ///   has duplicate filenames.
    /// </remarks>
    /// <param name="FileName">File name in the archive</param>
    /// <param name="Path">Path to extract to disk</param>
    /// <param name="CreateSubdirs">The output should create sub directories specified in the ZIP file</param>
    procedure Extract(const FileName: string; const Path: string = ''; CreateSubdirs: Boolean = True); overload;
    procedure Extract(Index: Integer; const Path: string = ''; CreateSubdirs: Boolean = True); overload;
    /// <summary> Extract All files </summary>
    /// <param name="Path">Path to extract to.</param>
    procedure ExtractAll(const Path: string = '');

    /// <summary> Read a file from arcive to an array of Bytes </summary>
    /// <remarks>
    ///   The overload that takes an Integer may be useful when a ZIP file
    ///   has duplicate filenames.
    /// </remarks>
    /// <param name="FileName">ZIP file FileName</param>
    /// <param name="Bytes">Output bytes</param>
    ///
    procedure Read(const FileName: string; out Bytes: TBytes); overload;
    procedure Read(Index: Integer; out Bytes: TBytes); overload;
    /// <summary> Get a stream to read a file from disk </summary>
    /// <remarks>
    ///   The Stream returned by this function is a decomression stream
    ///   wrapper around the interal Stream reading the zip file. You must
    ///   Free this stream before using other TZipFile methods that change the
    ///   contents of the ZipFile, such as Read or Add.
    ///   The overload that takes an Integer may be useful when a ZIP file
    ///   has duplicate filenames.
    /// </remarks>
    /// <param name="FileName">ZIP file FileName</param>
    /// <param name="Stream">Output Stream</param>
    /// <param name="LocalHeader">Local File header</param>
    procedure Read(const FileName: string; out Stream: TStream; out LocalHeader: TZipHeader); overload;
    procedure Read(Index: Integer; out Stream: TStream; out LocalHeader: TZipHeader); overload;

    /// <summary> Add a file to the ZIP file </summary>
    /// <param name="FileName">FileName to be added</param>
    /// <param name="ArchiveFileName">Path + Name of file in the arcive.
    ///   If Ommitted, <C>ExtractFileName(FileName)</C> will be used.</param>
    /// <param name="Compression">Compression mode.</param>
    procedure Add(const FileName: string; const ArchiveFileName: string = '';
      Compression: TZipCompression = zcDeflate); overload;
    /// <summary> Add a memory file to the ZIP file </summary>
    /// <param name="Data">Bytes to be added</param>
    /// <param name="ArchiveFileName">Path + Name of file in the arcive.</param>
    /// <param name="Compression">Compression mode.</param>
    ///
    procedure Add(Data: TBytes; const ArchiveFileName: string; Compression: TZipCompression = zcDeflate); overload;
    /// <summary> Add a memory file to the ZIP file </summary>
    /// <param name="Data">Stream of file to be added</param>
    /// <param name="ArchiveFileName">Path + Name of file in the arcive.</param>
    /// <param name="Compression">Compression mode.</param>
    procedure Add(Data: TStream; const ArchiveFileName: string; Compression: TZipCompression = zcDeflate); overload;
    /// <summary> Add a memory file to the ZIP file. Allows programmer to specify
    ///  the Local and Central Header data for more flexibility on what gets written.
    ///  Minimal vailidation is done on the Header parameters; speficying bad options
    ///  could result in a corrupted zip file. </summary>
    /// <param name="Data">Stream of file to be added</param>
    /// <param name="LocalHeader">The local header data</param>
    /// <param name="CentralHeader">A Pointer to an optional central header. If no
    /// central Header is provided, the Local Header information is used. </param>
    procedure Add(Data: TStream; LocalHeader: TZipHeader; CentralHeader: PZipHeader = nil); overload;
                                                         
                                                       

    /// <summary> Translate from FileName to index in ZIP Central Header
    /// </summary>
    /// <remarks>
    ///  A ZIP file may have dupicate entries with the same name. This
    ///  function will return the index of the first.
    /// </remarks>
    /// <param name="FileName">Path + Name of file in the arcive.</param>
    /// <returns>The index of the file in the archive, or -1 on failure.
    /// </returns>
    function IndexOf(const FileName: string): Integer;

    /// <returns> The mode the TZipFile is opened to</returns>
    property Mode: TZipMode read FMode;

    /// <returns>Total files in ZIP File</returns>
    property FileCount: Integer read GetFileCount;

    /// <returns>An array of FileNames in the ZIP file</returns>
    property FileNames: TArray<string> read GetFileNames;
    /// <returns>An array of the TZipHeader of the files in the ZIP file</returns>
    property FileInfos: TArray<TZipHeader> read GetFileInfos;

    /// <returns>FileName of a File in the ZipFile</returns>
    property FileName[Index: Integer]: string read GetFileName;
    /// <returns>TZipHeader of a File in the ZipFile</returns>
    property FileInfo[Index: Integer]: TZipHeader read GetFileInfo;
    /// <remarks>
    ///  File Comments can be changed for files opened in write mode at any point.
    ///  The comment is written when the Central Directory is written to disk.
    ///  Comments can be a maximum of 65535 bytes long. If a longer comment is supplied,
    ///  It is truncated before writing to the ZIP File.
    /// </remarks>
    property FileComment[Index: Integer]: string read GetFileComment write SetFileComment;
    /// <remarks>
    ///  Comments can be a maximum of 65535 bytes long. If a longer comment is supplied,
    ///  It is truncated before writing to the ZIP File.
    /// </remarks>
    property Comment: string read GetComment write SetComment;
    property UTF8Support: Boolean read FUTF8Support write SetUTF8Support default True;
  end;

implementation

uses
  System.RTLConsts,
  System.ZLib;

procedure VerifyRead(Stream: TStream; Buffer: TBytes; Count: Integer); overload;
begin
  if Stream.Read(Buffer, Count) <> Count then
  raise EZipException.CreateRes(@SZipErrorRead) at ReturnAddress;
end;

procedure VerifyRead(Stream: TStream; var Buffer: UInt8; Count: Integer); overload;
begin
  if Stream.Read(Buffer, Count) <> Count then
  raise EZipException.CreateRes(@SZipErrorRead) at ReturnAddress;
end;

procedure VerifyRead(Stream: TStream; var Buffer: UInt16; Count: Integer); overload;
begin
  if Stream.Read(Buffer, Count) <> Count then
  raise EZipException.CreateRes(@SZipErrorRead) at ReturnAddress;
end;

procedure VerifyRead(Stream: TStream; var Buffer: UInt32; Count: Integer); overload;
begin
  if Stream.Read(Buffer, Count) <> Count then
  raise EZipException.CreateRes(@SZipErrorRead) at ReturnAddress;
end;

procedure VerifyWrite(Stream: TStream; Buffer: TBytes; Count: Integer); overload;
begin
  if Stream.Write(Buffer, 0, Count) <> Count then
    raise EZipException.CreateRes(@SZipErrorWrite) at ReturnAddress;
end;

procedure VerifyWrite(Stream: TStream; Buffer: UInt8; Count: Integer); overload;
begin
  if Stream.Write(Buffer, Count) <> Count then
    raise EZipException.CreateRes(@SZipErrorWrite) at ReturnAddress;
end;

procedure VerifyWrite(Stream: TStream; Buffer: UInt16; Count: Integer); overload;
begin
  if Stream.Write(Buffer, Count) <> Count then
    raise EZipException.CreateRes(@SZipErrorWrite) at ReturnAddress;
end;

procedure VerifyWrite(Stream: TStream; Buffer: UInt32; Count: Integer); overload;
begin
  if Stream.Write(Buffer, Count) <> Count then
    raise EZipException.CreateRes(@SZipErrorWrite) at ReturnAddress;
end;

type
  /// <summary> Helper class for reading a segment of another stream.</summary>
  TStoredStream = class( TStream )
  private
    FStream: TStream;
    FPos: Int64;
  protected
    function GetSize: Int64; override;
  public
    constructor Create( Stream: TStream );

//    function Read(var Buffer; Count: Longint): Longint; override;
//    function Write(const Buffer; Count: Longint): Longint; override;
    function Read(Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Write(const Buffer: TBytes; Offset, Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

{ TStoredStream }

constructor TStoredStream.Create(Stream: TStream);
begin
  FStream := Stream;
  FPos := FStream.Position;
end;

function TStoredStream.GetSize: Int64;
begin
  Result := FStream.Size;
end;

function TStoredStream.Read(Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  Result := FStream.Read(Buffer, Offset, Count);
end;

function TStoredStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  Result := FStream.Seek(Offset, Origin)
end;

function TStoredStream.Write(const Buffer: TBytes; Offset, Count: Longint): Longint;
begin
  Result := FStream.Write(Buffer, Offset, Count);
end;

function TZipCompressionToString(Compression: TZipCompression): string;
begin
  case Compression of
    zcStored:    Result := 'Stored';                // do not localize
    zcShrunk:    Result := 'Shrunk';                // do not localize
    zcReduce1:   Result := 'Reduced1';              // do not localize
    zcReduce2:   Result := 'Reduced2';              // do not localize
    zcReduce3:   Result := 'Reduced3';              // do not localize
    zcReduce4:   Result := 'Reduced4';              // do not localize
    zcImplode:   Result := 'Imploded';              // do not localize
    zcTokenize:  Result := 'Tokenized';             // do not localize
    zcDeflate:   Result := 'Deflated';              // do not localize
    zcDeflate64: Result := 'Deflated64';            // do not localize
    zcPKImplode: Result := 'Imploded(TERSE)';       // do not localize
    zcBZIP2:     Result := 'BZIP2';                 // do not localize
    zcLZMA:      Result := 'LZMA';                  // do not localize
    zcTERSE:     Result := 'TERSE';                 // do not localize
    zcLZ77:      Result := 'LZ77';                  // do not localize
    zcWavePack:  Result := 'WavPack';               // do not localize
    zcPPMdI1:    Result := 'PPMd version I, Rev 1'; // do not localize
    else
      Result := 'Unknown';
  end;
end;

{ TZipFile }

function TZipFile.TBytesToString(B: TBytes) : string;
var
  E: TEncoding;
begin
  if FUTF8Support then 
    E := TEncoding.GetEncoding(65001)
  else
    E := TEncoding.GetEncoding(437);
  try
    Result := E.GetString(B);
  finally
    E.Free;
  end;
end;

function TZipFile.StringToTBytes(S: string): TBytes;
var
  E: TEncoding;
begin
  if FUTF8Support then 
    E := TEncoding.GetEncoding(65001)
  else
    E := TEncoding.GetEncoding(437);
  try
    Result := E.GetBytes(S);
  finally
    E.Free;
  end;
end;

function TZipFile.GetComment: string;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  Result := TBytesToString(FComment);
end;

function TZipFile.GetFileComment(Index: Integer): string;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  Result := TBytesToString(FFiles[Index].FileComment);
end;

function TZipFile.GetFileCount: Integer;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  Result := FFiles.Count;
end;

function TZipFile.GetFileInfo(Index: Integer): TZipHeader;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  Result := FFiles[Index];
end;

function TZipFile.GetFileInfos: TArray<TZipHeader>;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  Result := FFiles.ToArray;
end;

function TZipFile.GetFileName(Index: Integer): string;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  Result := TBytesToString(FFiles[Index].FileName);
end;

function TZipFile.GetFileNames: TArray<string>;
var
  I: Integer;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  SetLength(Result, FFiles.Count);
  for I := 0 to High(Result) do
    Result[I] := TBytesToString(FFiles[I].FileName);
end;

procedure TZipFile.ReadCentralHeader;
var
  I: Integer;
  Signature: UInt32;
  LEndHeader: TZipEndOfCentralHeader;
  LHeader: TZipHeader;
begin
  FFiles.Clear;
  if FStream.Size = 0 then
    Exit;
  // Read End Of Centeral Direcotry Header
  if not LocateEndOfCentralHeader(LEndHeader) then
    raise EZipException.CreateRes(@SZipErrorRead);
  // Move to the beginning of the CentralDirectory
  FStream.Position := LEndHeader.CentralDirOffset;
  // Save Begginning of Central Directory. This is where new files
  // get written to, and where the new central directory gets written when
  // closing.
  FEndFileData := LEndHeader.CentralDirOffset;
  // Read File Headers
  for I := 0 to LEndHeader.CentralDirEntries - 1 do
  begin
    // Verify Central Header signature
    FStream.Read(Signature, Sizeof(Signature));
    if Signature <> SIGNATURE_CENTRALHEADER then
      raise EZipException.CreateRes(@SZipInvalidCentralHeader);
    // Read Central Header
    VerifyRead(FStream, LHeader.MadeByVersion,      Sizeof(UInt16));
    VerifyRead(FStream, LHeader.RequiredVersion,    Sizeof(UInt16));
    VerifyRead(FStream, LHeader.Flag,               Sizeof(UInt16));
    VerifyRead(FStream, LHeader.CompressionMethod,  Sizeof(UInt16));
    VerifyRead(FStream, LHeader.ModifiedDateTime,   Sizeof(UInt32));
    VerifyRead(FStream, LHeader.CRC32,              Sizeof(UInt32));
    VerifyRead(FStream, LHeader.CompressedSize,     Sizeof(UInt32));
    VerifyRead(FStream, LHeader.UncompressedSize,   Sizeof(UInt32));
    VerifyRead(FStream, LHeader.FileNameLength,     Sizeof(UInt16));
    VerifyRead(FStream, LHeader.ExtraFieldLength,   Sizeof(UInt16));
    VerifyRead(FStream, LHeader.FileCommentLength,  Sizeof(UInt16));
    VerifyRead(FStream, LHeader.DiskNumberStart,    Sizeof(UInt16));
    VerifyRead(FStream, LHeader.InternalAttributes, Sizeof(UInt16));
    VerifyRead(FStream, LHeader.ExternalAttributes, Sizeof(UInt32));
    VerifyRead(FStream, LHeader.LocalHeaderOffset,  Sizeof(UInt32));

    // Read Dynamic length fields (FileName, ExtraField, FileComment)
    if LHeader.FileNameLength > 0 then
    begin
      SetLength(LHeader.FileName, LHeader.FileNameLength);
      VerifyRead(FStream, LHeader.FileName, LHeader.FileNameLength);
    end;
    if LHeader.ExtraFieldLength > 0 then
    begin
      SetLength(LHeader.ExtraField, LHeader.ExtraFieldLength);
      VerifyRead(FStream, LHeader.ExtraField, LHeader.ExtraFieldLength);
    end;
    if LHeader.FileCommentLength > 0 then
    begin
      SetLength(LHeader.FileComment, LHeader.FileCommentLength);
      VerifyRead(FStream, LHeader.FileComment, LHeader.FileCommentLength);
    end;
    if (LHeader.Flag and (1 shl 11)) = 0 then
      FUTF8Support := False;

    // Save File Header in interal list
    FFiles.Add(LHeader);
  end;
end;

procedure TZipFile.SetComment(Value: string);
begin
  FComment := StringToTBytes(Value);
  if not (FMode in [zmReadWrite, zmWrite]) then
    raise EZipException.CreateRes(@SZipNoWrite);
  if Length(FComment) > $FFFF then
    SetLength(FComment, $FFFF);
end;

procedure TZipFile.SetFileComment(Index: Integer; Value: string);
var
  LFile: TZipHeader;
begin
  if not (FMode in [zmReadWrite, zmWrite]) then
    raise EZipException.CreateRes(@SZipNoWrite);
  LFile := FFiles[Index];

  LFile.FileComment := StringToTBytes(Value);
  if Length(LFile.FileComment) > $FFFF then
    SetLength(LFile.FileComment, $FFFF);
  LFile.FileCommentLength := Length(LFile.FileComment);
  FFiles[Index] := LFile;
end;

procedure TZipFile.SetUTF8Support(const Value: Boolean);
begin
  if Value = FUTF8Support then Exit;
  if not (FMode in [zmReadWrite, zmWrite]) then
    raise EZipException.CreateRes(@SZipNoWrite);
  // Resetting this flag would require re-writing all the local headers with the
  // new strings and flag, and adjusting the offsets.
  if FFiles.Count <> 0 then
    raise EZipException.CreateRes(@SZipNotEmpty);

                                         
  FUTF8Support := Value;
end;

class constructor TZipFile.Create;
begin
  FCompressionHandler := TCompressionDict.Create;

  RegisterCompressionHandler(zcStored,
    function(InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader): TStream
    begin
      Result := TStoredStream.Create(InStream);
    end,
    function(InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader): TStream
    begin
      Result := TStoredStream.Create(InStream);
    end);

  RegisterCompressionHandler(zcDeflate,
    function(InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader): TStream
    begin
      Result := TZCompressionStream.Create(InStream, zcDefault, -15);
    end,
    function(InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader): TStream
    begin
      Result := TZDecompressionStream.Create(InStream, -15);
    end);
end;

class destructor TZipFile.Destroy;
begin
  FCompressionHandler.Free;
end;

class procedure TZipFile.RegisterCompressionHandler(
  Compression: TZipCompression; CompressStream, DecompressStream: TStreamConstructor);
begin
  FCompressionHandler.AddOrSetValue(Compression,
    TPair<TStreamConstructor, TStreamConstructor>.Create(CompressStream, DecompressStream));
end;

class function TZipFile.IsValid(const ZipFileName: string): Boolean;
var
  Z: TZipFile;
  Header: TZipEndOfCentralHeader;
begin
  Result := False;
  try
    Z := TZipFile.Create;
    try
      Z.FStream := TFileStream.Create(ZipFileName, fmOpenRead);
      try
        Result := Z.LocateEndOfCentralHeader(Header);
      finally
        Z.FStream.Free;
      end;
    finally
      Z.Free;
    end;
  except on E: EStreamError do
    // Swallow only Stream exceptions and return False
  end;
end;

function TZipFile.LocateEndOfCentralHeader(var Header: TZipEndOfCentralHeader): Boolean;
var
  I: Integer;
  LBackRead, LReadSize, LMaxBack: UInt32;
  LBackBuf: TBytes;
begin
  if FStream.Size < $FFFF then
    LMaxBack := FStream.Size
  else
    LMaxBack := $FFFF;
  LBackRead := 4;
  SetLength(LBackBuf, $404 - 1);
  while LBackRead < LMaxBack do
  begin
    if LBackRead + Cardinal(Length(LBackBuf) - 4) > LMaxBack then
      LBackRead := LMaxBack
    else
      Inc(LBackRead, Length(LBackBuf) -4);
    FStream.Position := FStream.Size - LBackRead;
    if Length(LBackBuf) < (FStream.Size - FStream.Position) then
      LReadSize := Length(LBackBuf)
    else
      LReadSize := FStream.Size - FStream.Position;
    VerifyRead(FStream, LBackBuf, LReadSize);

    for I := LReadSize - 4 downto 0 do
    begin
      if (LBackBuf[I]   = ((SIGNATURE_ZIPENDOFHEADER       ) and $FF)) and
         (LBackBuf[I+1] = ((SIGNATURE_ZIPENDOFHEADER shr  8) and $FF)) and
         (LBackBuf[I+2] = ((SIGNATURE_ZIPENDOFHEADER shr 16) and $FF)) and
         (LBackBuf[I+3] = ((SIGNATURE_ZIPENDOFHEADER shr 24) and $FF)) then
      begin
        Move(LBackBuf[I+4], Header, SizeOf(Header));
        if Header.CommentLength > 0 then
        begin
          FStream.Position := FStream.Size - LBackRead + I + 4 + SizeOf(Header);
          SetLength(FComment, Header.CommentLength);
          FStream.Read(FComment, Header.CommentLength);
        end
        else
          SetLength(FComment, 0);
        Exit(True);
      end;
    end;
  end;
  Result := False;
end;

class procedure TZipFile.ExtractZipFile(const ZipFileName: string; const Path: string);
var
  LZip: TZipFile;
begin
  LZip := TZipFile.Create;
  try
    LZip.Open(ZipFileName, zmRead);
    LZip.ExtractAll(Path);
    LZip.Close;
  finally
    LZip.Free;
  end;
end;

class procedure TZipFile.ZipDirectoryContents(const ZipFileName: string; const Path: string;
  Compression: TZipCompression);
var
  LZipFile: TZipFile;
  LFile: string;
  LZFile: string;
  LPath: string;
begin
  LZipFile := TZipFile.Create;
  try
    LZipFile.Open(ZipFileName, zmWrite);
    LPath := System.SysUtils.IncludeTrailingPathDelimiter(Path);
    for LFile in TDirectory.GetFiles(Path, '*', TSearchOption.soAllDirectories) do
    begin
      // Strip off root path
{$IFDEF MSWINDOWS}
      LZFile := StringReplace(Copy(LFile, Length(LPath) + 1, Length(LFile)), '\', '/', [rfReplaceAll]);
{$ELSE}
      LZFile := Copy(LFile, Length(LPath) + 1, Length(LFile));
{$ENDIF MSWINDOWS}
      LZipFile.Add(LFile, LZFile, Compression);
    end;
  finally
    LZipFile.Free;
  end;
end;

constructor TZipFile.Create;
begin
  inherited Create;
  FFiles := TList<TZipHeader>.Create;
  FMode := zmClosed;
  FUTF8Support := True;
end;

destructor TZipFile.Destroy;
begin
  Close; // In case a file is open for writing currently

  FFiles.Free;
  inherited;
end;

procedure TZipFile.Open(const ZipFileName: string; OpenMode: TZipMode);
var
  LMode: LongInt;
  LFileStream: TFileStream;
begin
  Close; // In case the user had a file open
  case OpenMode of
    zmRead:      LMode := fmOpenRead;
    zmReadWrite: LMode := fmOpenReadWrite;
    zmWrite:     LMode := fmCreate;
    else
      raise EZipException.CreateRes(@sArgumentInvalid);
  end;
  LFileStream := TFileStream.Create(ZipFileName, LMode);
  try
    Open(LFileStream, OpenMode);
    FFileStream := LFileStream;
  except
    FreeAndNil(LFileStream);
    raise;
  end;
end;

procedure TZipFile.Open(ZipFileStream: TStream; OpenMode: TZipMode);
begin
  Close; // In case the user had a file open
  if OpenMode = zmClosed then
    raise EZipException.CreateRes(@sArgumentInvalid);
  if (OpenMode = zmRead) and (ZipFileStream.Size = 0) then
    raise EZipException.CreateRes(@SReadError);

  FStream := ZipFileStream;
  FStartFileData := FStream.Position;
  if OpenMode in [zmRead, zmReadWrite] then
  try
    // Read the Central Header to verify it's a valid zipfile
    ReadCentralHeader;
  except
    // If it's an invalid zipfile, cleanup
    FStream := nil;
    raise;
  end;
  FMode := OpenMode;
end;

procedure TZipFile.Close;
var
  LHeader: TZipHeader;
  LEndOfHeader: TZipEndOfCentralHeader;
  I: Integer;
  Signature: UInt32;
begin
  try
    // Only need to write Central Directory and End Of Central Directory if writing
    if (FMode = zmReadWrite) or (FMode = zmWrite) then
    begin
      FStream.Position := FEndFileData;
      Signature := SIGNATURE_CENTRALHEADER;
      // Write File Signatures
      for I := 0 to FFiles.Count - 1 do
      begin
        LHeader := FFiles[I];
        VerifyWrite(FStream, Signature, SizeOf(Signature));
//        VerifyWrite(FStream, LHeader.MadeByVersion,  CENTRALHEADERSIZE);
        VerifyWrite(FStream, LHeader.MadeByVersion,      Sizeof(UInt16));
        VerifyWrite(FStream, LHeader.RequiredVersion,    Sizeof(UInt16));
        VerifyWrite(FStream, LHeader.Flag,               Sizeof(UInt16));
        VerifyWrite(FStream, LHeader.CompressionMethod,  Sizeof(UInt16));
        VerifyWrite(FStream, LHeader.ModifiedDateTime,   Sizeof(UInt32));
        VerifyWrite(FStream, LHeader.CRC32,              Sizeof(UInt32));
        VerifyWrite(FStream, LHeader.CompressedSize,     Sizeof(UInt32));
        VerifyWrite(FStream, LHeader.UncompressedSize,   Sizeof(UInt32));
        VerifyWrite(FStream, LHeader.FileNameLength,     Sizeof(UInt16));
        VerifyWrite(FStream, LHeader.ExtraFieldLength,   Sizeof(UInt16));
        VerifyWrite(FStream, LHeader.FileCommentLength,  Sizeof(UInt16));
        VerifyWrite(FStream, LHeader.DiskNumberStart,    Sizeof(UInt16));
        VerifyWrite(FStream, LHeader.InternalAttributes, Sizeof(UInt16));
        VerifyWrite(FStream, LHeader.ExternalAttributes, Sizeof(UInt32));
        VerifyWrite(FStream, LHeader.LocalHeaderOffset,  Sizeof(UInt32));

        if LHeader.FileNameLength <> 0 then
          VerifyWrite(FStream, LHeader.FileName, LHeader.FileNameLength);
        if LHeader.ExtraFieldLength <> 0 then
          VerifyWrite(FStream, LHeader.ExtraField, LHeader.ExtraFieldLength);
        if LHeader.FileCommentLength <> 0 then
          VerifyWrite(FStream, LHeader.FileComment, LHeader.FileCommentLength);
      end;
      // Only support writing single disk .ZIP files
      FillChar(LEndOfHeader, Sizeof(LEndOfHeader), 0);
      LEndOfHeader.CentralDirEntries := FFiles.Count;
      LEndOfHeader.NumEntriesThisDisk := FFiles.Count;
      LEndOfHeader.CentralDirSize := FStream.Position - FEndFileData;
      LEndOfHeader.CentralDirOffset := FEndFileData;
      // Truncate comment if it's too long
      if Length(FComment) > $FFFF then
        SetLength(FComment, $FFFF);
      LEndofHeader.CommentLength := Length(FComment);
      // Write End Of Centeral Directory
      Signature := SIGNATURE_ZIPENDOFHEADER;
      VerifyWrite(FStream, Signature, SizeOf(Signature));
//      VerifyWrite(FStream, LEndOfHeader, SizeOf(LEndOfHeader));
      VerifyWrite(FStream, LEndOfHeader.DiskNumber,          SizeOf(UInt16));
      VerifyWrite(FStream, LEndOfHeader.CentralDirStartDisk, SizeOf(UInt16));
      VerifyWrite(FStream, LEndOfHeader.NumEntriesThisDisk,  SizeOf(UInt16));
      VerifyWrite(FStream, LEndOfHeader.CentralDirEntries,   SizeOf(UInt16));
      VerifyWrite(FStream, LEndOfHeader.CentralDirSize,      SizeOf(UInt32));
      VerifyWrite(FStream, LEndOfHeader.CentralDirOffset,    SizeOf(UInt32));
      VerifyWrite(FStream, LEndOfHeader.CommentLength,       SizeOf(UInt16));

      if LEndOfHeader.CommentLength > 0 then
        VerifyWrite(FStream, FComment, LEndOfHeader.CommentLength);
    end;
  finally
    FMode := zmClosed;
    FFiles.Clear;
    FStream := nil;
    if Assigned(FFileStream) then
      FreeAndNil(FFileStream);
  end;
end;

procedure TZipFile.Extract(const FileName: string; const Path: string; CreateSubDirs: Boolean);
begin
  Extract(IndexOf(FileName), Path, CreateSubdirs);
end;

procedure TZipFile.Extract(Index: Integer; const Path: string; CreateSubdirs: Boolean);
var
  LInStream, LOutStream: TStream;
  LHeader: TZipHeader;
  LDir, LFileName: string;
begin
  // Get decompression stream for file
  Read(Index, LInStream, LHeader);
  try
    LFileName := TBytesToString(FFiles[Index].FileName);
{$IFDEF MSWINDOWS} // ZIP stores files with '/', so translate to a relative Windows path.
    LFileName := StringReplace(LFileName, '/', '\', [rfReplaceAll]);
{$ENDIF}
    // CreateSubDirs = False assumes the user passed in the path where they want the file to end up
    if CreateSubdirs then
      LFileName := TPath.Combine(Path, LFileName)
    else
      LFileName := TPath.Combine(Path, ExtractFileName(LFileName));
    // Force directory creation
    LDir := ExtractFileDir(LFileName);
    if CreateSubdirs and (LDir <> '') then
      TDirectory.CreateDirectory(ExtractFileDir(LFileName));
    // Open the File For output
    if LFileName.Chars[LFileName.Length-1] = PathDelim then
      Exit; // Central Directory Entry points at a directory, not a file.
    LOutStream := TFileStream.Create(LFileName, fmCreate);
    try // And Copy from the decompression stream.
      // See Bit 3 at http://www.pkware.com/documents/casestudies/APPNOTE.TXT
      if (LHeader.Flag and (1 SHL 3)) = 0 then
      begin
        // Empty files should not be read
        if FFiles[Index].UncompressedSize > 0 then
          LOutStream.CopyFrom(LInStream, FFiles[Index].UncompressedSize);
      end
      else
      begin
        LOutStream.CopyFrom(LInStream, FFiles[Index].UncompressedSize);
      end;
    finally
      LOutStream.Free;
    end;
  finally
    LInStream.Free;
  end;
end;

procedure TZipFile.ExtractAll(const Path: string);
var
  I: Integer;
begin
  if not (FMode in [zmReadWrite, zmRead]) then
    raise EZipException.CreateRes(@SZipNoRead);
  for I := 0 to FFiles.Count - 1 do
    Extract(I, Path);
end;

procedure TZipFile.Read(const FileName: string; out Bytes: TBytes);
begin
  Read(IndexOf(FileName), Bytes);
end;

procedure TZipFile.Read(Index: Integer; out Bytes: TBytes);
var
  LStream: TStream;
  LHeader: TZipHeader;
  ReadStart, ReadBytes: Int64;
begin
  Read(Index, LStream, LHeader);
  try
    if (LHeader.Flag and (1 SHL 3)) = 0 then
    begin
      SetLength(Bytes, FFiles[Index].UncompressedSize);
      if FFiles[Index].UncompressedSize > 0 then // Special case for empty files.
        VerifyRead(LStream, Bytes, LHeader.UncompressedSize);
    end
    else
    begin
      //CRC, Uncompressed, and Compressed Size follow the compressed data.
      SetLength(Bytes, 4096);
      ReadStart := 0;
      ReadBytes := 0; // Supress warning
      while True do
      begin
        ReadBytes := LStream.Read(Bytes[ReadStart], Length(Bytes)-ReadStart);
        if ReadBytes < (Length(Bytes) - ReadStart) then
          break;
        ReadStart := ReadStart + ReadBytes;
        SetLength(Bytes, Length(Bytes)*2);
      end;
      SetLength(Bytes, ReadStart + ReadBytes);
    end;
  finally
    LStream.Free;
  end;
end;
//{$ENDIF}

procedure TZipFile.Read(const FileName: string; out Stream: TStream; out LocalHeader: TZipHeader);
begin
  Read(IndexOf(FileName), Stream, LocalHeader);
end;

procedure TZipFile.Read(Index: Integer; out Stream: TStream; out LocalHeader: TZipHeader);
var
  Signature: UInt32;
begin
  if not (FMode in [zmReadWrite, zmRead]) then
    raise EZipException.CreateRes(@SZipNoRead);

  if (Index < 0) or (Index > FFiles.Count) then
    raise EZipException.CreateRes(@SFileNotFound);

  // Local Header doesn't have thse fields
  LocalHeader.MadeByVersion := 0;
  SetLength(LocalHeader.FileComment, 0);
  LocalHeader.FileCommentLength  := 0;
  LocalHeader.DiskNumberStart    := 0;
  LocalHeader.InternalAttributes := 0;
  LocalHeader.ExternalAttributes := 0;
  LocalHeader.LocalHeaderOffset  := 0;

  // Move to beginning of Local Header
  FStream.Position := FFiles[Index].LocalHeaderOffset + FStartFileData;
  // Verify local header signature
  FStream.Read(Signature, Sizeof(Signature));
  if Signature <> SIGNATURE_LOCALHEADER then
    raise EZipException.CreateRes(@SZipInvalidLocalHeader);
  // Read local header
//  FStream.Read(LocalHeader.RequiredVersion, LOCALHEADERSIZE);
    FStream.Read(LocalHeader.RequiredVersion,    Sizeof(UInt16));
    FStream.Read(LocalHeader.Flag,               Sizeof(UInt16));
    FStream.Read(LocalHeader.CompressionMethod,  Sizeof(UInt16));
    FStream.Read(LocalHeader.ModifiedDateTime,   Sizeof(UInt32));
    FStream.Read(LocalHeader.CRC32,              Sizeof(UInt32));
    FStream.Read(LocalHeader.CompressedSize,     Sizeof(UInt32));
    FStream.Read(LocalHeader.UncompressedSize,   Sizeof(UInt32));
    FStream.Read(LocalHeader.FileNameLength,     Sizeof(UInt16));
    FStream.Read(LocalHeader.ExtraFieldLength,   Sizeof(UInt16));
  // Read Name and extra fields
  SetLength(LocalHeader.FileName, LocalHeader.FileNameLength);
  FStream.Read(LocalHeader.FileName, LocalHeader.FileNameLength);
  if LocalHeader.ExtraFieldLength > 0 then
  begin
    SetLength(LocalHeader.ExtraField, LocalHeader.ExtraFieldLength);
    FStream.Read(LocalHeader.ExtraField, LocalHeader.ExtraFieldLength);
  end;
  // Create Decompression stream.
  Stream := FCompressionHandler[TZipCompression(FFiles[Index].CompressionMethod)].Value(FStream, Self, LocalHeader);
end;

procedure TZipFile.Add(Data: TStream; LocalHeader: TZipHeader; CentralHeader: PZipHeader);
var
  DataStart: Int64;
  LCompressStream: TStream;
  Signature: UInt32;
  LStartPos: Int64;
  LBuffer: TBytes;
begin
  // Seek to End of zipped data
  FStream.Position := FEndFileData;
  LocalHeader.LocalHeaderOffset := FEndFileData;
  // Require at least version 2.0
  if LocalHeader.MadeByVersion < 20 then
    LocalHeader.MadeByVersion := 20;
  if LocalHeader.RequiredVersion < 20 then
    LocalHeader.RequiredVersion := 20;

  // Trust the length of the strings over the Length members
  LocalHeader.FileNameLength   := Length(LocalHeader.FileName);
  LocalHeader.ExtraFieldLength := Length(LocalHeader.ExtraField);
  if CentralHeader = nil then
    CentralHeader := @LocalHeader
  else
  begin // Trust the length of the strings over the Length members
    CentralHeader^.FileNameLength   := Length(CentralHeader^.FileName);
    CentralHeader^.ExtraFieldLength := Length(CentralHeader^.ExtraField);
  end;
  CentralHeader^.FileCommentLength  := Length(CentralHeader^.FileComment);

  // Write Signature, Header, and FileName
  Signature := SIGNATURE_LOCALHEADER;
  VerifyWrite(FStream, Signature, SizeOf(Signature));
//  VerifyWrite(FStream, LocalHeader.RequiredVersion, LOCALHEADERSIZE);
    VerifyWrite(FStream, LocalHeader.RequiredVersion,    Sizeof(UInt16));
    VerifyWrite(FStream, LocalHeader.Flag,               Sizeof(UInt16));
    VerifyWrite(FStream, LocalHeader.CompressionMethod,  Sizeof(UInt16));
    VerifyWrite(FStream, LocalHeader.ModifiedDateTime,   Sizeof(UInt32));
    VerifyWrite(FStream, LocalHeader.CRC32,              Sizeof(UInt32));
    VerifyWrite(FStream, LocalHeader.CompressedSize,     Sizeof(UInt32));
    VerifyWrite(FStream, LocalHeader.UncompressedSize,   Sizeof(UInt32));
    VerifyWrite(FStream, LocalHeader.FileNameLength,     Sizeof(UInt16));
    VerifyWrite(FStream, LocalHeader.ExtraFieldLength,   Sizeof(UInt16));

  VerifyWrite(FStream, LocalHeader.FileName, LocalHeader.FileNameLength);
  if LocalHeader.ExtraFieldLength > 0 then
    VerifyWrite(FStream, LocalHeader.ExtraField, LocalHeader.ExtraFieldLength);
  // Save position to calcuate Compressed Size
  LStartPos := FStream.Position;
  DataStart := Data.Position;
  LocalHeader.UncompressedSize := Data.Size - DataStart;
  // Write Compressed data
  LCompressStream := FCompressionHandler[TZipCompression(LocalHeader.CompressionMethod)].Key(FStream, self, LocalHeader);
  try
    LCompressStream.CopyFrom(Data, LocalHeader.UncompressedSize);
  finally
    LCompressStream.Free;
  end;

  // Calcuate CompressedSize
  LocalHeader.CompressedSize := FStream.Position - LStartPos;
  Data.Position := DataStart;
  SetLength(LBuffer, $4000);
  // Calcuate Uncompressed data's CRC
  while Data.Position < LocalHeader.UncompressedSize do
    LocalHeader.CRC32 := crc32(LocalHeader.CRC32, @LBuffer[0],
      Data.Read(LBuffer, Length(LBuffer)));
  CentralHeader.UnCompressedSize := LocalHeader.UnCompressedSize;
  CentralHeader.CompressedSize := LocalHeader.CompressedSize;
  CentralHeader.CRC32 := LocalHeader.CRC32;
  // Save new End of zipped data mark
  FEndFileData := FStream.Position;
  // Move to beginning of Local Header offset and rewrite header
  // with correct CompressedSize and CRC32
  FStream.Position := LocalHeader.LocalHeaderOffset + SizeOf(UInt32);
//  FStream.Write(LocalHeader.RequiredVersion, LOCALHEADERSIZE);
  FStream.Write(LocalHeader.RequiredVersion,    Sizeof(UInt16));
  FStream.Write(LocalHeader.Flag,               Sizeof(UInt16));
  FStream.Write(LocalHeader.CompressionMethod,  Sizeof(UInt16));
  FStream.Write(LocalHeader.ModifiedDateTime,   Sizeof(UInt32));
  FStream.Write(LocalHeader.CRC32,              Sizeof(UInt32));
  FStream.Write(LocalHeader.CompressedSize,     Sizeof(UInt32));
  FStream.Write(LocalHeader.UncompressedSize,   Sizeof(UInt32));
  FStream.Write(LocalHeader.FileNameLength,     Sizeof(UInt16));
  FStream.Write(LocalHeader.ExtraFieldLength,   Sizeof(UInt16));

  FFiles.Add(CentralHeader^);
end;

procedure TZipFile.Add(const FileName: string; const ArchiveFileName: string;
  Compression: TZipCompression);
var
  LInStream: TStream;
  LHeader: TZipHeader;
  LArchiveFileName: string;
begin
  if not (FMode in [zmReadWrite, zmWrite]) then
    raise EZipException.CreateRes(@SZipNoWrite);

  if not FCompressionHandler.ContainsKey(Compression) then
    raise EZipException.CreateResFmt(@SZipNotSupported, [
      TZipCompressionToString(Compression) ]);

  // Setup Header
  FillChar(LHeader, sizeof(LHeader), 0);
  LHeader.Flag := 0;
  LInStream := TFileStream.Create(FileName, fmOpenRead);
  try
    LHeader.Flag := 0;
    LHeader.CompressionMethod := UInt16(Compression);
    LHeader.ModifiedDateTime := DateTimeToFileDate( tfile.GetLastWriteTime(FileName) );
    LHeader.UncompressedSize := LInStream.Size;
    LHeader.InternalAttributes := 0;
    LHeader.ExternalAttributes := 0;                                               
    if ArchiveFileName <> '' then
	  LArchiveFileName := ArchiveFileName
	else
      LArchiveFileName := ExtractFileName(FileName);
    if FUTF8Support then
      LHeader.Flag := LHeader.Flag or (1 SHL 11); // Language encoding flag, UTF8
    LHeader.FileName := StringToTBytes(LArchiveFileName);
    LHeader.FileNameLength := Length(LHeader.FileName);

    LHeader.ExtraFieldLength := 0;
    Add(LInStream, LHeader);
  finally
    LInStream.Free;
  end;
end;

procedure TZipFile.Add(Data: TBytes; const ArchiveFileName: string;
  Compression: TZipCompression);
var
  LInStream: TStream;
begin
  if not (FMode in [zmReadWrite, zmWrite]) then
    raise EZipException.CreateRes(@SZipNoWrite);

  if not FCompressionHandler.ContainsKey(Compression) then
    raise EZipException.CreateResFmt(@SZipNotSupported, [
      TZipCompressionToString(Compression) ]);

  LInStream := TBytesStream.Create(Data);
  try
    Add(LInStream, ArchiveFileName, Compression);
  finally
    LInStream.Free;
  end;
end;

procedure TZipFile.Add(Data: TStream; const ArchiveFileName: string;
  Compression: TZipCompression);
var
  LHeader: TZipHeader;
begin
  if not (FMode in [zmReadWrite, zmWrite]) then
    raise EZipException.CreateRes(@SZipNoWrite);

  if not FCompressionHandler.ContainsKey(Compression) then
    raise EZipException.CreateResFmt(@SZipNotSupported, [
      TZipCompressionToString(Compression) ]);

  // Setup Header
  FillChar(LHeader, sizeof(LHeader), 0);
  LHeader.Flag := 0;
  LHeader.CompressionMethod := UInt16(Compression);
  LHeader.ModifiedDateTime := DateTimeToFileDate( Now );
  LHeader.InternalAttributes := 0;
  LHeader.ExternalAttributes := 0;                                               
  if FUTF8Support then
    LHeader.Flag := LHeader.Flag or (1 SHL 11); // Language encoding flag, UTF8
  LHeader.FileName := StringToTBytes(ArchiveFileName);
  LHeader.FileNameLength := Length(LHeader.FileName);

  LHeader.ExtraFieldLength := 0;
  Add(Data, LHeader);
end;


function TZipFile.IndexOf(const FileName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FFiles.Count - 1 do
    if SameText(TBytesToString(FFiles[I].FileName), FileName) then
      Exit(I);
end;

end.

