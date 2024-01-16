{*******************************************************}
{                                                       }
{           CodeGear Delphi Runtime Library             }
{                                                       }
{ Copyright(c) 1995-2023 Embarcadero Technologies, Inc. }
{              All rights reserved                      }
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

{$WEAKPACKAGEUNIT OFF}

interface

{$WARN SYMBOL_PLATFORM OFF}

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
  SIGNATURE_ZIP64ENDOFHEADER: UInt32 = $07064B50;
  SIGNATURE_ZIP64CENTRALHEADER: UInt32 = $06064B50;

  ZIP64_MAXINT = UInt32($FFFFFFFF);

  ZIP64_EXTRAHEADER = $0001;

  LOCALHEADERSIZE = 26;
  CENTRALHEADERSIZE = 42;

  DEFAULT_BUFFER_SIZE = $100000;

  MADEBY_MSDOS = 0;
  MADEBY_UNIX = 3;

type
  /// <summary>ZIP64 CentralHeader extension</summary>
  TZip64Header = packed record
    Signature:           UInt32; // zip64 end of central dir signature
    HeaderSize:          Int64;  // size of zip64 end of central directory record
    MadeByVersion:       UInt16; // version made by
    RequiredVersion:     UInt16; // version needed to extract
    NumberOfDisks:       UInt32; // number of this disk
    CentralDirStartDisk: UInt32; // number of the disk with the start of the central directory
    NumEntriesThisDisk:  UInt64; // total number of entries in the central directory on this disk
    CentralDirEntries:   UInt64; // total number of entries in the central directory
    CentralDirSize:      UInt64; // size of the central directory
    CentralDirOffset:    UInt64; // offset of start of central directory with respect to the starting disk number
  //zip64 extensible data sector
  end;

  // <summary>ZIP64 CentralHeader locator</summary>
  TZip64EndOfCentralHeader = packed record
    Signature:             UInt32;
    CentralDirStartDisk:   UInt32;
    Zip64CentralDirOffset: UInt64;
    TotalNumberOfDisks:    UInt32;
  end;

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

  /// <summary>stored in Extra Field with tag $0001</summary>
  TZip64ExtraHeader = packed record
    UncompressedSize:  UInt64;
    CompressedSize:    UInt64; // optional
    LocalHeaderOffset: UInt64; // optional
    DiskNumberStart:   UInt32; // optional
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
  private
    function Get64ExtraHeaderField(const Header: TZip64ExtraHeader; Size, Field: Integer): UInt64;
    function GetUncompressedSize64: UInt64;
    procedure SetUncompressedSize64(Value: UInt64);
    function GetCompressedSize64: UInt64;
    procedure SetCompressedSize64(Value: UInt64);
    function GetLocalHeaderOffset64: UInt64;
    procedure SetLocalHeaderOffset64(Value: UInt64);
    function GetModifiedTime: TDateTime;
    procedure SetModifiedTime(const Value: TDateTime);
    function GetFileAttributes: TFileAttributes;
    procedure SetFileAttributes(const Value: TFileAttributes);
    function GetFlag(Index: Integer): Boolean;
    procedure SetFlag(Index: Integer; Value: Boolean);
  public
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
    property UncompressedSize64: UInt64 read GetUncompressedSize64 write SetUncompressedSize64;
    property CompressedSize64: UInt64 read GetCompressedSize64 write SetCompressedSize64;
    property LocalHeaderOffset64: UInt64 read GetLocalHeaderOffset64 write SetLocalHeaderOffset64;
    property ModifiedTime: TDateTime read GetModifiedTime write SetModifiedTime;
    property FileAttributes: TFileAttributes read GetFileAttributes write SetFileAttributes;
    // From https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT
    // Section 4.4.4 general purpose bit flag: (2 bytes)
    // Bit 0: If set, indicates that the file is encrypted.
    property IsEncrypted: Boolean index 0 read GetFlag write SetFlag;
    // Bit 3: If this bit is set, the fields crc-32, compressed size and
    // uncompressed size are put in the data descriptor immediately following
    // the compressed data.
    property UseDataDescriptor: Boolean index 3 read GetFlag;
    // Bit 11: Language encoding flag (EFS)
    property UTF8Support: Boolean index 11 read GetFlag write SetFlag;
  end;

  PZipHeader = ^TZipHeader;

  /// <summary> Exception type for all Zip errors. </summary>
  EZipException = class(Exception);
  EZipCRCException = class(EZipException);
  EZipWrongPassword = class(EZipCRCException);

  EZipFileNotFoundException = class(EZipException)
  private
    FFileName: string;
  public
    constructor Create(const AFileName: string);
    property FileName: string read FFileName;
  end;

  TZipMode = (zmClosed, zmRead, zmReadWrite, zmWrite);

  /// <summary> On progress event</summary>
  TZipProgressEvent = procedure(Sender: TObject; FileName: string; Header: TZipHeader; Position: Int64) of object;

  TZipFile = class;
  /// <summary> Function to Create a Compression/Decompression stream </summary>
  /// <remarks>
  ///  Call <c>RegisterCompressionHandler</c> to register a compression type that
  ///  can Compress/Decompress a stream. The output stream reads/write from/to InStream.
  /// </remarks>
  TStreamConstructor = reference to function(InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader): TStream;

  /// <summary> Callback to create a custom stream  based on the original</summary>
  TCreateCustomStreamCallBack = reference to function(const InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader; IsEncrypted: Boolean): TStream;
  TOnCreateCustomStream = function(const InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader; IsEncrypted: Boolean): TStream of object;

  /// <summary> Interface implementing encrypted ZIP encryptor/decryptor</summary>
  IZipCryptor = interface(IInterface)
    ['{CB1D5970-6CCC-402D-BAAD-02ED425ED07B}']
    procedure Init(const APassword: string; AEncrypt: Boolean);
    procedure Decrypt(var Buffer: TBytes);
    procedure Encrypt(var Buffer: TBytes);
  end;

  /// <summary> Class for creating and reading .ZIP files.
  /// </summary>
  TZipFile = class
  private type
    TCompressionReg = TPair<TStreamConstructor, TStreamConstructor>;
    TCompressionDict = TDictionary<TZipCompression, TCompressionReg>;
  private class var
    FCompressionHandler: TCompressionDict;
    FOnCreateDecompressStream: TOnCreateCustomStream;
    FCreateDecompressStreamCallBack: TCreateCustomStreamCallBack;
    FDefaultEncoding: TEncoding;
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
    function GetEncoding: TEncoding; virtual;
    function GetFileCount: Integer;
    function GetFileInfo(Index: Integer): TZipHeader;
    function GetFileInfos: TArray<TZipHeader>;
    function GetFileName(Index: Integer): string;
    function GetFileNames: TArray<string>;
    procedure ReadCentralHeader;
    procedure SetUTF8Support(const Value: Boolean);
    function LocateEndOfCentralHeader(var Header: TZipEndOfCentralHeader): Boolean;
    procedure DoZLibProgress(Sender: TObject);
  protected
    function InternalGetFileName(Index: Integer): string; virtual;
    procedure CheckFileName(const ArchiveFileName: string); virtual;
    function GetComment: string; virtual;
    function GetFileComment(Index: Integer): string; virtual;
    function GetTextEncode(const Header: TZipHeader): TEncoding; virtual;
    procedure SetComment(Value: string); virtual;
    procedure SetFileComment(Index: Integer; Value: string); virtual;
    procedure CheckCompressionSupported(Compression: TZipCompression);
    procedure CheckDecompressionSupported(Compression: TZipCompression);
    procedure CheckIndex(Index: Integer);
  public
    class constructor Create;
    class destructor Destroy;

    /// <remarks>
    ///  Call <c>RegisterCompressionHandler</c> to register a compression type that
    ///  can Compress/Decompress a stream. The output stream reads/write from/to InStream.
    /// </remarks>
    class procedure RegisterCompressionHandler(Compression: TZipCompression;
      const CompressStream, DecompressStream: TStreamConstructor);
    /// <remarks>
    ///  Call <c>UnRegisterCompressionHandler</c> to unregister a compression type.
    /// </remarks>
    class procedure UnRegisterCompressionHandler(Compression: TZipCompression);

    /// <param name="ZipFileName">Path to Zip File</param>
    /// <returns>Is the .ZIP file valid</returns>
    class function IsValid(const ZipFileName: string): Boolean; overload; static;

    /// <param name="Stream">Stream of a Zip File</param>
    /// <returns>Is the .ZIP file valid</returns>
    class function IsValid(Stream: TStream): Boolean; overload; static;

    /// <summary> Extract a ZipFile</summary>
    /// <param name="ZipFileName">File name of the ZIP file</param>
    /// <param name="Path">Path to extract to disk</param>
    /// <param name="ZipProgress">On progress callback.</param>
    class procedure ExtractZipFile(const ZipFileName: string; const Path: string; ZipProgress: TZipProgressEvent = nil); overload; static;
    class procedure ExtractZipFile(const ZipFileName: string; const Path: string; const Encoding: TEncoding; ZipProgress: TZipProgressEvent = nil); overload; static;

    /// <summary> Zip the contents of a directory </summary>
    /// <param name="ZipFileName">File name of the ZIP file</param>
    /// <param name="Path">Path of directory to zip</param>
    /// <param name="Compression">Compression mode.</param>
    /// <param name="ZipProgress">On progress callback.</param>
    class procedure ZipDirectoryContents(const ZipFileName: string; const Path: string; Compression: TZipCompression = zcDeflate; ZipProgress: TZipProgressEvent = nil); overload; static;
    class procedure ZipDirectoryContents(const ZipFileName: string; const Path: string; const Encoding: TEncoding; Compression: TZipCompression = zcDeflate; ZipProgress: TZipProgressEvent = nil); overload; static;

    /// <summary> Checks if header extra field contains unicode path, if true AFilename contains the unicode path</summary>
    class function GetUTF8PathFromExtraField(const AHeader: TZipHeader; out AFileName: string): Boolean;

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

    /// <summary> Read a file from archive to an array of Bytes </summary>
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
    /// <param name="CheckCrc">When True then enables CRC checking while reading Stream</param>
    procedure Read(const FileName: string; out Stream: TStream; out LocalHeader: TZipHeader; CheckCrc: Boolean = False); overload;
    procedure Read(Index: Integer; out Stream: TStream; out LocalHeader: TZipHeader; CheckCrc: Boolean = False); overload;

    /// <summary> Add a file to the ZIP file </summary>
    /// <param name="FileName">FileName to be added</param>
    /// <param name="ArchiveFileName">Path + Name of file in the archive.
    ///   If Ommitted, <C>ExtractFileName(FileName)</C> will be used.</param>
    /// <param name="Compression">Compression mode.</param>
    procedure Add(const FileName: string; const ArchiveFileName: string = '';
      Compression: TZipCompression = zcDeflate); overload;
    /// <summary> Add a memory file to the ZIP file </summary>
    /// <param name="Data">Bytes to be added</param>
    /// <param name="ArchiveFileName">Path + Name of file in the archive.</param>
    /// <param name="Compression">Compression mode.</param>
    procedure Add(Data: TBytes; const ArchiveFileName: string; Compression: TZipCompression = zcDeflate); overload;
    /// <summary> Add a memory file to the ZIP file </summary>
    /// <param name="Data">Stream of file to be added</param>
    /// <param name="ArchiveFileName">Path + Name of file in the archive.</param>
    /// <param name="Compression">Compression mode.</param>
    /// <param name="AExternalAttributes">External attributes for this file.</param>
    procedure Add(Data: TStream; const ArchiveFileName: string; Compression: TZipCompression = zcDeflate;
      AExternalAttributes: TFileAttributes = []); overload;
    /// <summary> Add a memory file to the ZIP file. Allows programmer to specify
    ///  the Local and Central Header data for more flexibility on what gets written.
    ///  Minimal vailidation is done on the Header parameters; speficying bad options
    ///  could result in a corrupted zip file. </summary>
    /// <param name="Data">Stream of file to be added</param>
    /// <param name="LocalHeader">The local header data</param>
    /// <param name="CentralHeader">A Pointer to an optional central header. If no
    /// central Header is provided, the Local Header information is used. </param>
    procedure Add(Data: TStream; LocalHeader: TZipHeader; CentralHeader: PZipHeader = nil); overload;
    /// <summary> Add a directory to the ZIP file </summary>
    /// <param name="DirName">DirName to be added.
    ///   If Specified, it existance will be checked.</param>
    ///   If Ommitted, <C>ArchiveDirName</C> will be used.</param>
    /// <param name="ArchiveDirName">Path + Name of directory in the archive.
    ///   If Ommitted, <C>ExtractFileName(DirName)</C> will be used.</param>
    procedure AddDirectory(const DirName: string; const ArchiveDirName: string = '');

    /// <summary>Remove a file from the ZIP file</summary>
    procedure Delete(const FileName: string); overload;
    procedure Delete(Index: Integer); overload;

    /// <summary>Rename a file in the ZIP file</summary>
    procedure Rename(const FileName, NewFileName: string); overload;
    procedure Rename(Index: Integer; const NewFileName: string); overload;

    /// <summary>
    /// Event fired before a file inside a zip file is decompressed, allows access to the raw stream for decrypt purposes
    /// </summary>
    class property OnCreateDecompressStream: TOnCreateCustomStream read FOnCreateDecompressStream write FOnCreateDecompressStream;
    /// <summary>
    /// Callback called before a file inside a zip file is decompressed, allows access to the raw stream for decrypt purposes
    /// </summary>
    class property CreateDecompressStreamCallBack: TCreateCustomStreamCallBack read FCreateDecompressStreamCallBack write FCreateDecompressStreamCallBack;

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

    /// <summary> Unlike IndexOf, will raise EZipException if the file is not found</summary>
    function GetFileIndex(const FileName: string): Integer;

    /// <returns>FileName of a TZipHeader</returns>
    function GetHeaderFileName(const Header: TZipHeader): string;

    /// <returns> The mode the TZipFile is opened to</returns>
    property Mode: TZipMode read FMode;
    /// <returns> The stream the TZipFile is opened with</returns>
    property Stream: TStream read FStream;

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
    property Encoding: TEncoding read GetEncoding write FEncoding;
    /// <summary>Password to read or write a crypted Zip.
    ///  Note, empty string is considered as no password is set. </summary>
    property Password: string read FPassword write FPassword;
    /// <summary>Interface of crypted Zip encryptor/decryptor. </summary>
    property Cryptor: IZipCryptor read FCryptor write FCryptor;
    /// <summary> On progress event. </summary>
    property OnProgress: TZipProgressEvent read FOnProgress write FOnProgress;
  end;

implementation

uses
{$IFDEF MSWINDOWS}
  Winapi.Windows,
{$ENDIF}
  System.RTLConsts,
  System.ZLib,
  System.Types;

const
  MaxCommentLength = $FFFF;

function DateTimeToWinFileDate(DateTime: TDateTime): UInt32;
var
  Year, Month, Day, Hour, Min, Sec, MSec: Word;
begin
  DecodeDate(DateTime, Year, Month, Day);
  if (Year < 1980) or (Year > 2107)
    then Result := 0
  else
  begin
    DecodeTime(DateTime, Hour, Min, Sec, MSec);
    LongRec(Result).Lo := (Sec shr 1) or (Min shl 5) or (Hour shl 11);
    LongRec(Result).Hi := Day or (Month shl 5) or ((Year - 1980) shl 9);
  end;
end;

function WinFileDateToDateTime(FileDate: UInt32; out DateTime: TDateTime): Boolean;
var
  LDate: TDateTime;
  LTime: TDateTime;
begin
  Result := TryEncodeDate(
    LongRec(FileDate).Hi shr 9 + 1980,
    LongRec(FileDate).Hi shr 5 and 15,
    LongRec(FileDate).Hi and 31,
    LDate);

  if Result then
  begin
    Result := TryEncodeTime(
      LongRec(FileDate).Lo shr 11,
      LongRec(FileDate).Lo shr 5 and 63,
      LongRec(FileDate).Lo and 31 shl 1, 0, LTime);

    if Result then
      DateTime := LDate + LTime;
  end;
end;

procedure VerifyRead(Stream: TStream; Buffer: TBytes; Count: Integer); overload;
begin
  try
    Stream.ReadBuffer(Buffer, Count);
  except
    on E: EZipException do
      raise
    else
      Exception.RaiseOuterException(EZipException.CreateRes(@SZipErrorRead));
  end;
end;

procedure VerifyRead(Stream: TStream; var Buffer; Count: Integer); overload;
begin
  try
    Stream.ReadBuffer(Buffer, Count);
  except
    on E: EZipException do
      raise
    else
      Exception.RaiseOuterException(EZipException.CreateRes(@SZipErrorRead));
  end;
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

type
  TExtraField = packed record
    FieldId: Word;
    FieldLen: Word;
  // Data: Array[FieldLen] of Byte
  end;

function GetExtraField(const Data: TBytes; FieldId, FieldLen: Word; Extra: Pointer): Integer;
var
  Offset: Integer;
  pField: ^TExtraField;
  Count: Integer;
begin
  Offset := 0;
  Count := Length(Data);
  while Offset + SizeOf(TExtraField) < Count do
  begin
    pField := @Data[Offset];
    if pField.FieldId = FieldId then
    begin
      Result := pField.FieldLen;
      if Extra <> nil then
      begin
        if Result < FieldLen then
          FieldLen := Result;
        Move(Data[Offset + SizeOf(TExtraField)], Extra^, FieldLen);
      end;
      Exit;
    end;
    Inc(Offset, SizeOf(TExtraField) + pField.FieldLen);
  end;
  Result := 0;
end;

procedure DelExtraField(var Data: TBytes; FieldId: Word);
var
  Offset: Integer;
  pField: ^TExtraField;
  Count: Integer;
begin
  Offset := 0;
  Count := Length(Data);
  while Offset + SizeOf(TExtraField) < Count do
  begin
    pField := @Data[Offset];
    if pField.FieldId = FieldId then
    begin
      Delete(Data, Offset, SizeOf(TExtraField) + pField.FieldLen);
      Exit;
    end;
    Inc(Offset, SizeOf(TExtraField) + pField.FieldLen);
  end;
end;

procedure SetExtraField(var Data: TBytes; FieldId, FieldLen: Word; Extra: Pointer);
var
  Offset: Integer;
  pField: ^TExtraField;
  Count: Integer;
  Len: Integer;
begin
  if FieldLen = 0 then
  begin
    DelExtraField(Data, FieldId);
    Exit;
  end;
  Offset := 0;
  Count := Length(Data);
  while Offset + SizeOf(TExtraField) < Count do
  begin
    pField := @Data[Offset];
    Len := SizeOf(TExtraField) + pField.FieldLen;
    if Offset + Len > Count then
      Exit;
    if pField.FieldId = FieldId then
    begin
      Inc(Offset, SizeOf(TExtraField));
      Len := Integer(FieldLen) - pField.FieldLen;
      if Len < 0 then
      begin
        pField.FieldLen := FieldLen;
        Delete(Data, Offset, -Len);
      end else
      if Len > 0 then
      begin
        pField.FieldLen := FieldLen;
        SetLength(Data, Length(Data) + Len);
        Move(Data[Offset], Data[Offset + Len], Length(Data) - Offset - Len);
      end;
      Move(Extra^, Data[Offset], FieldLen);
      Exit;
    end;
    Inc(Offset, Len);
  end;
  Count := Length(Data);
  SetLength(Data, Count + SizeOf(TExtraField) + FieldLen);
  pField := @Data[Count];
  pField.FieldId := FieldId;
  pField.FieldLen := FieldLen;
  Inc(Count, SizeOf(TExtraField));
  Move(Extra^, Data[Count], FieldLen);
end;

type
  TZipDecryptStream = class(TStream)
  private
    FDecryptor: IZipCryptor;
    FStream: TStream;
    FStart: Int64;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(AStream: TStream; ACRC32: Cardinal;
      const ACryptor: IZipCryptor; const APassword: string);
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
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

  TCRCStream = class(TStream)
  private
    FStream: TStream;
    FExpectedCRC: Cardinal;
    FEncrypted: Boolean;
    FSize: UInt64;
    FCRC: Cardinal;
  protected
    function GetSize: Int64; override;
  public
    constructor Create(AStream: TStream; const AHeader: TZipHeader);
    destructor Destroy; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
  end;

{ TZipHeader }

function TZipHeader.Get64ExtraHeaderField(const Header: TZip64ExtraHeader;
  Size, Field: Integer): UInt64;

  procedure Error;
  begin
    raise EZipException.CreateRes(@SZipErrorRead) at ReturnAddress;
  end;

var
  P: PByte;
begin
  P := @Header;
  if UncompressedSize = $FFFFFFFF then
  begin
    if Size < SizeOf(UInt64) then Error;
    if Field = 0 then Exit(PUInt64(P)^);
    Inc(P, SizeOf(UInt64));
    Dec(Size, SizeOf(UInt64));
  end
  else
    if Field = 0 then Exit(UncompressedSize);
  if CompressedSize = $FFFFFFFF then
  begin
    if Size < SizeOf(UInt64) then Error;
    if Field = 1 then Exit(PUInt64(P)^);
    Inc(P, SizeOf(UInt64));
    Dec(Size, SizeOf(UInt64));
  end
  else
    if Field = 1 then Exit(CompressedSize);
  if LocalHeaderOffset = $FFFFFFFF then
  begin
    if Size < SizeOf(UInt64) then Error;
    if Field = 2 then Exit(PUInt64(P)^);
    Inc(P, SizeOf(UInt64));
    Dec(Size, SizeOf(UInt64));
  end
  else
    if Field = 2 then Exit(LocalHeaderOffset);
  if DiskNumberStart = $FFFF then
  begin
    if Size < SizeOf(UInt32) then Error;
    if Field = 3 then Exit(PUInt32(P)^);
  end
  else
    if Field = 3 then Exit(DiskNumberStart);
  Error;
  Result := 0;
end;

function TZipHeader.GetUncompressedSize64: UInt64;
var
  Extra: TZip64ExtraHeader;
  Size: Integer;
begin
  Size := GetExtraField(ExtraField, ZIP64_EXTRAHEADER, SizeOf(Extra), @Extra);
  Result := Get64ExtraHeaderField(Extra, Size, 0);
end;

procedure TZipHeader.SetUncompressedSize64(Value: UInt64);
var
  Extra: TZip64ExtraHeader;
  Size: Integer;
begin
  Size := GetExtraField(ExtraField, ZIP64_EXTRAHEADER, SizeOf(Extra), @Extra);
  if (Value >= ZIP64_MAXINT) or (Size > 1 * 8) then
  begin
    RequiredVersion := 45;
    Extra.UncompressedSize := Value;
    UncompressedSize := ZIP64_MAXINT;
    if Size = 0 then
      Size := 1 * 8;
    // if we don't know the CompressedSize, we reserve place for a 64bits value
    if (CompressedSize = 0) and (Size < 2 * 8) then
    begin
      Extra.CompressedSize := 0;
      CompressedSize := ZIP64_MAXINT;
      Size := 2 * 8;
    end;
  end else begin
    UncompressedSize := Value;
    Size := 0;
  end;
  if (Size <> 0) or (ExtraField <> nil) then
  begin
    SetExtraField(ExtraField, ZIP64_EXTRAHEADER, Size, @Extra);
    ExtraFieldLength := Length(ExtraField);
  end;
  if Size = 0 then
    RequiredVersion := 20;
end;

function TZipHeader.GetCompressedSize64: UInt64;
var
  Extra: TZip64ExtraHeader;
  Size: Integer;
begin
  Size := GetExtraField(ExtraField, ZIP64_EXTRAHEADER, SizeOf(Extra), @Extra);
  Result := Get64ExtraHeaderField(Extra, Size, 1);
end;

procedure TZipHeader.SetCompressedSize64(Value: UInt64);
var
  Extra: TZip64ExtraHeader;
  Size: Integer;
begin
  Size := GetExtraField(ExtraField, ZIP64_EXTRAHEADER, SizeOf(Extra), @Extra);
  // if Size = 2 * 8 we keep the ZIP64 Extension size, even if Value is less then ZIP64_MAXINT
  if (Value >= ZIP64_MAXINT) or (Size >= 2 * 8) then
  begin
    RequiredVersion := 45;
    Extra.CompressedSize := Value;
    CompressedSize := ZIP64_MAXINT;
    if Size < 2 * 8 then
      Size := 2 * 8;
  end else begin
    CompressedSize := Value;
  end;
  if (Size > 0) or (ExtraField <> nil) then
  begin
    SetExtraField(ExtraField, ZIP64_EXTRAHEADER, Size, @Extra);
    ExtraFieldLength := Length(ExtraField);
  end;
end;

function TZipHeader.GetLocalHeaderOffset64: UInt64;
var
  Extra: TZip64ExtraHeader;
  Size: Integer;
begin
  Size := GetExtraField(ExtraField, ZIP64_EXTRAHEADER, SizeOf(Extra), @Extra);
  Result := Get64ExtraHeaderField(Extra, Size, 2);
end;

procedure TZipHeader.SetLocalHeaderOffset64(Value: UInt64);
var
  Extra: TZip64ExtraHeader;
  Size : Integer;
begin
  Size := GetExtraField(ExtraField, ZIP64_EXTRAHEADER, SizeOf(Extra), @Extra);
  if Value >= ZIP64_MAXINT then
  begin
    RequiredVersion := 45;
    if Size = 0 then
    begin
      Extra.UncompressedSize := UncompressedSize;
      UncompressedSize := ZIP64_MAXINT;
    end;
    if Size < 2 * 8 then
    begin
      Extra.CompressedSize := CompressedSize;
      CompressedSize := ZIP64_MAXINT;
    end;
    Extra.LocalHeaderOffset := Value;
    LocalHeaderOffset := ZIP64_MAXINT;
    Size := 3 * 8;
  end else begin
    LocalHeaderOffset := Value;
    if Size = 3 * 8 then
      Size := 2 * 8;
  end;
  if (Size > 0) or (ExtraField <> nil) then
  begin
    SetExtraField(ExtraField, ZIP64_EXTRAHEADER, Size, @Extra);
    ExtraFieldLength := Length(ExtraField);
  end;
  if Size = 0 then
    RequiredVersion := 20;
end;

function TZipHeader.GetModifiedTime: TDateTime;
begin
  if not WinFileDateToDateTime(ModifiedDateTime, Result) then
    Result := Default(TDateTime);
end;

procedure TZipHeader.SetModifiedTime(const Value: TDateTime);
begin
  ModifiedDateTime := DateTimeToWinFileDate(Value);
end;

function TZipHeader.GetFileAttributes: TFileAttributes;
begin
{$IFDEF MSWINDOWS}
  if (Hi(MadeByVersion) = MADEBY_MSDOS) then
    Result := TFile.IntegerToFileAttributes(ExternalAttributes and $000000FF)
  else
    Result := [];
{$ENDIF}
{$IFDEF POSIX}
  if (Hi(MadeByVersion) = MADEBY_UNIX) and (ExternalAttributes shr 16 <> 0) then
    Result := TFile.IntegerToFileAttributes(ExternalAttributes shr 16)
  else
    Result := [];
{$ENDIF}
end;

procedure TZipHeader.SetFileAttributes(const Value: TFileAttributes);
begin
  ExternalAttributes := UInt32(TFile.FileAttributesToInteger(Value)){$IFDEF POSIX} shl 16{$ENDIF};
end;

function TZipHeader.GetFlag(Index: Integer): Boolean;
begin
  Result := (Flag and (1 shl Index)) <> 0;
end;

procedure TZipHeader.SetFlag(Index: Integer; Value: Boolean);
begin
  if Value then
    Flag := Flag or (1 shl Index)
  else
    Flag := Flag and not (1 shl Index);
end;

{ TZipFile }

function TZipFile.GetComment: string; // System comment.
var
  E: TEncoding;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  if UTF8Support then
    E := TEncoding.UTF8
  else
    E := Encoding;
  Result := E.GetString(FComment);
end;

function TZipFile.GetEncoding: TEncoding;

  function GetDefaultEncoding: TEncoding;
  var
    E: TEncoding;
  begin
    if FDefaultEncoding = nil then
    begin
{$IFDEF MSWINDOWS}
      E := TEncoding.GetEncoding(Winapi.Windows.GetOEMCP);
{$ELSE}
      E := TEncoding.GetEncoding(437);
{$ENDIF}
{$IFDEF AUTOREFCOUNT}
      E.__ObjAddRef;
{$ENDIF AUTOREFCOUNT}
      if AtomicCmpExchange(Pointer(FDefaultEncoding), Pointer(E), nil) <> nil then
        E.Free;
    end;
    Result := FDefaultEncoding;
  end;

begin
  if FEncoding = nil then
    Result := GetDefaultEncoding
  else
    Result := FEncoding;
end;

function TZipFile.GetFileComment(Index: Integer): string;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  CheckIndex(Index);
  Result := GetTextEncode(FFiles[Index]).GetString(FFiles[Index].FileComment);
end;

function TZipFile.GetFileCount: Integer;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  Result := Length(FFiles);
end;

function TZipFile.GetFileInfo(Index: Integer): TZipHeader;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  CheckIndex(Index);
  Result := FFiles[Index];
end;

function TZipFile.GetFileInfos: TArray<TZipHeader>;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  Result := FFiles;
end;

function TZipFile.InternalGetFileName(Index: Integer): string;
begin
  Result := GetTextEncode(FFiles[Index]).GetString(FFiles[Index].FileName);
end;

function TZipFile.GetFileName(Index: Integer): string;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  CheckIndex(Index);
  Result := InternalGetFileName(Index);
end;

function TZipFile.GetFileNames: TArray<string>;
var
  I: Integer;
begin
  if FMode = zmClosed then
    raise EZipException.CreateRes(@SZipNotOpen);
  SetLength(Result, Length(FFiles));
  for I := 0 to High(Result) do
    Result[I] := InternalGetFileName(I);
end;

procedure TZipFile.ReadCentralHeader;
var
  I: UInt64;
  Signature: UInt32;
  LEndHeader: TZipEndOfCentralHeader;
  LEndHeader64: TZip64EndOfCentralHeader;
  LHeader64: TZip64Header;
  LHeader: TZipHeader;
begin
  FFiles := nil;
  if FStream.Size = 0 then
    Exit;
  // Read End Of Centeral Direcotry Header
  if not LocateEndOfCentralHeader(LEndHeader) then
    raise EZipException.CreateRes(@SZipErrorRead);
  // ZIP64 End Of Central Directory Header
  if LEndHeader.CentralDirOffset = ZIP64_MAXINT then
  begin
    VerifyRead(FStream, LEndHeader64.Signature, SizeOf(LEndHeader64));
    if LEndHeader64.Signature <> SIGNATURE_ZIP64ENDOFHEADER then
      raise EZipException.CreateRes(@SZipErrorRead);
    FStream.Position := LEndHeader64.Zip64CentralDirOffset;
    VerifyRead(FStream, LHeader64.Signature, SizeOf(TZip64Header));
    if LHeader64.Signature <> SIGNATURE_ZIP64CENTRALHEADER then
      raise EZipException.CreateRes(@SZipErrorRead);
    FStream.Position := LHeader64.CentralDirOffset;
    FEndFileData := LHeader64.CentralDirOffset;
  end else begin
    // Move to the beginning of the CentralDirectory
    FStream.Position := LEndHeader.CentralDirOffset;
    // Save Begginning of Central Directory. This is where new files
    // get written to, and where the new central directory gets written when
    // closing.
    FEndFileData := LEndHeader.CentralDirOffset;
    LHeader64.CentralDirEntries := LEndHeader.CentralDirEntries;
  end;
  // Read File Headers
  SetLength(FFiles, LHeader64.CentralDirEntries);
  for I := 1 to LHeader64.CentralDirEntries do
  begin
    // Verify Central Header signature
    VerifyRead(FStream, Signature, Sizeof(Signature));
    if Signature <> SIGNATURE_CENTRALHEADER then
      raise EZipException.CreateRes(@SZipInvalidCentralHeader);
    // Read Central Header
    LHeader := Default(TZipHeader);
    VerifyRead(FStream, LHeader.MadeByVersion, CENTRALHEADERSIZE);
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
    // Save File Header in interal list
    FFiles[I - 1] := LHeader;
  end;
end;

procedure TZipFile.SetComment(Value: string);
var
  E: TEncoding;
begin
  if UTF8Support then
    E := TEncoding.UTF8
  else
    E := Encoding;
  FComment := E.GetBytes(Value);

  if not (FMode in [zmReadWrite, zmWrite]) then
    raise EZipException.CreateRes(@SZipNoWrite);
  if Length(FComment) > MaxCommentLength then
    SetLength(FComment, MaxCommentLength);
end;

procedure TZipFile.SetFileComment(Index: Integer; Value: string);
var
  LFile: TZipHeader;
begin
  if not (FMode in [zmReadWrite, zmWrite]) then
    raise EZipException.CreateRes(@SZipNoWrite);
  CheckIndex(Index);

  LFile := FFiles[Index];
  LFile.UTF8Support := UTF8Support;
  LFile.FileComment := GetTextEncode(LFile).GetBytes(Value);
  if Length(LFile.FileComment) > MaxCommentLength then
    SetLength(LFile.FileComment, MaxCommentLength);
  LFile.FileCommentLength := Length(LFile.FileComment);
  FFiles[Index] := LFile;
end;

procedure TZipFile.SetUTF8Support(const Value: Boolean);
begin
  if Value = FUTF8Support then Exit;
  if not (FMode in [zmReadWrite, zmWrite]) then
    raise EZipException.CreateRes(@SZipNoWrite);
  FUTF8Support := Value;
end;

class constructor TZipFile.Create;
begin
  FCompressionHandler := TCompressionDict.Create;

  RegisterCompressionHandler(zcStored,
    function(InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader): TStream
    begin
      Result := TProxySubrangeStream.Create(InStream, -1, -1, False);
    end,
    function(InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader): TStream
    begin
      Result := TProxySubrangeStream.Create(InStream, InStream.Position,
        Item.UncompressedSize64, InStream <> ZipFile.FStream);
    end);

  RegisterCompressionHandler(zcDeflate,
    function(InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader): TStream
    begin
      Result := TZCompressionStream.Create(InStream, zcDefault, -15);
    end,
    function(InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader): TStream
    begin
      Result := TZDecompressionStream.Create(InStream, -15, InStream <> ZipFile.FStream);
    end);

{$IFDEF MSWINDOWS}
  RegisterCompressionHandler(zcDeflate64,
    nil,
    function(InStream: TStream; const ZipFile: TZipFile; const Item: TZipHeader): TStream
    begin
      Result := TZInflate9Stream.Create(InStream, InStream <> ZipFile.FStream);
    end);
{$ENDIF}
end;

class destructor TZipFile.Destroy;
begin
  FreeAndNil(FCompressionHandler);
  FreeAndNil(FDefaultEncoding);
end;

class procedure TZipFile.RegisterCompressionHandler(
  Compression: TZipCompression; const CompressStream, DecompressStream: TStreamConstructor);
begin
  FCompressionHandler.AddOrSetValue(Compression,
    TCompressionReg.Create(CompressStream, DecompressStream));
end;

class procedure TZipFile.UnRegisterCompressionHandler(
  Compression: TZipCompression);
begin
  FCompressionHandler.Remove(Compression);
end;

procedure TZipFile.CheckCompressionSupported(Compression: TZipCompression);
var
  LReg: TCompressionReg;
begin
  if not FCompressionHandler.TryGetValue(Compression, LReg) or
     not Assigned(LReg.Key) then
  begin
    var s: string;
    if Assigned(LReg.Value) then
      s := SZipOnlyDecompressionSup;
    raise EZipException.CreateResFmt(@SZipCompressionNotSup,
      [TZipCompressionToString(Compression) + s]) at ReturnAddress;
  end;
end;

procedure TZipFile.CheckDecompressionSupported(Compression: TZipCompression);
var
  LReg: TCompressionReg;
begin
  if not FCompressionHandler.TryGetValue(Compression, LReg) or
     not Assigned(LReg.Value) then
  begin
    var s: string;
    if Assigned(LReg.Value) then
      s := SZipOnlyCompressionSup;
    raise EZipException.CreateResFmt(@SZipDecompressionNotSup,
      [TZipCompressionToString(Compression) + s]) at ReturnAddress;
  end;
end;

procedure TZipFile.CheckIndex(Index: Integer);
var
  LHigh: Integer;
begin
  LHigh := Length(FFiles) - 1;
  if Cardinal(Index) > Cardinal(LHigh) then
    raise EZipException.Create(ListIndexErrorMsg(Index, LHigh, Self)) at ReturnAddress;
end;

class function TZipFile.IsValid(const ZipFileName: string): Boolean;
var
  Stream: TStream;
begin
  Stream := TFileStream.Create(ZipFileName, fmOpenRead or fmShareDenyWrite);
  try
    Result := IsValid(Stream);
  finally
    Stream.Free;
  end;
end;

class function TZipFile.IsValid(Stream: TStream): Boolean;
var
  Z: TZipFile;
  Header: TZipEndOfCentralHeader;
begin
  Result := False;
  try
    Z := TZipFile.Create;
    try
      Z.FStream := Stream;
      Result := Z.LocateEndOfCentralHeader(Header);
    finally
      Z.Free;
    end;
  except
    on E: EStreamError do
      // Swallow only Stream exceptions and return False
  end;
end;

function TZipFile.LocateEndOfCentralHeader(var Header: TZipEndOfCentralHeader): Boolean;
var
  I: Integer;
  LBackRead, LReadSize, LMaxBack, L: UInt32;
  LBackBuf: TBytes;
  LLastPosition: Int64;
begin
  L := SizeOf(TZipEndOfCentralHeader) + SizeOf(UInt32);
  LMaxBack := MaxCommentLength + L;
  if FStream.Size < LMaxBack then
    LMaxBack := FStream.Size;
  LBackRead := L;
  SetLength(LBackBuf, 50 * L);
  LLastPosition := Low(Int64);
  while LBackRead <= LMaxBack do
  begin
    if LBackRead + UInt32(Length(LBackBuf)) - L > LMaxBack then
      LBackRead := LMaxBack
    else
      Inc(LBackRead, UInt32(Length(LBackBuf)) - L);
    FStream.Position := FStream.Size - LBackRead;
    if FStream.Position = LLastPosition then
      Break;
    LLastPosition := FStream.Position;
    if Length(LBackBuf) < (FStream.Size - FStream.Position) then
      LReadSize := Length(LBackBuf)
    else
      LReadSize := FStream.Size - FStream.Position;
    VerifyRead(FStream, LBackBuf, LReadSize);

    for I := LReadSize - L downto 0 do
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
          VerifyRead(FStream, FComment, Header.CommentLength);
        end
        else
          SetLength(FComment, 0);
        if Header.CentralDirOffset = ZIP64_MAXINT then
        begin
          FStream.Position := FStream.Size - LBackRead + I - SizeOf(TZip64EndOfCentralHeader);
        end;
        Exit(True);
      end;
    end;
  end;
  Result := False;
end;

class procedure TZipFile.ExtractZipFile(const ZipFileName: string; const Path: string; ZipProgress: TZipProgressEvent);
begin
  ExtractZipFile(ZipFileName, Path, nil, ZipProgress);
end;

class procedure TZipFile.ExtractZipFile(const ZipFileName: string; const Path: string; const Encoding: TEncoding; ZipProgress: TZipProgressEvent);
var
  LZip: TZipFile;
begin
  LZip := TZipFile.Create;
  try
    LZip.Encoding := Encoding;
    if Assigned(ZipProgress) then
      LZip.OnProgress := ZipProgress;
    LZip.Open(ZipFileName, zmRead);
    LZip.ExtractAll(Path);
    LZip.Close;
  finally
    LZip.Free;
  end;
end;

class procedure TZipFile.ZipDirectoryContents(const ZipFileName: string; const Path: string;
  Compression: TZipCompression; ZipProgress: TZipProgressEvent);
begin
  ZipDirectoryContents(ZipFileName, Path, nil, Compression, ZipProgress);
end;

class procedure TZipFile.ZipDirectoryContents(const ZipFileName: string; const Path: string;
  const Encoding: TEncoding; Compression: TZipCompression; ZipProgress: TZipProgressEvent);
var
  LZipFile: TZipFile;
  LFile: string;
  LZFile: string;
  LPath: string;
  LFiles: TStringDynArray;
begin
  LZipFile := TZipFile.Create;
  try
    LZipFile.Encoding := Encoding;
    if Assigned(ZipProgress) then
      LZipFile.OnProgress := ZipProgress;
    if TFile.Exists(ZipFileName) then
      TFile.Delete(ZipFileName);
    LFiles := TDirectory.GetFiles(Path, '*', TSearchOption.soAllDirectories);
    LZipFile.Open(ZipFileName, zmWrite);
    LPath := System.SysUtils.IncludeTrailingPathDelimiter(Path);
    for LFile in LFiles do
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

// Extract Unicode Path
// Based on section 4.6.9 -Info-ZIP Unicode Path Extra Field (0x7075) from
// https://pkware.cachefly.net/webdocs/casestudies/APPNOTE.TXT
// Stores the UTF-8 version of the file name field as stored in the
//       local header and central directory header. (Last Revision 20070912)
//
//         Value         Size        Description
//         -----         ----        -----------
// (UPath) 0x7075        Short       tag for this extra block type ("up")
//         TSize         Short       total data size for this block
//         Version       1 byte      version of this extra field, currently 1
//         NameCRC32     4 bytes     File Name Field CRC32 Checksum
//         UnicodeName   Variable    UTF-8 version of the entry File Name
class function TZipFile.GetUTF8PathFromExtraField(const AHeader: TZipHeader; out AFileName: string): Boolean;
const
  UPATH = $7075;
  SIZEPOS = 2;
  CRCPOS = 5;
  PATHPOS = 9;
  PATHSIZESUB = 5;
var
  I: Integer;
  LTotalSize: Word;
  LCRC: Cardinal;
  LPathCRC: Cardinal;
begin
  Result := False;
  for I := 0 to AHeader.ExtraFieldLength - 2 do
  begin
    if PWord(@AHeader.ExtraField[I])^ = UPATH then
    begin
      LTotalSize := PWord(@AHeader.ExtraField[I + SIZEPOS])^;
      LCRC := PCardinal(@AHeader.ExtraField[I + CRCPOS])^;
      LPathCRC := crc32(0, nil, 0);
      LPathCRC := crc32(LPathCRC, @AHeader.FileName[0], Length(AHeader.FileName));
      if LPathCRC = LCRC then
      begin
        AFileName := TEncoding.UTF8.GetString(AHeader.ExtraField, I + PATHPOS, LTotalSize - PATHSIZESUB);
        Result := True;
      end;
      Break;
    end;
  end;
end;

constructor TZipFile.Create;
begin
  inherited Create;
  FMode := zmClosed;
  FUTF8Support := True;
end;

destructor TZipFile.Destroy;
begin
  // In case a file is open for writing currently
  Close;
  inherited;
end;

procedure TZipFile.DoZLibProgress(Sender: TObject);
begin
  if Assigned(FOnProgress) then
    FOnProgress(Self, FCurrentFile, FCurrentHeader, (Sender as TStream).Position);
end;

procedure TZipFile.Open(const ZipFileName: string; OpenMode: TZipMode);
var
  LMode: LongInt;
  LFileStream: TFileStream;
begin
  // In case the user had a file open
  Close;
  case OpenMode of
    zmRead:      LMode := fmOpenRead or fmShareDenyWrite;
    zmReadWrite: LMode := fmOpenReadWrite;
    zmWrite:     LMode := fmCreate;
    else         raise EZipException.CreateRes(@sArgumentInvalid);
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
  LHeader64: TZip64Header;
  LEndHeader64: TZip64EndOfCentralHeader;
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
      for I := 0 to Length(FFiles) - 1 do
      begin
        LHeader := FFiles[I];
        VerifyWrite(FStream, Signature, SizeOf(Signature));
        VerifyWrite(FStream, LHeader.MadeByVersion, CENTRALHEADERSIZE);
        if LHeader.FileNameLength <> 0 then
          VerifyWrite(FStream, LHeader.FileName, LHeader.FileNameLength);
        if LHeader.ExtraFieldLength <> 0 then
          VerifyWrite(FStream, LHeader.ExtraField, LHeader.ExtraFieldLength);
        if LHeader.FileCommentLength <> 0 then
          VerifyWrite(FStream, LHeader.FileComment, LHeader.FileCommentLength);
      end;
      // Only support writing single disk .ZIP files
      FillChar(LEndOfHeader, Sizeof(LEndOfHeader), 0);

      if (Length(FFiles) >= $FFFF) or (FEndFileData >= ZIP64_MAXINT) then
      begin
        LHeader64.Signature := SIGNATURE_ZIP64CENTRALHEADER;
        LHeader64.HeaderSize := 44;
        LHeader64.MadeByVersion := 45;
        LHeader64.RequiredVersion := 45;
        LHeader64.NumberOfDisks := 0;
        LHeader64.CentralDirStartDisk := 0;
        LHeader64.NumEntriesThisDisk := Length(FFiles);
        LHeader64.CentralDirEntries := Length(FFiles);
        LHeader64.CentralDirSize := FStream.Position - FEndFileData;
        LHeader64.CentralDirOffset := FEndFileData;

        LEndHeader64.Signature := SIGNATURE_ZIP64ENDOFHEADER;
        LEndHeader64.CentralDirStartDisk := 0;
        LEndHeader64.Zip64CentralDirOffset := FStream.Position;
        LEndHeader64.TotalNumberOfDisks := 1;

        VerifyWrite(FStream, LHeader64, SizeOf(LHeader64));
        VerifyWrite(FStream, LEndHeader64, SizeOf(LEndHeader64));

        LEndOfHeader.CentralDirEntries := $FFFF;
        LEndOfHeader.NumEntriesThisDisk := $FFFF;
        LEndOfHeader.CentralDirSize := ZIP64_MAXINT;
        LEndOfHeader.CentralDirOffset := ZIP64_MAXINT;
      end else begin
        LEndOfHeader.CentralDirEntries := Length(FFiles);
        LEndOfHeader.NumEntriesThisDisk := Length(FFiles);
        LEndOfHeader.CentralDirSize := FStream.Position - FEndFileData;
        LEndOfHeader.CentralDirOffset := FEndFileData;
      end;
      // Truncate comment if it's too long
      if Length(FComment) > MaxCommentLength then
        SetLength(FComment, MaxCommentLength);
      LEndofHeader.CommentLength := Length(FComment);
      // Write End Of Centeral Directory
      Signature := SIGNATURE_ZIPENDOFHEADER;
      VerifyWrite(FStream, Signature, SizeOf(Signature));
      VerifyWrite(FStream, LEndOfHeader.DiskNumber, SizeOf(LEndOfHeader));
      if LEndOfHeader.CommentLength > 0 then
        VerifyWrite(FStream, FComment, LEndOfHeader.CommentLength);
      FStream.Size := FStream.Position;
    end;
  finally
    FMode := zmClosed;
    FFiles := nil;
    FStream := nil;
    FEndFileData := 0;
    FreeAndNil(FFileStream);
  end;
end;

procedure TZipFile.Extract(const FileName: string; const Path: string; CreateSubDirs: Boolean);
begin
  Extract(GetFileIndex(FileName), Path, CreateSubdirs);
end;

procedure TZipFile.Extract(Index: Integer; const Path: string; CreateSubdirs: Boolean);
var
  LInStream, LOutStream: TStream;
  LHeader: TZipHeader;
  LDir, LFileName: string;
  LModifiedDateTime: TDateTime;
  LBuffer: TBytes;
  LLen: Integer;
  LAttrs: TFileAttributes;
  LLinkTarget: string;
begin
  // Get decompression stream for file
  Read(Index, LInStream, LHeader, True);
  FCurrentHeader := LHeader;
  try
    if not GetUTF8PathFromExtraField(LHeader, LFileName) then
      LFileName := GetTextEncode(FFiles[Index]).GetString(FFiles[Index].FileName);
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

    LAttrs := FFiles[Index].FileAttributes;

    if TFileAttribute.faDirectory in LAttrs then
    begin
      TDirectory.CreateDirectory(LFileName);

      if DirectoryExists(LFileName, False) then
      begin
        if WinFileDateToDateTime(LHeader.ModifiedDateTime, LModifiedDateTime) then
        begin
          TDirectory.SetCreationTime(LFileName, LModifiedDateTime);
          TDirectory.SetLastWriteTime(LFileName, LModifiedDateTime);
        end;
{$IFDEF MSWINDOWS}
        if (Hi(FFiles[Index].MadeByVersion) = MADEBY_MSDOS) then
          TDirectory.SetAttributes(LFileName, LAttrs);
{$ENDIF}
{$IFDEF POSIX}
        if (Hi(FFiles[Index].MadeByVersion) = MADEBY_UNIX) and (FFiles[Index].ExternalAttributes shr 16 <> 0) then
          TDirectory.SetAttributes(LFileName, LAttrs);
{$ENDIF}
      end;
    end

    else if TFileAttribute.faSymLink in LAttrs then
    begin
      FCurrentFile := LFileName;
      try
        LLen := FFiles[Index].UncompressedSize64;
        if LLen > 0 then
        begin
          SetLength(LBuffer, LLen);
          VerifyRead(LInStream, LBuffer, LLen);
          LLinkTarget := TEncoding.UTF8.GetString(LBuffer);
          TFile.CreateSymLink(LFileName, LLinkTarget);
        end;
        if Assigned(FOnProgress) then
          FOnProgress(Self, FCurrentFile, FCurrentHeader, LLen);
      finally
        FCurrentFile := '';
      end;
    end

    else
    begin
      LOutStream := TFileStream.Create(LFileName, fmCreate);
      // And Copy from the decompression stream.
      try
        FCurrentFile := LFileName;
        LOutStream.CopyFrom(LInStream);
        if Assigned(FOnProgress) then
          FOnProgress(Self, FCurrentFile, FCurrentHeader, LOutStream.Position);
      finally
        LOutStream.Free;
        FCurrentFile := '';
      end;

      if FileExists(LFileName, False) then
      begin
        if WinFileDateToDateTime(LHeader.ModifiedDateTime, LModifiedDateTime) then
        begin
          TFile.SetCreationTime(LFileName, LModifiedDateTime);
          TFile.SetLastWriteTime(LFileName, LModifiedDateTime);
        end;
{$IFDEF MSWINDOWS}
        if (Hi(FFiles[Index].MadeByVersion) = MADEBY_MSDOS) then
          TFile.SetAttributes(LFileName, LAttrs);
{$ENDIF}
{$IFDEF POSIX}
        if (Hi(FFiles[Index].MadeByVersion) = MADEBY_UNIX) and (FFiles[Index].ExternalAttributes shr 16 <> 0) then
          TFile.SetAttributes(LFileName, LAttrs);
{$ENDIF}
      end;
    end;

  finally
    FCurrentHeader := Default(TZipHeader);
    LInStream.Free;
  end;
end;

procedure TZipFile.ExtractAll(const Path: string);
var
  I: Integer;
begin
  if not (FMode in [zmReadWrite, zmRead]) then
    raise EZipException.CreateRes(@SZipNoRead);
  for I := 0 to Length(FFiles) - 1 do
    Extract(I, Path);
end;

procedure TZipFile.Read(const FileName: string; out Bytes: TBytes);
begin
  Read(GetFileIndex(FileName), Bytes);
end;

procedure TZipFile.Read(Index: Integer; out Bytes: TBytes);
var
  LStream: TStream;
  LHeader: TZipHeader;
  UncompressedSize: UInt64;
begin
  Read(Index, LStream, LHeader, True);
  try
    UncompressedSize := FFiles[Index].UncompressedSize64;
    SetLength(Bytes, UncompressedSize);
    // Special case for empty files.
    if UncompressedSize > 0 then
      VerifyRead(LStream, Bytes, UncompressedSize);
  finally
    LStream.Free;
  end;
end;

procedure TZipFile.Read(const FileName: string; out Stream: TStream;
  out LocalHeader: TZipHeader; CheckCrc: Boolean);
begin
  Read(GetFileIndex(FileName), Stream, LocalHeader, CheckCrc);
end;

procedure TZipFile.Read(Index: Integer; out Stream: TStream;
  out LocalHeader: TZipHeader; CheckCrc: Boolean);
var
  Signature: UInt32;
  StreamCons: TStreamConstructor;
  Compression: TZipCompression;
  LIsEncrypted: Boolean;
  LStream: TStream;
begin
  if not (FMode in [zmReadWrite, zmRead]) then
    raise EZipException.CreateRes(@SZipNoRead);
  CheckIndex(Index);

  // Local Header doesn't have these fields
  LocalHeader.MadeByVersion := 0;
  SetLength(LocalHeader.FileComment, 0);
  LocalHeader.FileCommentLength  := 0;
  LocalHeader.DiskNumberStart    := 0;
  LocalHeader.InternalAttributes := 0;
  LocalHeader.ExternalAttributes := 0;
  LocalHeader.LocalHeaderOffset  := 0;

  // Move to beginning of Local Header
  FStream.Position := Int64(FFiles[Index].LocalHeaderOffset64) + FStartFileData;
  // Verify local header signature
  VerifyRead(FStream, Signature, Sizeof(Signature));
  if Signature <> SIGNATURE_LOCALHEADER then
    raise EZipException.CreateRes(@SZipInvalidLocalHeader);
  // Read local header
  VerifyRead(FStream, LocalHeader.RequiredVersion, LOCALHEADERSIZE);
  // Read Name and extra fields
  SetLength(LocalHeader.FileName, LocalHeader.FileNameLength);
  VerifyRead(FStream, LocalHeader.FileName, LocalHeader.FileNameLength);
  if LocalHeader.ExtraFieldLength > 0 then
  begin
    SetLength(LocalHeader.ExtraField, LocalHeader.ExtraFieldLength);
    VerifyRead(FStream, LocalHeader.ExtraField, LocalHeader.ExtraFieldLength);
  end;

  // Create Decryption stream, if required
  LIsEncrypted := LocalHeader.IsEncrypted;
  if Assigned(TZipFile.FOnCreateDecompressStream) then
    LStream := TZipFile.FOnCreateDecompressStream(FStream, Self, LocalHeader, LIsEncrypted)
  else if Assigned(TZipFile.FCreateDecompressStreamCallBack) then
    LStream := TZipFile.FCreateDecompressStreamCallBack(FStream, Self, LocalHeader, LIsEncrypted)
  else if LIsEncrypted and (FPassword <> '') then
  begin
    if not Assigned(Cryptor) then
      raise EZipException.CreateRes(@SZipCryptorNotAssigned);
    LStream := TZipDecryptStream.Create(FStream, LocalHeader.CRC32, Cryptor, FPassword);
  end
  else
    LStream := FStream;

  // Create Decompression stream
  Compression := TZipCompression(FFiles[Index].CompressionMethod);
  CheckDecompressionSupported(Compression);
  StreamCons := FCompressionHandler[Compression].Value;
  if LocalHeader.UseDataDescriptor then
  begin
    // LocalHeader can have 0 sizes if bit 3 is set
    LocalHeader.CRC32 := FFiles[Index].CRC32;
    LocalHeader.CompressedSize := FFiles[Index].CompressedSize;
    LocalHeader.UncompressedSize := FFiles[Index].UncompressedSize;
  end;
  Stream := StreamCons(LStream, Self, LocalHeader);
  if Stream is TZDecompressionStream then
  begin
    FCurrentHeader := LocalHeader;
    TZDecompressionStream(Stream).OnProgress := DoZLibProgress;
  end;

  // Create CRC checking stream, if required
  if CheckCrc then
    Stream := TCRCStream.Create(Stream, LocalHeader);
end;

procedure TZipFile.Add(Data: TStream; LocalHeader: TZipHeader; CentralHeader: PZipHeader);
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
  // Seek to End of zipped data
  LFileStart := FEndFileData + FStartFileData;
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
    CentralHeader^.IsEncrypted      := FPassword <> '';
    CentralHeader^.FileNameLength   := Length(CentralHeader^.FileName);
    CentralHeader^.ExtraFieldLength := Length(CentralHeader^.ExtraField);
  end;
  CentralHeader^.FileCommentLength  := Length(CentralHeader^.FileComment);

  // Write Signature, Header, and FileName
  FStream.Position := LFileStart;
  LSignature := SIGNATURE_LOCALHEADER;
  VerifyWrite(FStream, LSignature, SizeOf(LSignature));
  VerifyWrite(FStream, LocalHeader.RequiredVersion, LOCALHEADERSIZE);
  VerifyWrite(FStream, LocalHeader.FileName, LocalHeader.FileNameLength);
  if LocalHeader.ExtraFieldLength > 0 then
    VerifyWrite(FStream, LocalHeader.ExtraField, LocalHeader.ExtraFieldLength);
  // Save position to calculate Compressed Size
  LStartPos := FStream.Position;
  if Data <> nil then
  begin
    // Crypt
    if FPassword = '' then
      LStream := FStream
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
      LStream := TZipCryptStream.Create(FStream, LocalHeader.CRC32, Cryptor, FPassword);
    end;
    // Write Compressed data
    FCurrentHeader := LocalHeader;
    LCompression := TZipCompression(LocalHeader.CompressionMethod);
    CheckCompressionSupported(LCompression);
    LCompressStream := FCompressionHandler[LCompression].Key(LStream, Self, LocalHeader);
    if LCompressStream is TZCompressionStream then
      TZCompressionStream(LCompressStream).OnProgress := DoZLibProgress;
    try
      if FPassword <> '' then
        // Don't need to recompute CRC
        LCompressStream.CopyFrom(Data, LDataEnd - LDataStart)
      else
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
      if Assigned(FOnProgress) then
        FOnProgress(Self, FCurrentFile, FCurrentHeader, LCompressStream.Position);
    finally
      LCompressStream.Free;
      if LStream <> FStream then
        LStream.Free;
      FCurrentHeader := Default(TZipHeader);
    end;
  end;

  // Calculate CompressedSize
  LocalHeader.CompressedSize64 := FStream.Position - LStartPos;

  CentralHeader.UnCompressedSize := LocalHeader.UnCompressedSize;
  CentralHeader.CompressedSize := LocalHeader.CompressedSize;
  CentralHeader.CRC32 := LocalHeader.CRC32;
  CentralHeader.ExtraFieldLength := LocalHeader.ExtraFieldLength;
  CentralHeader.ExtraField := LocalHeader.ExtraField;
  // Save new End of zipped data mark
  FEndFileData := FStream.Position;
  // Move to beginning of Local Header offset and rewrite header
  // with correct CompressedSize and CRC32
  FStream.Position := LocalHeader.LocalHeaderOffset64 + SizeOf(UInt32);
  VerifyWrite(FStream, LocalHeader.RequiredVersion, LOCALHEADERSIZE);
  if LocalHeader.ExtraFieldLength > 0 then
  begin
    VerifyWrite(FStream, LocalHeader.FileName, LocalHeader.FileNameLength);
    VerifyWrite(FStream, LocalHeader.ExtraField, LocalHeader.ExtraFieldLength);
  end;
  L := Length(FFiles);
  SetLength(FFiles, L + 1);
  FFiles[L] := CentralHeader^;
end;

procedure TZipFile.AddDirectory(const DirName, ArchiveDirName: string);
begin
  if DirName <> '' then
  begin
    if not TDirectory.Exists(DirName) then
      raise EDirectoryNotFoundException.CreateRes(@SPathNotFound, DirName);
    Add(DirName, ArchiveDirName, TZipCompression.zcStored);
  end
  else
    Add(nil, ArchiveDirName, TZipCompression.zcStored, [TFileAttribute.faDirectory]);
end;

procedure TZipFile.Delete(const FileName: string);
begin
  Delete(GetFileIndex(FileName));
end;

procedure MoveUp(Stream: TStream; FromOffset: Int64; ToOffset: Int64; MoveCount: Int64);
var
  LBuffer: TBytes;
  LCount: Integer;
begin
  if MoveCount <= 0 then
    Exit;
  Assert(FromOffset > ToOffset);
  if MoveCount > DEFAULT_BUFFER_SIZE then
    LCount := DEFAULT_BUFFER_SIZE
  else
    LCount := MoveCount;
  SetLength(LBuffer, LCount);
  while LCount > 0 do
  begin
    Stream.Position := FromOffset;
    Stream.ReadData(LBuffer, LCount);
    Stream.Position := ToOffset;
    Stream.WriteData(LBuffer, LCount);
    Inc(FromOffset, LCount);
    Inc(ToOffset, LCount);
    Dec(MoveCount, LCount);
    if MoveCount < LCount then
      LCount := MoveCount;
  end;
end;

procedure TZipFile.Delete(Index: Integer);
var
  TargetOffset: Int64;
  SourceOffset: Int64;
  FileIndex: Integer;
  DeltaOffset: Int64;
  FileOffset: UInt64;
begin
  if not (FMode in [zmReadWrite, zmWrite]) then
    raise EZipException.CreateRes(@SZipNoWrite);
  CheckIndex(Index);

  TargetOffset := FFiles[Index].LocalHeaderOffset64;
  System.Delete(FFiles, Index, 1);

  // 4.4.1.3  The entries in the central directory MAY NOT necessarily
  //      be in the same order that files appear in the .ZIP file.
  SourceOffset := FEndFileData;
  for FileIndex := 0 to Length(FFiles) - 1 do
  begin
    FileOffset := FFiles[FileIndex].LocalHeaderOffset64;
    if (FileOffset > TargetOffset) and (FileOffset < SourceOffset) then
      SourceOffset := FileOffset;
  end;

  if SourceOffset < FEndFileData then
  begin
    // [....][TargetOffset...][SourceOffset....][...........][FEndFileData...]
    //       <----------------[.............................]
    MoveUp(FStream, SourceOffset, TargetOffset, FEndFileData - SourceOffset);
    DeltaOffset := SourceOffset - TargetOffset;
    Dec(FEndFileData, DeltaOffset);
    // Update LocalHeaderOffsets
    for FileIndex := 0 to Length(FFiles) - 1 do
    begin
      FileOffset := FFiles[FileIndex].LocalHeaderOffset64;
      if FileOffset > TargetOffset then
      begin
        FileOffset := FileOffset - UInt64(DeltaOffset);
        FFiles[FileIndex].LocalHeaderOffset64 := FileOffset;
      end;
    end;
  end else
  begin
    // it was the last entry, just truncate FEndFileData
    FEndFileData := TargetOffset;
  end;
end;

procedure TZipFile.Rename(const FileName, NewFileName: string);
begin
  Rename(GetFileIndex(FileName), NewFileName);
end;

procedure TZipFile.Rename(Index: Integer; const NewFileName: string);
var
  LFile: TZipHeader;
begin
  if not (FMode in [zmReadWrite, zmWrite]) then
    raise EZipException.CreateRes(@SZipNoWrite);
  CheckIndex(Index);

  LFile := FFiles[Index];
  LFile.UTF8Support := UTF8Support;
  LFile.FileName := GetTextEncode(LFile).GetBytes(NewFileName);
  LFile.FileNameLength := Length(LFile.FileName);
  FFiles[Index] := LFile;
end;

procedure TZipFile.CheckFileName(const ArchiveFileName: string);
begin
  if ArchiveFileName = '' then
    raise EZipException.CreateRes(@SZipFileNameEmpty);
end;

function TZipFile.GetTextEncode(const Header: TZipHeader): TEncoding;
begin
  if Header.UTF8Support then
    Result := TEncoding.UTF8
  else
    Result := Encoding;
end;

procedure TZipFile.Add(const FileName: string; const ArchiveFileName: string;
  Compression: TZipCompression);
var
  LInStream: TStream;
  LHeader: TZipHeader;
  LArchiveFileName: string;
begin
  CheckFileName(FileName);
  if not (FMode in [zmReadWrite, zmWrite]) then
    raise EZipException.CreateRes(@SZipNoWrite);
  CheckCompressionSupported(Compression);

  // Setup Header
  FillChar(LHeader, sizeof(LHeader), 0);
  FCurrentFile := FileName;
  LInStream := nil;
  try
    if TDirectory.Exists(FileName, False) then
    begin
      LHeader.ModifiedTime := TDirectory.GetLastWriteTime(FileName);
      LHeader.FileAttributes := TDirectory.GetAttributes(FileName);
    end
    else
    begin
      LInStream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
      LHeader.ModifiedTime := TFile.GetLastWriteTime(FileName);
      LHeader.FileAttributes := TFile.GetAttributes(FileName);
    end;
    {$IFDEF MSWINDOWS}
    LHeader.MadeByVersion := Word(MADEBY_MSDOS shl 8);
    {$ENDIF}
    {$IFDEF POSIX}
    LHeader.MadeByVersion := Word(MADEBY_UNIX shl 8);
    {$ENDIF}
    LHeader.CompressionMethod := UInt16(Compression);
    if ArchiveFileName <> '' then
      LArchiveFileName := ArchiveFileName
    else
      LArchiveFileName := ExtractFileName(FileName);
    LHeader.UTF8Support := FUTF8Support;
    LHeader.FileName := GetTextEncode(LHeader).GetBytes(LArchiveFileName);
    LHeader.FileNameLength := Length(LHeader.FileName);

    Add(LInStream, LHeader);
  finally
    LInStream.Free;
    FCurrentFile := '';
  end;
end;

procedure TZipFile.Add(Data: TBytes; const ArchiveFileName: string;
  Compression: TZipCompression);
var
  LInStream: TStream;
begin
  CheckFileName(ArchiveFileName);
  if not (FMode in [zmReadWrite, zmWrite]) then
    raise EZipException.CreateRes(@SZipNoWrite);
  CheckCompressionSupported(Compression);

  LInStream := TBytesStream.Create(Data);
  try
    Add(LInStream, ArchiveFileName, Compression);
  finally
    LInStream.Free;
  end;
end;

procedure TZipFile.Add(Data: TStream; const ArchiveFileName: string;
  Compression: TZipCompression; AExternalAttributes: TFileAttributes);
var
  LHeader: TZipHeader;
begin
  CheckFileName(ArchiveFileName);
  if not (FMode in [zmReadWrite, zmWrite]) then
    raise EZipException.CreateRes(@SZipNoWrite);
  CheckCompressionSupported(Compression);

  // Setup Header
  FillChar(LHeader, sizeof(LHeader), 0);
  {$IFDEF MSWINDOWS}
  LHeader.MadeByVersion := Word(MADEBY_MSDOS shl 8);
  {$ENDIF}
  {$IFDEF POSIX}
  LHeader.MadeByVersion := Word(MADEBY_UNIX shl 8);
  {$ENDIF}
  LHeader.CompressionMethod := UInt16(Compression);
  LHeader.ModifiedTime := Now;
  LHeader.FileAttributes := AExternalAttributes;

  LHeader.UTF8Support := FUTF8Support;
  LHeader.FileName := GetTextEncode(LHeader).GetBytes(ArchiveFileName);
  LHeader.FileNameLength := Length(LHeader.FileName);

  Add(Data, LHeader);
end;

function TZipFile.IndexOf(const FileName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to Length(FFiles) - 1 do
    if SameText(GetTextEncode(FFiles[I]).GetString(FFiles[I].FileName), FileName) then
      Exit(I);
end;

function TZipFile.GetFileIndex(const FileName: string): Integer;
begin
  Result := IndexOf(FileName);
  if Result < 0 then
    raise EZipFileNotFoundException.Create(FileName);
end;

function TZipFile.GetHeaderFileName(const Header: TZipHeader): string;
begin
  Result := GetTextEncode(Header).GetString(Header.FileName);
end;

{ EZipFileNotFoundException }

constructor EZipFileNotFoundException.Create(const AFileName: string);
begin
  FFileName := AFileName;
  inherited CreateRes(@SSpecifiedFileNotFound);
end;

{ TZipDecryptStream }

constructor TZipDecryptStream.Create(AStream: TStream; ACRC32: Cardinal;
  const ACryptor: IZipCryptor; const APassword: string);
var
  Header: array[0..11] of Byte;
begin
  inherited Create;
  FStream := AStream;
  FStart := FStream.Position;
  FDecryptor := ACryptor;
  FDecryptor.Init(APassword, False);
  VerifyRead(Self, Header, SizeOf(Header));
  if Header[11] <> (ACRC32 shr 24) then
    raise EZipWrongPassword.CreateRes(@SZipWrongPassword);
end;

function TZipDecryptStream.GetSize: Int64;
begin
  Result := FStream.Size - FStart;
end;

function TZipDecryptStream.Read(var Buffer; Count: Longint): Longint;
var
  P: PByte;
  LBuffer: TBytes;
  LLen: Longint;
begin
  if Count <= 0 then
    Exit(0);
  Result := 0;
  P := @Buffer;
  repeat
    SetLength(LBuffer, Count);
    LLen := FStream.Read(LBuffer[0], Count);
    if LLen <= 0 then
      Break;
    SetLength(LBuffer, LLen);
    FDecryptor.Decrypt(LBuffer);
    LLen := Length(LBuffer);
    if LLen > Count then
      raise EZipException.CreateRes(@SZipErrorRead);
    Move(LBuffer[0], P^, LLen);
    Inc(P, LLen);
    Dec(Count, LLen);
    Inc(Result, LLen);
  until Count = 0;
end;

function TZipDecryptStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (Origin = soCurrent) and (Offset = 0) then
    Result := FStream.Position - FStart
  else
    raise EStreamError.CreateResFmt(@sSeekNotImplemented, [Classname]);
end;

{ TZipCryptStream }

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

{ TCRCStream }

constructor TCRCStream.Create(AStream: TStream; const AHeader: TZipHeader);
begin
  inherited Create;
  FStream := AStream;
  FExpectedCRC := AHeader.CRC32;
  FEncrypted := AHeader.IsEncrypted;
  FSize := AHeader.UncompressedSize64;
  FCRC := crc32(0, nil, 0);
end;

destructor TCRCStream.Destroy;
begin
  FStream.Free;
  inherited;
end;

function TCRCStream.GetSize: Int64;
begin
  Result := FSize;
end;

function TCRCStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
begin
  if (Origin = soBeginning) and (Offset = 0) then
    Result := 0 // required to Get Current Position
  else
    raise EStreamError.CreateResFmt(@sSeekNotImplemented, [Classname]);
end;

function TCRCStream.Read(var Buffer; Count: Longint): Longint;
begin
  if Count > FSize then
    Count := FSize;
  Result := 0;

  if Count > 0 then
    try
      Result := FStream.Read(Buffer, Count);
      if Result > 0 then
      begin
        FCRC := crc32(FCRC, @Buffer, Result);
        Dec(FSize, Result);
      end;
    except
      on EZDecompressionError do
        if FEncrypted then
          Exception.RaiseOuterException(EZipWrongPassword.CreateRes(@SZipWrongPassword))
        else
          raise;
    end;

  if (FSize = 0) and (FCRC <> FExpectedCRC) then
    if FEncrypted then
      raise EZipWrongPassword.CreateRes(@SZipWrongPassword)
    else
      raise EZipCRCException.CreateRes(@SZipErrorRead);
end;

end.
