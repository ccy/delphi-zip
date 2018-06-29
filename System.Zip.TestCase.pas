unit System.Zip.TestCase;

interface

uses System.Classes, System.SysUtils, TestFramework, System.Zip2;

type
  TTestCase_Zip = class(TTestCase)
  private
    class function ReadFile (const APath : string) : TBytes; static;
    class procedure ReadBuffer (AStream       : TStream;
                                var   Buffer  : TBytes;
                                Offset, Count : Int64); static;
    function CreateFile(const AExtension: string): string;
  protected
    function GetCompression: TZipCompression; virtual; abstract;
  published
    procedure TestCase_Zip_File;
{$IFDEF WIN64}
    procedure TestCase_BigZip_File;
{$ENDIF}
    procedure TestCase_MultipleFile;
  end;

  TTestCase_Zip_zcStored = class(TTestCase_Zip)
  protected
    function GetCompression: TZipCompression; override;
  end;

  TTestCase_Zip_zcDeflate = class(TTestCase_Zip)
  protected
    function GetCompression: TZipCompression; override;
  end;

  TTestCase_Zip_zcLZMA = class(TTestCase_Zip)
  protected
    function GetCompression: TZipCompression; override;
  end;

implementation

uses System.IOUtils, System.RTLConsts, System.Types, System.Zip.LZMA;

function TTestCase_Zip.CreateFile(const AExtension: string): string;
  var
    LCheminTemp : string;
BEGIN
  LCheminTemp := TPath.GetTempFileName;
  try
    Result := TPath.ChangeExtension(LCheminTemp, AExtension);
  finally
    TFile.Delete(LCheminTemp);
  end;
end;

class procedure TTestCase_Zip.ReadBuffer(AStream       : TStream;
                                         var   Buffer  : TBytes;
                                         Offset, Count : Int64);
  var
    LTotalCount,
    LReadCount : NativeInt;
begin
  { Perform a read directly. Most of the time this will succeed
    without the need to go into the WHILE loop. }
  LTotalCount := AStream.Read64(Buffer, Offset, Count);
  { Check if there was an error }
  if LTotalCount < 0 then
    raise EReadError.CreateRes(@SReadError);

  while (LTotalCount < Count) DO
  begin
    { Try to read a contiguous block of <Count> size }
    LReadCount := AStream.Read64(Buffer, Offset + LTotalCount, (Count - LTotalCount));
    { Check if we read something and decrease the number of bytes left to read }
    if LReadCount <= 0 then
      raise EReadError.CreateRes(@SReadError)
    else
      Inc(LTotalCount, LReadCount);
  end;
end;

class function TTestCase_Zip.ReadFile(const APath: string): TBytes;
  var
    LFileStream : TFileStream;
begin
  LFileStream := NIL;
  try
    LFileStream := TFileStream.Create(APath, fmOpenRead or fmShareDenyWrite);
    SetLength(Result, LFileStream.Size);

    ReadBuffer(LFileStream, Result, 0, Length(Result));
  finally
    LFileStream.Free;
  end;
end;

{$IFDEF WIN64}
procedure TTestCase_Zip.TestCase_BigZip_File;
  var
    LCheminZip,
    LCheminFichier : string;
    LZipFile       : TZipFile;
    LOutput        : TBytes;
    LFileStream    : TFileStream;
    LLongueurVoulue,
    LLongueur      : Int64;
BEGIN
  LCheminZip := CreateFile ('.zip');
  LCheminFichier := CreateFile ('.bin');

  LFileStream := NIL;
  try
    LFileStream := TFile.Create (LCheminFichier);
    LLongueurVoulue := $100000000;
    LFileStream.Size := LLongueurVoulue;
  finally
    LFileStream.Free;
  end;

  // Compress File
  LZipFile := TZipFile.Create;
  LZipFile.Open(LCheminZip, zmWrite);
  try
    try
      LZipFile.Add(LCheminFichier, '', GetCompression);
    finally
      LZipFile.Close;
      LZipFile.Free;
      TFile.Delete(LCheminFichier);
    end;

    // Decompress File
    TZipFile.ExtractZipFile(LCheminZip, TPath.GetTempPath);
  finally
    TFile.Delete(LCheminZip);
  end;

  CheckTrue(TFile.Exists(LCheminFichier));

  try
    LOutput := ReadFile(LCheminFichier);
    LLongueur := Length(LOutput);
    CheckEquals(LLongueurVoulue, LLongueur);
  finally
    TFile.Delete(LCheminFichier);
  end;
end;
{$ENDIF}

procedure TTestCase_Zip.TestCase_MultipleFile;
  const
    C_NbFichier : Integer = 65536;
  var
    LCheminZip,
    LCheminTemp,
    LNomFichier,
    LCheminFichier  : string;
    LI              : Integer;
    LZipFile        : TZipFile;
    LInput, LOutput : TBytes;
    LFiles          : TStringDynArray;
BEGIN
  LCheminZip := CreateFile ('.zip');
  LCheminFichier := CreateFile ('.bin');

  Randomize;
  SetLength(LInput, 10);
  for LI := Low(LInput) to High(LInput) do
    LInput[LI] := Random(128);

  TFile.WriteAllBytes(LCheminFichier, LInput);

  // Compress File
  LZipFile := TZipFile.Create;
  LZipFile.Open(LCheminZip, zmWrite);
  try
    try
      LNomFichier := TPath.GetFileName(LCheminFichier);
      for LI := 1 to C_NbFichier do
        LZipFile.Add (LCheminFichier,
                      string.Format('%0:s.%1:d', [LNomFichier, LI]),
                      GetCompression);
    finally
      LZipFile.Close;
      LZipFile.Free;
      TFile.Delete(LCheminFichier);
    end;

    // Decompress File
    LCheminTemp := LCheminFichier.Substring(0, length (LCheminFichier) - 4);
    TDirectory.CreateDirectory(LCheminTemp);

    try
      TZipFile.ExtractZipFile(LCheminZip, LCheminTemp);

      LFiles := TDirectory.GetFiles(LCheminTemp);
      CheckEquals(C_NbFichier, Length (LFiles));

      for LI:= 0 to C_NbFichier - 1 do
      begin
        LOutput := TFile.ReadAllBytes(LFiles[LI]);
        CheckEquals(Length(LInput), Length(LOutput));
        CheckTrue(CompareMem(LInput, LOutput, Length(LInput)));
      end;
    finally
      TDirectory.Delete(LCheminTemp, True);
    end;
  finally
    TFile.Delete(LCheminZip);
  end;
end;

procedure TTestCase_Zip.TestCase_Zip_File;
var fZip, fData: string;
    i: integer;
    Z: TZipFile;
    B, C: TBytes;
begin
  fZip := TPath.ChangeExtension(TPath.GetTempFileName, '.zip');
  fData := TPath.ChangeExtension(TPath.GetTempFileName, '.bin');

  Randomize;
  SetLength(B, 10 * 1024 * 1024);
  for i := Low(B) to High(B) do
    B[i] := Random(128);

  TFile.WriteAllBytes(fData, B);

  // Compress File
  Z := TZipFile.Create;
  Z.Open(fZip, zmWrite);
  try
    Z.Add(fData, '', GetCompression);
  finally
    Z.Close;
    Z.Free;
    TFile.Delete(fData);
  end;

  // Decompress File
  TZipFile.ExtractZipFile(fZip, TPath.GetTempPath);
  TFile.Delete(fZip);

  CheckTrue(TFile.Exists(fData));

  C := TFile.ReadAllBytes(fData);
  CheckEquals(Length(B), Length(C));
  CheckTrue(CompareMem(B, C, Length(B)));

  TFile.Delete(fData);
end;

function TTestCase_Zip_zcStored.GetCompression: TZipCompression;
begin
  Result := zcStored;
end;

function TTestCase_Zip_zcDeflate.GetCompression: TZipCompression;
begin
  Result := zcDeflate;
end;

function TTestCase_Zip_zcLZMA.GetCompression: TZipCompression;
begin
  Result := zcLZMA;
end;

initialization
  RegisterTests([
    TTestCase_Zip_zcStored.Suite
  , TTestCase_Zip_zcDeflate.Suite
  , TTestCase_Zip_zcLZMA.Suite
  ]);
end.
