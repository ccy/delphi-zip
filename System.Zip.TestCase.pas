unit System.Zip.TestCase;

interface

uses TestFramework, System.Zip2;

type
  TTestCase_Zip = class(TTestCase)
  protected
    function GetCompression: TZipCompression; virtual; abstract;
  published
    procedure TestCase_Zip_File;
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

uses Classes, System.SysUtils, System.IOUtils, System.Zip.LZMA;

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
