program ZipTest;

{$APPTYPE CONSOLE}

uses
  TextTestRunner,
  Threads in 'lzmasdk\Delphi\Threads.pas',
  LzmaTypes in 'lzmasdk\Delphi\LzmaTypes.pas',
  LzFind in 'lzmasdk\Delphi\LzFind.pas',
  LzFindMt in 'lzmasdk\Delphi\LzFindMt.pas',
  LzmaDec in 'lzmasdk\Delphi\LzmaDec.pas',
  LzmaEnc in 'lzmasdk\Delphi\LzmaEnc.pas',
  System.Zip2 in 'System.Zip2.pas',
  System.Zip.LZMA in 'System.Zip.LZMA.pas',
  System.Zip.TestCase in 'System.Zip.TestCase.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  if IsConsole then
    RunRegisteredTests.Free;
  Readln;
end.
