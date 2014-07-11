program ZipTest;

{$APPTYPE CONSOLE}

uses
  TextTestRunner,
  System.Zip.LZMA in 'System.Zip.LZMA.pas',
  System.Zip.TestCase in 'System.Zip.TestCase.pas';

begin
  ReportMemoryLeaksOnShutdown := True;
  if IsConsole then
    RunRegisteredTests.Free;
  Readln;
end.
