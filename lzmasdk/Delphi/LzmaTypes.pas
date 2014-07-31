unit LzmaTypes;

interface

uses Winapi.Windows;

const
{$IFDEF UNDERSCOREIMPORTNAME}
  _PU = '_';
{$ELSE}
  _PU = '';
{$ENDIF}

  SZ_OK                  = 0;
  SZ_ERROR_DATA          = 1;
  SZ_ERROR_MEM           = 2;
  SZ_ERROR_CRC           = 3;
  SZ_ERROR_UNSUPPORTED   = 4;
  SZ_ERROR_PARAM         = 5;
  SZ_ERROR_INPUT_EOF     = 6;
  SZ_ERROR_OUTPUT_EOF    = 7;
  SZ_ERROR_READ          = 8;
  SZ_ERROR_WRITE         = 9;
  SZ_ERROR_PROGRESS      = 10;
  SZ_ERROR_FAIL          = 11;
  SZ_ERROR_THREAD        = 12;
  SZ_ERROR_ARCHIVE       = 16;
  SZ_ERROR_NO_ARCHIVE    = 17;

  LZMA_PROPS_SIZE         = 5;

{$Z4}
type
  TSRes = Integer;

  TWRes = UInt32;

  PISzAlloc = ^TISzAlloc;

  PSzAllocProc = ^TSzAllocProc;
  TSzAllocProc = function(Sender: PISzAlloc; size: SIZE_T): Pointer; cdecl;

  PSzFreeProc = ^TSzFreeProc;
  TSzFreeProc = procedure(Sender: PISzAlloc; address: Pointer); cdecl;

  TISzAlloc = record
  strict private
    SzAlloc: TSzAllocProc;
    SzFree: TSzFreeProc;
  public
    procedure Init;
  end;

  PISeqInStream = Pointer;

  TInStreamReadProc = function(p: PISeqInStream; buf: PByte; var size: SIZE_T): TSRes; cdecl;

  PISeqOutStream = Pointer;

  TOutStreamWriteProc = function(p: PISeqOutStream; const buf: Pointer; size: SIZE_T): SIZE_T; cdecl;

  PICompressProgress = Pointer;

  TCompressProgressProc = function(p: PICompressProgress; inSize: UInt64; outSize: UInt64): TSRes; cdecl;

function _wcscpy(dest, src: PWideChar): PWideChar; cdecl; external 'msvcrt.dll' name 'wcscpy';

procedure CheckLzma(const aStatus: Integer);

implementation

uses System.SysUtils;

function SzAllocProc(Sender: PISzAlloc; size: SIZE_T): Pointer; cdecl;
begin
  if size > 0 then
    Result := AllocMem(size)
  else
    Result := nil;
end;

procedure SzFreeProc(Sender: PISzAlloc; address: Pointer); cdecl;
begin
  if address <> nil then
    FreeMem(address);
end;

procedure CheckLzma(const aStatus: Integer);
begin
  if aStatus <> SZ_OK then
    raise Exception.CreateFmt('LZMA Error. Code: %d', [aStatus]);
end;

procedure TISzAlloc.Init;
begin
  SzAlloc := SzAllocProc;
  SzFree := SzFreeProc;
end;

end.
