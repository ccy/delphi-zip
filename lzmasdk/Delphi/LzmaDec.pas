unit LzmaDec;

interface

uses Winapi.Windows, System.Win.Crtl, System.SysUtils, LzmaTypes;

const
  LZMA_REQUIRED_INPUT_MAX = 20;

{$Z4}
type
  {$define _LZMA_PROB32}

  PCLzmaProb = ^TCLzmaProb;
  {$ifdef _LZMA_PROB32}
  TCLzmaProb = UInt32;
  {$else}
  TCLzmaProb = Word;
  {$endif}

  TCLzmaProps = record
    lc, lp, pb: Cardinal;
    dicSize: UInt32;
  end;

  ELzmaFinishMode = (
    LZMA_FINISH_ANY,   { finish at any point }
    LZMA_FINISH_END    { block must be finished at the end }
  );

  ELzmaStatus = (
    LZMA_STATUS_NOT_SPECIFIED,               { use main error code instead }
    LZMA_STATUS_FINISHED_WITH_MARK,          { stream was finished with end mark. }
    LZMA_STATUS_NOT_FINISHED,                { stream was not finished }
    LZMA_STATUS_NEEDS_MORE_INPUT,            { you must provide more input bytes }
    LZMA_STATUS_MAYBE_FINISHED_WITHOUT_MARK  { there is probability that stream was finished without end mark }
  );

  TCLzmaDec = record
    prob: TCLzmaProps;
    prop: PCLzmaProb;
    dic: TBytes;
    buf: TBytes;
    range, code: UInt32;
    dicPos: SIZE_T;
    dicBufSize: SIZE_T;
    processedPos: UInt32;
    checkDicSize: UInt32;
    state: Cardinal;
    reps: array[0..3] of UInt32;
    remainLen: Cardinal;
    needFlush: Integer;
    needInitState: Integer;
    numProbs: UInt32;
    tempBufSize: Cardinal;
    tempBuf: array[0..LZMA_REQUIRED_INPUT_MAX - 1] of Byte;
  public
    procedure Construct;
  end;

function LzmaDec_Allocate(var state: TCLzmaDec; const prop; propsSize:
    Cardinal; var alloc: TISzAlloc): TSRes; cdecl; external name _PU +
    'LzmaDec_Allocate';

procedure LzmaDec_Init(var p: TCLzmaDec); cdecl; external name _PU +
    'LzmaDec_Init';

procedure LzmaDec_Free(var state: TCLzmaDec; var alloc: TISzAlloc); cdecl;
    external name _PU + 'LzmaDec_Free';

function LzmaDec_DecodeToBuf(var p: TCLzmaDec; var dest; var destLen: SIZE_T;
    const src; var srcLen: SIZE_T; finishMode: ElzmaFinishMode; var status:
    ELzmaStatus): TSRes; cdecl; external name _PU + 'LzmaDec_DecodeToBuf';

implementation

{$ifdef Win32}
  {$L Win32\LzmaDec.obj}
{$else}
  {$L Win64\LzmaDec.o}
{$endif}

procedure TCLzmaDec.Construct;
begin
  FillChar(Self, SizeOf(Self), 0);
end;

end.
