unit LzFindMt;

interface

uses System.SysUtils, Threads, LzmaTypes, LzFind;

const
  kMtCacheLineDummy = 128;

{$Z4}
type
  TCMtSync = record
    wasCreated: Boolean;
    needStart: Boolean;
    exit: Boolean;
    stopWriting: Boolean;

    thread: TCThread;
    canStart: TCAutoResetEvent;
    wasStarted: TCAutoResetEvent;
    wasStopped: TCAutoResetEvent;
    freeSemaphore: TCSemaphore;
    filledSemaphore: TCSemaphore;
    csWasInitialized: Boolean;
    csWasEntered: Boolean;
    cs: TCCriticalSection;
    numProcessedBlocks: UInt32;
  end;

  TMf_Mix_Matches = function(p: Pointer; matchMinPos: UInt32; var distances: UInt32): PCardinal; cdecl;

  TMf_GetHeads = procedure(const buffer: TBytes; pos: UInt32; hash: PCardinal; hashMask: UInt32;
    heads: PCardinal; numHeads: UInt32; const crc: PCardinal); cdecl;

  TCMatchFinderMt = record
    (* LZ *)
    pointerToCurPos: PByte;
    btBuf: PCardinal;
    btBufPos: UInt32;
    btBufPosLimit: UInt32;
    lzPos: UInt32;
    btNumAvailBytes: UInt32;

    UInt32: PCardinal;
    fixedHashSize: UInt32;
    historySize: UInt32;
    crc: PCardinal;

    MixMatchesFunc: TMf_Mix_Matches;

    (* LZ + BT *)
    btSync: TCMtSync;
    btDummy: array[0..kMtCacheLineDummy - 1] of Byte;

    (* BT *)
    hashBuf: PCardinal;
    hashBufPos: UInt32;
    hashBufPosLimit: UInt32;
    hashNumAvail: UInt32;

    son: PCLzRef;
    matchMaxLen: UInt32;
    numHashBytes: UInt32;
    pos: UInt32;
    buffer: PByte;
    cyclicBufferPos: UInt32;
    cyclicBufferSize: UInt32; (* it must be historySize + 1 *)
    cutValue: UInt32;

    (* BT + Hash *)
    hashSync: TCMtSync;
    (* Byte hashDummy[kMtCacheLineDummy]; *)

    (* Hash *)
    GetHeadsFunc: TMf_GetHeads;
    MatchFinder: PCMatchFinder;
  end;

procedure {$ifdef UNDERSCOREIMPORTNAME}_MatchFinderMt_Construct{$else}MatchFinderMt_Construct{$endif}(var p: TCMatchFinderMt); cdecl; external;

procedure {$ifdef UNDERSCOREIMPORTNAME}_MatchFinderMt_Destruct{$else}MatchFinderMt_Destruct{$endif}(var p: TCMatchFinderMt; var alloc:
    TISzAlloc); cdecl; external;

function {$ifdef UNDERSCOREIMPORTNAME}_MatchFinderMt_Create{$else}MatchFinderMt_Create{$endif}(var p: TCMatchFinderMt; historySize: UInt32;
    keepAddBufferBefore: UInt32; matchMaxLen: UInt32; keepAddBufferAfter:
    UInt32; var alloc: TISzAlloc): TSRes; cdecl; external;

procedure {$ifdef UNDERSCOREIMPORTNAME}_MatchFinderMt_CreateVTable{$else}MatchFinderMt_CreateVTable{$endif}(var p: TCMatchFinderMt; var vTable:
    TIMatchFinder); cdecl; external;

procedure {$ifdef UNDERSCOREIMPORTNAME}_MatchFinderMt_ReleaseStream{$else}MatchFinderMt_ReleaseStream{$endif}(var p: TCMatchFinderMt); cdecl; external;

implementation

uses System.Win.Crtl, Winapi.Windows;

{$ifdef Win32}
  {$L Win32\LzFindMt.obj}
{$else}
  {$L Win64\LzFindMt.o}
{$endif}

end.
