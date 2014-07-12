unit LzFind;

interface

uses Winapi.Windows, System.SysUtils, LzmaTypes;

{$Z4}
type
  PCLzRef = ^TCLzRef;
  TCLzRef = UInt32;

  PCMatchFinder = ^TCMatchFinder;

  TCMatchFinder = record
    buffer: PByte;
    pos: UInt32;
    posLimit: UInt32;
    streamPos: UInt32;
    lenLimit: UInt32;

    cyclicBufferPos: UInt32;
    cyclicBufferSize: UInt32;  (* it must be = (historySize + 1) *)

    matchMaxLen: UInt32;
    hash: PCLzRef;
    son: PCLzRef;
    hashMask: UInt32;
    cutValue: UInt32;

    bufferBase: PByte;
    stream: PISeqInStream;
    streamEndWasReached: Integer;

    blockSize: UInt32;
    keepSizeBefore: UInt32;
    keepSizeAfter: UInt32;

    numHashBytes: UInt32;
    directInput: Integer;
    directInputRem: SIZE_T;
    btMode: Integer;
    bigHash: Integer;
    historySize: UInt32;
    fixedHashSize: UInt32;
    hashSizeSum: UInt32;
    numSons: UInt32;
    result: TSRes;
    crc: array[0..255] of UInt32;
  end;

  //typedef void (*Mf_Init_Func)(void *object);
  Mf_Init_Func = procedure (aobject: pointer); cdecl;

  //typedef Byte (*Mf_GetIndexByte_Func)(void *object, Int32 index);
  Mf_GetIndexByte_Func = function(aobject: pointer): UInt32; cdecl;

  //typedef UInt32 (*Mf_GetNumAvailableBytes_Func)(void *object);
  Mf_GetNumAvailableBytes_Func = function(aobject: pointer): UInt32; cdecl;

  //typedef const Byte * (*Mf_GetPointerToCurrentPos_Func)(void *object);
  Mf_GetPointerToCurrentPos_Func = function(aobject: pointer): PByte; cdecl;

  //typedef UInt32 (*Mf_GetMatches_Func)(void *object, UInt32 *distances);
  Mf_GetMatches_Func = function(aobject: Pointer; var distances: UInt32): UInt32; cdecl;

  //typedef void (*Mf_Skip_Func)(void *object, UInt32);
  Mf_Skip_Func = procedure(aobject: Pointer; a: UInt32); cdecl;

  TIMatchFinder = record
    Init: Mf_Init_Func;
    GetIndexByte: Mf_GetIndexByte_Func;
    GetNumAvailableBytes: Mf_GetNumAvailableBytes_Func;
    GetPointerToCurrentPos: Mf_GetPointerToCurrentPos_Func;
    GetMatches: Mf_GetMatches_Func;
    Skip: Mf_Skip_Func;
  end;

function _MatchFinder_NeedMove(var p: TCMatchFinder): Integer; cdecl; external;

function _MatchFinder_GetPointerToCurrentPos(var p: TCMatchFinder): TBytes; cdecl; external;

procedure _MatchFinder_MoveBlock(var p: TCMatchFinder); cdecl; external;

procedure _MatchFinder_ReadIfRequired(var p: TCMatchFinder); cdecl; external;

procedure _MatchFinder_ReduceOffsets(var p: TCMatchFinder; subValue: UInt32); cdecl; external;

procedure _MatchFinder_Normalize3(subValue: UInt32; items: PCLzRef; numItems: UInt32); cdecl; external;

function _MatchFinder_Create(var p: TCMatchFinder; historySize: UInt32;
    keepAddBufferBefore: UInt32; matchMaxLen: UInt32; keepAddBufferAfter: UInt32;
    var alloc: TISzAlloc): Integer; cdecl; external;

procedure _MatchFinder_Init(var p: TCMatchFinder); cdecl; external;

function _GetMatchesSpec1(lenLimit: UInt32; curMatch: UInt32; pos: UInt32; const buffer: TBytes; son: PCLzRef;
    _cyclicBufferPos: UInt32; _cyclicBufferSize: UInt32; _cutValue: UInt32;
    var distances: UInt32; maxLen: UInt32): TArray<UInt32>; cdecl; external;

procedure _MatchFinder_Construct(var p: TCMatchFinder); cdecl; external;

procedure _MatchFinder_Free(var p: TCMatchFinder; var alloc: TISzAlloc); cdecl; external;

procedure _MatchFinder_CreateVTable(var p: TCMatchFinder; var vTable: TIMatchFinder); cdecl; external;

implementation

uses System.Win.Crtl;

{$L Win32\LzFind.obj}

end.
