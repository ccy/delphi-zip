unit Threads;

interface

uses System.Win.Crtl, Winapi.Windows, LzmaTypes, System.Classes;

{$Z4}
type
  TCEvent = THandle;

  TCSemaphore = THandle;

  TCCriticalSection = TRTLCriticalSection;

  TCAutoResetEvent = TCEvent;

  TCThread = THandle;

  TThread_Func_Type = Pointer;

function __beginthreadex(__security_attr: Pointer; __stksize: Cardinal;
    __start: TThread_Func_Type; __arg: Pointer; __create_flags: Cardinal; var
    __thread_id: Cardinal): Cardinal; cdecl; external msvcrt name '_beginthreadex';

function _Event_Reset(var p: TCEvent): TWRes; cdecl; external;

function _Event_Set(var p: TCEvent): TWRes; cdecl; external;

function _Handle_WaitObject(h: THandle): TWRes; cdecl; external;

function _Semaphore_Release1(var p: TCSemaphore): TWRes; cdecl; external;

function _HandlePtr_Close(var h: THandle): TWRes; cdecl; external;

function _CriticalSection_Init(var p: TCCriticalSection): TWRes; cdecl; external;

function _AutoResetEvent_CreateNotSignaled(var p: TCAutoResetEvent): TWRes; cdecl; external;

function _Semaphore_Create(var p: TCSemaphore; initCount: UInt32; maxCount: UInt32): TWRes; cdecl; external;

function _Thread_Create(var p: TCThread; func: TThread_Func_Type; param: LPVOID): TWRes; cdecl; external;

implementation

{$L Win32\Threads.obj}

end.
