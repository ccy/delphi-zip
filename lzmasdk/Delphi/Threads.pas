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

function {$ifdef UNDERSCOREIMPORTNAME}__beginthreadex{$else}_beginthreadex{$endif}(__security_attr: Pointer; __stksize: Cardinal;
    __start: TThread_Func_Type; __arg: Pointer; __create_flags: Cardinal; var
    __thread_id: Cardinal): Cardinal; cdecl; external msvcrt name '_beginthreadex';

function {$ifdef UNDERSCOREIMPORTNAME}_Event_Reset{$else}Event_Reset{$endif}(var p: TCEvent): TWRes; cdecl; external;

function {$ifdef UNDERSCOREIMPORTNAME}_Event_Set{$else}Event_Set{$endif}(var p: TCEvent): TWRes; cdecl; external;

function {$ifdef UNDERSCOREIMPORTNAME}_Handle_WaitObject{$else}Handle_WaitObject{$endif}(h: THandle): TWRes; cdecl; external;

function {$ifdef UNDERSCOREIMPORTNAME}_Semaphore_Release1{$else}Semaphore_Release1{$endif}(var p: TCSemaphore): TWRes; cdecl; external;

function {$ifdef UNDERSCOREIMPORTNAME}_HandlePtr_Close{$else}HandlePtr_Close{$endif}(var h: THandle): TWRes; cdecl; external;

function {$ifdef UNDERSCOREIMPORTNAME}_CriticalSection_Init{$else}CriticalSection_Init{$endif}(var p: TCCriticalSection): TWRes; cdecl;
    external;

function {$ifdef UNDERSCOREIMPORTNAME}_AutoResetEvent_CreateNotSignaled{$else}AutoResetEvent_CreateNotSignaled{$endif}(var p: TCAutoResetEvent): TWRes;
    cdecl; external;

function {$ifdef UNDERSCOREIMPORTNAME}_Semaphore_Create{$else}Semaphore_Create{$endif}(var p: TCSemaphore; initCount: UInt32; maxCount:
    UInt32): TWRes; cdecl; external;

function {$ifdef UNDERSCOREIMPORTNAME}_Thread_Create{$else}Thread_Create{$endif}(var p: TCThread; func: TThread_Func_Type; param:
    LPVOID): TWRes; cdecl; external;

implementation

{$ifdef Win32}
  {$L Win32\Threads.obj}
{$else}
  {$L Win64\Threads.o}
{$endif}

end.
