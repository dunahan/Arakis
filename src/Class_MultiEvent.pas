(*------------------------------------------------------------------------------
Class_MultiEvent

Implements the Observer/Subject pattern, without using interfaces. Instead
of observers subscribing to an object (the Subject), they subscribe to
events, instead. Inspired by the example at:

http://www.jerometremblay.com/ow.asp?ObserverPattern

(It's a WIKI, and the front page is in French, so I'm not certain if I should
credit Jerome Tremblay personally or not. :) I've made a few changes to the
code I found posted there (2004-01-23).

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

unit Class_MultiEvent;

interface

uses
  Classes, SysUtils;

type

  TMultiEvent		= class

  private
    FObservers		: TList;

  protected

    function FindObserver(Observer: TMethod): Integer;
    function GetObserver(Index: Integer): TMethod;
    procedure SignalObserver(Observer: TMethod); virtual; abstract;

  public

    constructor Create;
    destructor Destroy; override;

    procedure Attach(Observer: TMethod);
    procedure Detach(Observer: TMethod);

    procedure Signal;

    function ObserverCount: Cardinal;
    function HasAttachments: Boolean;

  end;

  TMultiNotifyEvent	= class(TMultiEvent)

  private

    FSender		: TObject;

  protected

    procedure SignalObserver(Observer: TMethod); override;

  public

    procedure Attach(Observer: TNotifyEvent);
    procedure Detach(Observer: TNotifyEvent);

    procedure Signal(Sender: TObject);

    function IsAttached(Observer: TNotifyEvent): Boolean;

  end;


implementation

{$ASSERTIONS ON}


{ TMultiEvent }


(*------------------------------------------------------------------------------
FindObserver

------------------------------------------------------------------------------*)
function TMultiEvent.FindObserver(Observer: TMethod): integer;
var
  I:			Integer;
begin
  { Search fails by default, if there is a match, result will be updated. }
  Result		:= -1;
  for I := (FObservers.Count div 2)-1 downto 0 do
    { We have a match only if both the Code and Data pointers are the same. }
    if
      (Observer.Code = FObservers[i * 2 ]) and
      (Observer.Data = FObservers[i * 2 + 1])
    then begin
      Result		:= I;
      break;
    end;
end;

(*------------------------------------------------------------------------------
GetObserver

------------------------------------------------------------------------------*)
function TMultiEvent.GetObserver(Index: integer): TMethod;
begin
  { Fill the TMethod pointer with its code and data pointer. }
  Result.Code		:= FObservers[Index * 2];
  Result.Data		:= FObservers[Index * 2 + 1];
end;

(*------------------------------------------------------------------------------
Create

------------------------------------------------------------------------------*)
constructor TMultiEvent.Create;
begin
  inherited;

  FObservers		:= TList.Create;

end;

(*------------------------------------------------------------------------------
Destroy

------------------------------------------------------------------------------*)
destructor TMultiEvent.Destroy;
begin
  { This assertion is facultative, but it ensures the subject is 'clean'
    when destroyed. }
  Assert(FObservers.Count = 0, 'Not all observers were detached.');

  FreeAndNil(FObservers);

  inherited;
end;

(*------------------------------------------------------------------------------
Attach

------------------------------------------------------------------------------*)
procedure TMultiEvent.Attach(Observer: TMethod);
var
  Index			: Integer;
begin
  Index			:= FindObserver(Observer);

  Assert(Index < 0, 'This observer was already attached to this event.');

  { A method contains two pointers:
    - The code pointer (where the procedure is in memory)
    - The data pointer (what instance of the object calls the procedure)
  }
  if Index < 0 then begin
    FObservers.Add(Observer.Code);
    FObservers.Add(Observer.Data);
  end;

end;

(*------------------------------------------------------------------------------
Detach

------------------------------------------------------------------------------*)
procedure TMultiEvent.Detach(Observer: TMethod);
var
  Index			: Integer;
begin
  Index			:= FindObserver(Observer) * 2;

  { Again, the assertion is facultative, nothing would be broken
    if we just ignored it. }
  // TODO 4: CHANGE: Make these exceptions rather than assertions
  Assert(Index >= 0, 'The observer was not attached to this event.');

  if Index >= 0 then begin
    FObservers.Delete(Index); // Delete code pointer
    FObservers.Delete(Index); // Delete data pointer
  end;
end;

(*------------------------------------------------------------------------------
Signal

Call SignalObserver for each stored observer in reverse order.

SignalObserver (which is declared in sub-classes) will typecast the TMethod
record into whatever procedure type it handles.

------------------------------------------------------------------------------*)
procedure TMultiEvent.Signal;
var
  I:			Integer;
begin
  for I := (FObservers.Count div 2)-1 downto 0 do
    SignalObserver(GetObserver(I));
end;

(*------------------------------------------------------------------------------
ObserverCount

How many observers are subscribed to this event?

------------------------------------------------------------------------------*)
function TMultiEvent.ObserverCount: Cardinal;
begin
  Result		:= FObservers.Count div 2;
end;

(*------------------------------------------------------------------------------
HasAttachments

Is anyone observing this event?

------------------------------------------------------------------------------*)
function TMultiEvent.HasAttachments: Boolean;
begin
  Result		:= FObservers.Count > 0;
end;


{ TMultiNotifyEvent }


(*------------------------------------------------------------------------------
SignalObserver

------------------------------------------------------------------------------*)
procedure TMultiNotifyEvent.SignalObserver(Observer: TMethod);
begin
  TNotifyEvent(Observer)(FSender);
end;

(*------------------------------------------------------------------------------
Attach

------------------------------------------------------------------------------*)
procedure TMultiNotifyEvent.Attach(Observer: TNotifyEvent);
begin
  inherited Attach(TMethod(Observer));
end;

(*------------------------------------------------------------------------------
Detach

------------------------------------------------------------------------------*)
procedure TMultiNotifyEvent.Detach(Observer: TNotifyEvent);
begin
  inherited Detach(TMethod(Observer));
end;

(*------------------------------------------------------------------------------
Signal

------------------------------------------------------------------------------*)
procedure TMultiNotifyEvent.Signal(Sender: TObject);
begin
  FSender := Sender;
  inherited Signal;
end;

(*------------------------------------------------------------------------------
IsAttached

Is this observer attached?

------------------------------------------------------------------------------*)
function TMultiNotifyEvent.IsAttached(Observer: TNotifyEvent): Boolean;
begin
  Result		:= FindObserver(TMethod(Observer)) > -1;
end;


(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
end.
