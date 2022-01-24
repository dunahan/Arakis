(*------------------------------------------------------------------------------
Class_2daFile

2DA files (also called Meta files by Leto) are the data indices of NeverWinter
Nights, an exposed method for extrapolating meaningful information from the
binary storage in a GFF.

The T2daFile object only provides access to one file. Considering 2DAs are
usually used in conjunction, the T2daHash object is more useful for generic
access.

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

unit Class_2daFile;

interface

uses
  SysUtils, Classes, StrUtils,
  Header_Leto;

type

  T2daHeader		= packed record
    Sig			: array[0..3] of Char;
    Ver			: array[0..3] of Char;
    Clearance		: String;
    Columns		: String;
  end;

  T2daMatchEvent	= procedure(
    Sender		: TObject;
    Row			: Integer;
    Col			: String;
    ICol		: Integer;
    Value		: String;
    var Matches		: Boolean
  ) of object;

  T2daFile		= class

  private

    FLoaded		: Boolean;

    function GetCol(const Col: String): Integer;

  public

    FileName		: String;

    Header		: T2daHeader;

    Cols		: TStringList;
    Rows		: array of array of String;

    property Loaded: Boolean read FLoaded;

    constructor Create(const AFileName: String = '');
    destructor Destroy; override;

    function LoadFromFile(AFileName: String): TLetoError;
    function LoadFromStream(const Stream: TStream): TLetoError;

    function SaveToFile(const AFileName: String = ''): TLetoError;
    function SaveToStream(const Stream: TStream): TLetoError;

    procedure Clear;

    function Locate(
      const Col		: String;
      const Value	: String;
      const OnMatch	: T2daMatchEvent = nil
    ): Integer;

    function LocateAll(
      const Col		: String;
      const Value	: String;
      const OnMatch	: T2daMatchEvent = nil
    ): TStringList;

    function Lookup(
      const Row		: Integer;
      const Col		: String;
      var Field		: String
    ): Boolean;

  end;

  T2daHash		= class

  private

    FDir		: String;
    FCaching		: Boolean;
    FError		: TLetoError;
    FHash		: TStringList;

    procedure SetCaching(const Value: Boolean);
    procedure SetDir(const Value: String);

  public

    property Directory: String read FDir write SetDir;
    property Caching: Boolean read FCaching write SetCaching;

    property LastError: TLetoError read FError;

    property Hash: TStringList read FHash;

    constructor Create;
    destructor Destroy; override;

    function Get2da(const Name: String; var Meta: T2daFile): TLetoError;

    procedure BuildCache;

    procedure Clear;

    function Locate(
      const A		: String;
      const Col		: String;
      const Value	: String;
      const OnMatch	: T2daMatchEvent = nil
    ): Integer;

    function LocateAll(
      const A		: String;
      const Col		: String;
      const Value	: String;
      const OnMatch	: T2daMatchEvent = nil
    ): TStringList;

    function Lookup(
      const A		: String;
      const Row		: Integer;
      const Col		: String;
      var Field		: String
    ): Boolean;

  end;

implementation


{ T2daFile }


(*------------------------------------------------------------------------------
GetCol

------------------------------------------------------------------------------*)
function T2daFile.GetCol(const Col: String): Integer;
begin
  if (Col = '') or (Col = '.') then
    Result		:= 0
  else if not TryStrToInt(Col, Result) then
    Result		:= Cols.IndexOf(Col);
  if Result > Cols.Count-1 then
    Result		:= -1;
end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor T2daFile.Create(const AFileName: String);
begin
  Cols			:= TStringList.Create;

  if AFileName = '' then
    FLoaded		:= True
  else
    LoadFromFile(AFileName);
end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor T2daFile.Destroy;
begin
  Clear;
  FreeAndNil(Cols);

  inherited;
end;

(*------------------------------------------------------------------------------
LoadFromFile

------------------------------------------------------------------------------*)
function T2daFile.LoadFromFile(AFileName: String): TLetoError;
var
  Stream		: TFileStream;
begin
  Clear;

  if Lowercase(ExtractFileExt(AFileName)) <> '.2da' then
    AFileName		:= AFileName + '.2da';

  if not FileExists(AFileName) then begin
    Result		:= Err_Missing_File;
    Exit;
  end;

  FileName		:= AFileName;

  try
    Stream		:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  except
    Result		:= Err_Locked_File;
    Exit;
  end;

  Result		:= LoadFromStream(Stream);

  FreeAndNil(Stream);

end;

(*------------------------------------------------------------------------------
LoadFromStream

------------------------------------------------------------------------------*)
function T2daFile.LoadFromStream(const Stream: TStream): TLetoError;
var
  C			: Char;
  T			: set of Char;
  Flag			: Byte;
  S			: String;
  I			: Integer;
  Done			: Boolean;
  function _GetCh_: Char;
  begin
    C := #0;
    if Stream.Read(C, 1) <> 1 then
      raise Exception.Create('');
    Result := C;
  end;
  function _GetS_: String;
  begin
    S			:= '';
    _GetCh_;
    if C = '"' then
      T			:= ['"']
    else begin
      T			:= [' ', #13, #10];
      S			:= S + C;
    end;
    while not(_GetCh_ in T) do
      S			:= S + C;
    if C = #13 then
      Stream.Seek(-1, soFromCurrent);
    Result		:= S;
  end;
  procedure _Shift_;
  begin
    while (_GetCh_ = ' ') do ;
    Stream.Seek(-1, soFromCurrent);
  end;
begin
  FLoaded		:= False;
  Result		:= Err_Not_2da;
  
  Done			:= False;

  // TODO 3: CHANGE: Use line-at-a-time method in 2DA state machine (like Lexer)

  try

  if Stream.Size < 10 then Exit;

  Stream.Seek(0, 0);

  { Header }

  Stream.Read(Header.Sig, 4);
  Stream.Read(Header.Ver, 4);

  if Lowercase(Header.Sig + Header.Ver) <> '2da v2.0' then Exit;

  { Clearance }

  Header.Clearance	:= '';
  Flag			:= 0;
  while Flag < 2 do begin
    while (_GetCh_ = ' ') do
      Header.Clearance	:= Header.Clearance + C;
    if C = #13 then begin
      if (_GetCh_ <> #10) then Exit;
      Inc(Flag);
      Header.Clearance	:= Header.Clearance + #13#10;
    end else
      Exit;
  end;

  { Columns }

  Header.Columns	:= '';
  while (_GetCh_ <> #13) do
    Header.Columns	:= Header.Columns + C;
  if (_GetCh_ <> #10) or (Header.Columns = '') then Exit;
  Header.Columns	:= Header.Columns + #13#10;
  Cols.CommaText	:= Header.Columns;
  if Cols.Count < 1 then Exit;

  { Rows }

  while Stream.Position < Stream.Size do begin
    { Newline terminates indexes }
    if _GetCh_ = #13 then
      Break
    else
      Stream.Seek(-1, soFromCurrent);

    { Index }
    S			:= _GetS_;
    if not TryStrToInt(S, I) or (I <> Length(Rows)) then Exit;
    _Shift_;

    { Columns }
    SetLength(Rows, Length(Rows)+1);
    SetLength(Rows[High(Rows)], Cols.Count);
    for I := 0 to Cols.Count-1 do begin
      try
        S := _GetS_;
        Rows[High(Rows)][I] := S;
        _Shift_;
      except
        { Valid to end abruptly after last column }
        Done		:= (I = Cols.Count-1) and (S <> '');
        if not Done then Exit;
      end;
    end;

    { End of row }
    if Done or (Stream.Position + 1 = Stream.Size) then
      Break
    else if (_GetCh_ <> #13) or (_GetCh_ <> #10) then
      Exit;

  end;

  Result		:= Success;

  except
    on E:Exception do
      Exit;
  end;

end;

(*------------------------------------------------------------------------------
SaveToFile

------------------------------------------------------------------------------*)
function T2daFile.SaveToFile(const AFileName: String): TLetoError;
var
  Stream		: TFileStream;
begin

  if AFileName <> '' then
    FileName		:= AFileName;

  Result		:= Err_No_Filename;
  if FileName = '' then Exit;

  try
    Stream		:= TFileStream.Create(FileName, fmCreate);
    try
      Result		:= SaveToStream(Stream);
    finally
      FreeAndNil(Stream);
    end;
  except
    on EFCreateError do begin
      Result		:= Err_Locked_File;
    end;
  end;

end;

(*------------------------------------------------------------------------------
SaveToStream

------------------------------------------------------------------------------*)
function T2daFile.SaveToStream(const Stream: TStream): TLetoError;
begin
  Result		:= Err_NYI;
end;

(*------------------------------------------------------------------------------
Clear

------------------------------------------------------------------------------*)
procedure T2daFile.Clear;
var
  I			: Integer;
begin
  for I := 0 to High(Rows) do
    SetLength(Rows[I], 0);
  SetLength(Rows, 0);
  Cols.Clear;

  Header.Clearance	:= '';
  Header.Columns	:= '';

end;

(*------------------------------------------------------------------------------
Locate

------------------------------------------------------------------------------*)
function T2daFile.Locate(
  const Col		: String;
  const Value		: String;
  const OnMatch		: T2daMatchEvent
): Integer;
var
  R, C			: Integer;
  S			: String;
  Matches		: Boolean;
begin
  Result		:= -1;
  C			:= GetCol(Col);
  for R := 0 to High(Rows) do begin
    S			:= Rows[R][C];
    Matches		:= False;
    if Assigned(OnMatch) then
      OnMatch(self, R, Col, C, S, Matches)
    else
      Matches :=
        (
          (S = '****') and ((Value = '') or (Value = '*') or (Value = '****'))
        ) or (
          (S <> '****') and StringMatch(Value, S)
        );
    if Matches then begin
      Result		:= R;
      Break;
    end;
  end;
end;

(*------------------------------------------------------------------------------
LocateAll

------------------------------------------------------------------------------*)
function T2daFile.LocateAll(
  const Col		: String;
  const Value		: String;
  const OnMatch		: T2daMatchEvent
): TStringList;
var
  R, C			: Integer;
  S			: String;
  Matches		: Boolean;
begin
  Result		:= TStringList.Create;
  C			:= GetCol(Col);
  // DONE 1: BUG: [4.0.5] Exception on invalid 2DA col
  if C = -1 then Exit;
  for R := 0 to High(Rows) do begin
    S			:= Rows[R][C];
    Matches		:= False;
    if Assigned(OnMatch) then
      OnMatch(self, R, Col, C, S, Matches)
    else
      Matches :=
        (
          (S = '****') and ((Value = '') or (Value = '*') or (Value = '****'))
        ) or (
          (S <> '****') and StringMatch(Value, S)
        );
    if Matches then
      Result.Add(IntToStr(R));
  end;
end;

(*------------------------------------------------------------------------------
Lookup

------------------------------------------------------------------------------*)
function T2daFile.Lookup(
  const Row		: Integer;
  const Col		: String;
  var Field		: String
): Boolean;
var
  C			: Integer;
begin
  Result		:= False;

  C			:= GetCol(Col);

  if (C < 0) or (Row < 0) or (Row > High(Rows)) then
    Exit
  else
    Field		:= Rows[Row][C];

  if Field = '****' then
    Field		:= '';

  Result		:= True;
    
end;


{ T2daHash }


(*------------------------------------------------------------------------------
property Caching

------------------------------------------------------------------------------*)
procedure T2daHash.SetCaching(const Value: Boolean);
begin
  FCaching		:= Value;

end;

(*------------------------------------------------------------------------------
property Directory

------------------------------------------------------------------------------*)
procedure T2daHash.SetDir(const Value: String);
begin
  FDir			:= Value;
  if FDir = '' then
    Exit
  else if not(RightStr(FDir, 1)[1] in ['/', '\'])  then
    FDir		:= FDir + PathDelim;
end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor T2daHash.Create;
begin
  Caching		:= True;
  FHash			:= TStringList.Create;
  FError		:= Success;
end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor T2daHash.Destroy;
begin
  Clear;
  FreeAndNil(FHash);

  inherited;
end;

(*------------------------------------------------------------------------------
Get2da

Try to load and parse this 2DA. Name can be either a filename or just the
name of the 2DA - e.g., "feats". For an unqualified name, the 2DA must
already be cached.

------------------------------------------------------------------------------*)
function T2daHash.Get2da(
  const Name		: String;
  var Meta		: T2daFile
): TLetoError;
var
  I			: Integer;
begin
  Result		:= Success;

  try

  { Is it cached? }
  I			:= Hash.IndexOf(Name);
  if I > -1 then begin
    Meta		:= T2daFile(Hash.Objects[I]);
    Exit;
  end;

  { Try and load it. }
  Meta			:= T2daFile.Create;
  Result		:= Meta.LoadFromFile(Directory + Name);
  if Result <> Success then begin
    FreeAndNil(Meta);
    Exit;
  end else if Caching then
    Hash.AddObject(Name, Meta);

  finally
    FError		:= Result;
  end;

end;

(*------------------------------------------------------------------------------
BuildCache

Load and cache every single 2DA in the directory.

------------------------------------------------------------------------------*)
procedure T2daHash.BuildCache;
begin

end;

(*------------------------------------------------------------------------------
Clear

------------------------------------------------------------------------------*)
procedure T2daHash.Clear;
var
  I			: Integer;
begin
  for I := 0 to Hash.Count-1 do
    T2daFile(Hash.Objects[I]).Free;
  Hash.Clear;
end;

(*------------------------------------------------------------------------------
Locate

------------------------------------------------------------------------------*)
function T2daHash.Locate(
  const A		: String;
  const Col		: String;
  const Value		: String;
  const OnMatch		: T2daMatchEvent
): Integer;
var
  M			: T2daFile;
begin
  Result		:= -1;
  if Get2da(A, M) <> Success then Exit;

  Result		:= M.Locate(Col, Value, OnMatch);

end;

(*------------------------------------------------------------------------------
LocateAll

------------------------------------------------------------------------------*)
function T2daHash.LocateAll(
  const A		: String;
  const Col		: String;
  const Value		: String;
  const OnMatch		: T2daMatchEvent
): TStringList;
var
  M			: T2daFile;
begin
  Result		:= nil;
  if Get2da(A, M) <> Success then Exit;
  Result		:= M.LocateAll(Col, Value, OnMatch);
end;

(*------------------------------------------------------------------------------
Lookup

------------------------------------------------------------------------------*)
function T2daHash.Lookup(
  const A		: String;
  const Row		: Integer;
  const Col		: String;
  var Field		: String
): Boolean;
var
  M			: T2daFile;
begin
  Result := (Get2da(A, M) = Success) and M.Lookup(Row, Col, Field);
end;

(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
end.
