(*------------------------------------------------------------------------------
Class_XbaseFile

The classes necessary for Xbase support, TDbfFile and TFptFile, which are the
files NWN uses for StoreCampaignObject() and RetrieveCampaignObject(), a
unique means of transporting GFF data out of and into a *live* game, by
means of an intermediary (database) file.

Support for these file types has been added to TLetoFile.

The entire Xbase spec is not supported - really just enough of it for
LetoScript to be able to work with FPT files. Index files aren't even
supported at all, just DBF and FPT. The emphasis on FoxPro is because FPT
files are FoxPro files.

The specification was based on the notes at:

http://clicketyclick.dk/docs/index.html

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

unit Class_XbaseFile;
// TODO 4: BUG: Verify all Integer, ReadInt

interface

uses
  SysUtils, Classes, Math, DateUtils,
  Header_Leto, Class_GffFile;

type

  // Note that not nearly the whole DBF spec is supported
  TDbfHeader		= record
    Version		: Byte;
    LastUpdate		: array[1..3] of Byte;
    RecordCount		: Cardinal;
    HeaderLen		: Word;
    RecordLen		: Word;
    IncompleteTrans	: Boolean;
    Encryption		: Boolean;
    MDXFlag		: Byte;
    Language		: Byte;
  end;

  // Nothing after Length is used by NWN
  TDbfField		= record
    Name		: String;
    FieldType		: Char;
    Address		: Cardinal;
    Length		: Byte;
    Decimal		: Byte;
  end;
  PDbfField		= ^TDbfField;

  TDbfRecord		= record
    Deleted		: Boolean;
    Data		: String;
  end;
  PDbfRecord		= ^TDbfRecord;

  TDbfFile		= class

  private

    FHeader		: TDbfHeader;
    FFileName		: String;

    FFields		: array of PDbfField;
    FRecords		: array of PDbfRecord;

    function Pad(
      const S		: String;
      const Len		: Integer;
      const PadLeft	: Boolean = False
    ): String;

    function GetFieldCount: Cardinal;
    function GetRecCount: Cardinal;

    function GetField(Name: String): PDbfField;
    function GetRecord(const Index: Integer): PDbfRecord;

  public

    property Header: TDbfHeader read FHeader;
    property FileName: String read FFileName write FFileName;

    property FieldCount: Cardinal read GetFieldCount;
    property RecordCount: Cardinal read GetRecCount;

    property Field[Name: String]: PDbfField read GetField;
    property Rec[const Index: Integer]: PDbfRecord read GetRecord;

    constructor Create(const AFileName: String = '');
    destructor Destroy; override;

    function LoadFromFile(AFileName: String): TLetoError;
    function LoadFromStream(const Stream: TStream): TLetoError;

    function SaveToFile(AFileName: String = ''): TLetoError;
    function SaveToStream(const Stream: TStream): TLetoError;

    procedure Clear;

    function IndexOf(VarName: String): Integer;

    function ValueOf(
      const ARec	: PDbfRecord;
      const AField	: PDbfField
    ): String; overload;
    function ValueOf(
      const ARec	: PDbfRecord;
      const FieldName	: String
    ): String; overload;
    function ValueOf(
      const RecIndex	: Integer;
      const FieldName	: String
    ): String; overload;

    function AddRec(const Data: String = ''): PDbfRecord;

    function AddNwnRec(
      const VarName	: String;
      const PlayerId	: String;
      const TimeStamp	: String;
      const VarType	: Char;
      const Int		: Integer;
      const Dbl1	: Double;
      const Dbl2	: Double;
      const Dbl3	: Double;
      const Dbl4	: Double;
      const Dbl5	: Double;
      const Dbl6	: Double;
      const Memo	: Cardinal
    ): PDbfRecord;

    function AddSimpleNwnRec(
      const VarName	: String;
      const PlayerId	: String = '';
      const Memo	: Cardinal = 1
    ): PDbfRecord;

    procedure SetRec(ARec: PDbfRecord; const Data: String);

    procedure SetNwnRec(
      ARec		: PDbfRecord;
      const VarName	: String;
      const PlayerId	: String;
      const TimeStamp	: String;
      const VarType	: Char;
      const Int		: Integer;
      const Dbl1	: Double;
      const Dbl2	: Double;
      const Dbl3	: Double;
      const Dbl4	: Double;
      const Dbl5	: Double;
      const Dbl6	: Double;
      const Memo	: Cardinal
    );

    procedure SetSimpleNwnRec(
      ARec		: PDbfRecord;
      const VarName	: String;
      const PlayerId	: String = '';
      const Memo	: Cardinal = 1
    );

  end;

  TFptObj		= record
    Block		: Cardinal;
    Stream		: TStream;
  end;
  PFptObj		= ^TFptObj;

  TFptFile		= class

  private

    FDbf		: TDbfFile;
    FNextRec		: Cardinal;
    FRecSize		: Cardinal;

    FList		: TList;

    function GetFileName: String;
    function GetGffCount: Cardinal;

  public

    property Dbf: TDbfFile read FDbf write FDbf;
    property NextRec: Cardinal read FNextRec write FNextRec;
    property RecSize: Cardinal read FRecSize write FRecSize;

    property FileName: String read GetFileName;

    property GffCount: Cardinal read GetGffCount;

    constructor Create(const ADbf: TDbfFile = nil);
    destructor Destroy; override;

    function LoadFromFile(AFileName: String = ''): TLetoError;
    function LoadFromStream(const Stream: TStream): TLetoError;

    function SaveToFile(AFileName: String = ''): TLetoError;
    function SaveToStream(const Stream: TStream): TLetoError;

    function BlockToIndex(const Block: Cardinal): Integer;

    procedure RecalcBlocks;

    {
    function ExtractGff(const AtIndex: Integer): TGffFile; overload;
    function ExtractGff(const ObjName: String): TGffFile; overload;
    }
    function ExtractStream(const ObjName: String): TStream;

    function InsertGff(const Gff: TGffFile): Cardinal;
    function InsertStream(const Stream: TStream): Cardinal;

    function InsertNamed(
      const Gff		: TGffFile;
      const ObjName	: String
    ): Cardinal; overload;
    function InsertNamed(
      const Stream	: TStream;
      const ObjName	: String
    ): Cardinal; overload;

    function ReplaceNamed(
      const Stream	: TStream;
      const ObjName	: String
    ): Cardinal;

    procedure Clear;

    // Implemented in TFptFile specifically for LetoScript <fpt>
    // No longer used... phase out

    function GetListOfObjNames(const Mask: String = '*.*'): TStringList;

    procedure Delete(const ObjName: String);

  end;


implementation


{ TDbfFile }


(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TDbfFile.Create(const AFileName: String);
begin

  if AFileName <> '' then
    LoadFromFile(AFileName);

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TDbfFile.Destroy;
begin
  Clear;

  inherited;
end;

(*------------------------------------------------------------------------------
Pad

------------------------------------------------------------------------------*)
function TDbfFile.Pad(
  const S		: String;
  const Len		: Integer;
  const PadLeft		: Boolean
): String;
var
  L			: Integer;
begin
  L			:= Length(S);
  if L = Len then
    Result		:= S
  else if L > Len then
    Result		:= Copy(S, 1, Len)
  else if PadLeft then
    Result		:= StringOfChar(' ', Len-L) + S
  else
    Result		:= S + StringOfChar(' ', Len-L);
end;

(*------------------------------------------------------------------------------
property FieldCount

------------------------------------------------------------------------------*)
function TDbfFile.GetFieldCount: Cardinal;
begin
  Result := Length(FFields);
end;

(*------------------------------------------------------------------------------
property RecordCount

------------------------------------------------------------------------------*)
function TDbfFile.GetRecCount: Cardinal;
begin
  Result := Length(FRecords);
end;

(*------------------------------------------------------------------------------
property Rec

------------------------------------------------------------------------------*)
function TDbfFile.GetRecord(const Index: Integer): PDbfRecord;
begin
  if (Index < Low(FRecords)) or (Index > High(FRecords)) then
    Result		:= nil
  else
    Result		:= FRecords[Index];
end;

(*------------------------------------------------------------------------------
property Field

Implemented with case-insensitive name (NOT part of the Xbase spec!) purely
as a LetoScript convenience.

------------------------------------------------------------------------------*)
function TDbfFile.GetField(Name: String): PDbfField;
var
  I			: Integer;
begin
  Name			:= Lowercase(Name);
  for I := Low(FFields) to High(FFields) do
    if Lowercase(FFields[I].Name) = Name then begin
      Result		:= FFields[I];
      Exit;
    end;
  Result		:= nil;
end;

(*------------------------------------------------------------------------------
LoadFromFile

------------------------------------------------------------------------------*)
function TDbfFile.LoadFromFile(AFileName: String): TLetoError;
var
  Stream		: TFileStream;
begin

  // DONE 1: BUG: [4.0.4] Linux case-sensitivity
  if not FileExists(AFileName) then
    AFileName		:= ChangeFileExt(AFileName, '.DBF');

  if not FileExists(AFileName) then begin
    Result		:= Err_Missing_File;
    Exit;
  end;

  Clear;

  FFileName		:= AFileName;

  try
    Stream		:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
    Result		:= LoadFromStream(Stream);
  except
    Result		:= Err_Locked_File;
  end;

  FreeAndNil(Stream);

end;

(*------------------------------------------------------------------------------
LoadFromStream

------------------------------------------------------------------------------*)
function TDbfFile.LoadFromStream(const Stream: TStream): TLetoError;
var
  I			: Integer;
begin
  Result		:= Success;

  if not Assigned(Stream) then
    Result		:= Err_Internal
  else if Stream.Size < 32 then
    Result		:= Err_Bad_Header;

  if Result <> Success then Exit;

  Stream.Seek(0, 0);

  // Header
  //
  with Header do begin
    Version		:= ReadInt(Stream, 1);
    for I := 1 to 3 do
      LastUpdate[I]	:= ReadInt(Stream, 1);
    RecordCount		:= ReadInt(Stream);
    HeaderLen		:= ReadInt(Stream, 2);
    RecordLen		:= ReadInt(Stream, 2);

    Stream.Seek(2, soFromCurrent);

    IncompleteTrans	:= Boolean(ReadInt(Stream, 1));
    Encryption		:= Boolean(ReadInt(Stream, 1));

    Stream.Seek(12, soFromCurrent);

    MDXFlag		:= ReadInt(Stream, 1);
    Language		:= ReadInt(Stream, 1);
  end; // with Header

  Stream.Seek(2, soFromCurrent);

  if
    Stream.Size <
    Header.HeaderLen + (Header.RecordCount * Header.RecordLen) + 1
  then begin
    Result		:= Err_Bad_Header;
    Exit;
  end;

  SetLength(FFields, (Header.HeaderLen - 33) div 32);
  SetLength(FRecords, Header.RecordCount);

  // Fields
  //
  for I := 0 to FieldCount-1 do begin
    New(FFields[I]);
    FFields[I].Name		:= ReadString(Stream, 11);
    FFields[I].FieldType	:= ReadString(Stream, 1)[1];
    FFields[I].Address		:= ReadInt(Stream, 4);
    FFields[I].Length		:= ReadInt(Stream, 1);
    FFields[I].Decimal		:= ReadInt(Stream, 1);

    Stream.Seek(14, soFromCurrent);
  end;

  // Terminator
  if Byte(ReadInt(Stream, 1)) <> $0D then begin
    Result		:= Err_Bad_Header;
    Exit;
  end;

  // Records
  //
  for I := 0 to Header.RecordCount-1 do begin
    New(FRecords[I]);
    FRecords[I].Deleted		:= ReadInt(Stream, 1) = $2A;
    FRecords[I].Data		:= ReadString(Stream, Header.RecordLen-1);
  end;

  // End of file
  if Byte(ReadInt(Stream, 1)) <> $1A then
    Result		:= Err_Bad_Header;

end;

(*------------------------------------------------------------------------------
SaveToFile

------------------------------------------------------------------------------*)
function TDbfFile.SaveToFile(AFileName: String): TLetoError;
var
  Stream		: TFileStream;
begin
  if AFileName = '' then
    AFileName		:= FileName;

  Result		:= Err_No_Filename;
  if AFileName = '' then Exit;

  try
    Stream		:= TFileStream.Create(AFileName, fmOpenWrite or fmShareDenyNone);
    try
      Result		:= SaveToStream(Stream);
    finally
      FreeAndNil(Stream);
    end;
  except
    Result		:= Err_Locked_File;
  end;

end;

(*------------------------------------------------------------------------------
SaveToStream

------------------------------------------------------------------------------*)
function TDbfFile.SaveToStream(const Stream: TStream): TLetoError;
var
  I			: Integer;
  C			: Cardinal;
  S			: String;

  F			: PDbfField;
  R			: PDbfRecord;
begin
  Result		:= Err_Internal;
  if not Assigned(Stream) then Exit;

  Stream.Size		:= Header.HeaderLen + (RecordCount * Header.RecordLen) + 1;

  // Zero-out the stream
  C			:= 0;
  for I := 1 to Stream.Size div 4 do
    Stream.Write(C, 4);

  Stream.Seek(0, 0);

  // Header
  //
  Stream.Write(Header.Version, 1);

  C			:= CurrentYear - 2000;
  Stream.Write(C, 1);
  C			:= MonthOf(Date);
  Stream.Write(C, 1);
  C			:= DayOf(Date);
  Stream.Write(C, 1);

  C			:= RecordCount;
  Stream.Write(C, 4);

  Stream.Write(Header.HeaderLen, 2);
  Stream.Write(Header.RecordLen, 2);

  Stream.Seek(2, soFromCurrent);

  C			:= Cardinal(Header.IncompleteTrans);
  Stream.Write(C, 1);
  C			:= Cardinal(Header.Encryption);
  Stream.Write(C, 1);

  Stream.Seek(12, soFromCurrent);

  Stream.Write(Header.MDXFlag, 1);
  Stream.Write(Header.Language, 1);

  Stream.Seek(2, soFromCurrent);

  // Fields
  //
  for I := Low(FFields) to High(FFields) do begin

    F			:= FFields[I];

    S			:= F.Name;
    if Length(S) > 11 then
      S			:= Copy(S, 1, 11);
    Stream.Write(PChar(S)^, Length(S));
    Stream.Seek(11-Length(S), soFromCurrent);
    Stream.Write(PChar(String(F.FieldType))^, 1);

    C			:= F.Address;
    Stream.Write(C, 4);
    C			:= F.Length;
    Stream.Write(C, 1);
    C			:= F.Decimal;
    Stream.Write(C, 1);

    Stream.Seek(14, soFromCurrent);

  end;

  // Terminator
  C			:= $0D;
  Stream.Write(C, 1);

  // Records
  //
  for I := Low(FRecords) to High(FRecords) do begin

    R			:= Rec[I];

    if R.Deleted then
      C			:= $2A
    else
      C			:= $20;
    Stream.Write(C, 1);

    Stream.Write(PChar(R.Data)^, Length(R.Data));

  end;

  // End of file
  C			:= $1A;
  Stream.Write(C, 1);

  Result		:= Success;

end;

(*------------------------------------------------------------------------------
Clear

------------------------------------------------------------------------------*)
procedure TDbfFile.Clear;
var
  I			: Integer;
begin
  FillChar(FHeader, SizeOf(FHeader), 0);

  for I := Low(FFields) to High(FFields) do
    Dispose(FFields[I]);

  for I := Low(FRecords) to High(FRecords) do
    Dispose(FRecords[I]);

  SetLength(FFields, 0);
  SetLength(FRecords, 0);

end;

(*------------------------------------------------------------------------------
IndexOf

A very NWN-specific function. The fields NWN puts in the DBF are constant,
and VARNAME is the first of them, to identify the "name" used in
StoreCampaignObject.

This function will find the index of the record with a VARNAME equal to the
value specified - case-insensitive (as a convenience). Using this Index,
it's then possible to use RecField to isolate the value of the MEMO field:

RecField[ IndexOfVarName('THE_PC'), 'MEMO' ]

Convert that to an Int, and you can pass it right along to TFptFile.ExtractGff,
which is exactly what TFptFile.ExtractGff(ObjName) does.

------------------------------------------------------------------------------*)
function TDbfFile.IndexOf(VarName: String): Integer;
var
  I			: Integer;
  F			: PDbfField;
  R			: PDbfRecord;
  V			: String;
begin
  Result		:= -1;

  F			:= Field['varname'];
  if not Assigned(F) then Exit;

  VarName		:= Lowercase(VarName);

  for I := Low(FRecords) to High(FRecords) do begin
    R			:= FRecords[I];
    V			:= Lowercase( Trim(ValueOf(R, F)) );
    if V = VarName then begin
      Result		:= I;
      Exit;
    end;
  end;

end;

(*------------------------------------------------------------------------------
ValueOf

For the given Record (index), return the Data of the field named.

------------------------------------------------------------------------------*)
function TDbfFile.ValueOf(
  const ARec		: PDbfRecord;
  const AField		: PDbfField
): String;
begin
  if Assigned(ARec) and Assigned(AField) then
    Result := Copy(ARec.Data, AField.Address, AField.Length);
end;

function TDbfFile.ValueOf(
  const ARec		: PDbfRecord;
  const FieldName	: String
): String;
begin
  Result := ValueOf( ARec, Field[FieldName] );
end;

function TDbfFile.ValueOf(
  const RecIndex	: Integer;
  const FieldName	: String
): String;
begin
  Result := ValueOf( Rec[RecIndex], Field[FieldName] );
end;

(*------------------------------------------------------------------------------
AddRec

------------------------------------------------------------------------------*)
function TDbfFile.AddRec(const Data: String): PDbfRecord;
begin
  New(Result);
  Result.Deleted := False;

  SetLength(FRecords, Length(FRecords)+1);
  FRecords[High(FRecords)] := Result;
  
  SetRec(Result, Data);
end;

(*------------------------------------------------------------------------------
AddNwnRec

------------------------------------------------------------------------------*)
function TDbfFile.AddNwnRec(
  const VarName, PlayerId,
    TimeStamp		: String;
  const VarType		: Char;
  const Int		: Integer;
  const Dbl1, Dbl2, Dbl3,
    Dbl4, Dbl5, Dbl6	: Double;
  const Memo		: Cardinal
): PDbfRecord;
begin
  Result		:= AddRec;
  SetNwnRec(
    Result,
    VarName, PlayerId, TimeStamp, VarType,
    Int, Dbl1, Dbl2, Dbl3, Dbl4, Dbl5, Dbl6, Memo
  );
end;

(*------------------------------------------------------------------------------
AddSimpleNwnRec

------------------------------------------------------------------------------*)
function TDbfFile.AddSimpleNwnRec(
  const VarName,
    PlayerId		: String;
  const Memo		: Cardinal
): PDbfRecord;
begin
  Result		:= AddRec;
  SetSimpleNwnRec(Result, VarName, PlayerId, Memo);
end;

(*------------------------------------------------------------------------------
SetRec

------------------------------------------------------------------------------*)
procedure TDbfFile.SetRec(ARec: PDbfRecord; const Data: String);
begin
  ARec.Data := Pad(Data, 221);
end;

(*------------------------------------------------------------------------------
SetNwnRec

------------------------------------------------------------------------------*)
procedure TDbfFile.SetNwnRec(
  ARec			: PDbfRecord;
  const VarName, PlayerId,
    TimeStamp		: String;
  const VarType		: Char;
  const Int		: Integer;
  const Dbl1, Dbl2, Dbl3,
    Dbl4, Dbl5, Dbl6	: Double;
  const Memo		: Cardinal
);
{$IFDEF MSWINDOWS}
var
  FS			: TFormatSettings;
begin

  FillChar(FS, SizeOf(FS), 0);

  ARec.Data :=
    Pad(VarName, 32) +
    Pad(PlayerId, 32) +
    Pad(TimeStamp, 16) +
    VarType +
    Pad( IntToStr(Int), 10 ) +
    Pad( FloatToStr(Dbl1, FS), 20 ) +
    Pad( FloatToStr(Dbl2, FS), 20 ) +
    Pad( FloatToStr(Dbl3, FS), 20 ) +
    Pad( FloatToStr(Dbl4, FS), 20 ) +
    Pad( FloatToStr(Dbl5, FS), 20 ) +
    Pad( FloatToStr(Dbl6, FS), 20 ) +
    Pad( IntToStr(Memo), 10, True );

end;
{$ENDIF}

{$IFDEF LINUX}
begin
  ARec.Data :=
    Pad(VarName, 32) +
    Pad(PlayerId, 32) +
    Pad(TimeStamp, 16) +
    VarType +
    Pad( IntToStr(Int), 10 ) +
    Pad( FloatToStr(Dbl1), 20 ) +
    Pad( FloatToStr(Dbl2), 20 ) +
    Pad( FloatToStr(Dbl3), 20 ) +
    Pad( FloatToStr(Dbl4), 20 ) +
    Pad( FloatToStr(Dbl5), 20 ) +
    Pad( FloatToStr(Dbl6), 20 ) +
    Pad( IntToStr(Memo), 10, True );
end;
{$ENDIF}

(*------------------------------------------------------------------------------
SetSimpleNwnRec

------------------------------------------------------------------------------*)
procedure TDbfFile.SetSimpleNwnRec(
  ARec			: PDbfRecord;
  const VarName,
    PlayerId		: String;
  const Memo		: Cardinal
);
var
{$IFDEF MSWINDOWS}
  FS			: TFormatSettings;
{$ENDIF}
  TimeStamp		: String;
begin

  // 03/09/0400:32:23
  {$IFDEF MSWINDOWS}
  FillChar(FS, SizeOf(FS), 0);
  FS.ShortDateFormat	:= 'MM/DD/YY';
  FS.DateSeparator	:= '/';
  FS.LongTimeFormat	:= 'HH:MM:SS';
  FS.TimeSeparator	:= ':';
  TimeStamp		:= DateToStr(Now, FS) + TimeToStr(Now, FS);
  {$ENDIF}
  {$IFDEF LINUX}
  ShortDateFormat	:= 'MM/DD/YY';
  LongTimeFormat	:= 'HH:MM:SS';
  TimeStamp             := DateToStr(Now) + TimeToStr(Now);
  {$ENDIF}

  ARec.Data :=
    Pad(VarName, 32) +
    Pad(PlayerId, 32) +
    TimeStamp +
    'O' +
    StringOfChar(' ', 130) +
    Pad( IntToStr(Memo), 10, True );

end;


{ TFptFile }


(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TFptFile.Create(const ADbf: TDbfFile);
begin
  Dbf			:= ADbf;

  FList			:= TList.Create;

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TFptFile.Destroy;
begin
  Clear;

  FreeAndNil(FList);

  inherited;
end;

(*------------------------------------------------------------------------------
property FileName

------------------------------------------------------------------------------*)
function TFptFile.GetFileName: String;
begin
  if Assigned(Dbf) then
    Result		:= Dbf.FileName
  else
    Result		:= '';
end;

(*------------------------------------------------------------------------------
property GffCount

------------------------------------------------------------------------------*)
function TFptFile.GetGffCount: Cardinal;
begin
  Result		:= FList.Count;
end;

(*------------------------------------------------------------------------------
LoadFromFile

------------------------------------------------------------------------------*)
function TFptFile.LoadFromFile(AFileName: String): TLetoError;
var
  Stream		: TFileStream;
begin
  if (AFileName = '') and Assigned(Dbf) then
    AFileName		:= ChangeFileExt(Dbf.FileName, '.FPT');
  if not FileExists(AFileName) then
    AFileName		:= ChangeFileExt(Dbf.FileName, '.fpt');

  if not FileExists(AFileName) then begin
    Result		:= Err_Missing_File;
    Exit;
  end;

  Clear;

  try
    Stream		:= TFileStream.Create(AFileName, fmOpenRead or fmShareDenyNone);
    Result		:= LoadFromStream(Stream);
  except
    Result		:= Err_Locked_File;
  end;

  FreeAndNil(Stream);

end;

(*------------------------------------------------------------------------------
LoadFromStream

------------------------------------------------------------------------------*)
function TFptFile.LoadFromStream(const Stream: TStream): TLetoError;
var
  GffStream, GffObj	: TMemoryStream;
  LastRec		: Cardinal;
  Building		: Boolean;
  ObjType		: Cardinal;
  Length		: Cardinal;
begin
  Result		:= Success;

  if not Assigned(Stream) then
    Result		:= Err_Internal
  else if Stream.Size < 512 then
    Result		:= Err_Bad_Header;

  if Result <> Success then Exit;

  Stream.Seek(0, 0);

  // Header
  //

  // NextRec is built internally
  LastRec		:= Swap32(ReadInt(Stream));
  Dec(LastRec);

  Stream.Seek(2, soFromCurrent);

  RecSize		:= Swap16(ReadInt(Stream, 2));

  if Stream.Size < RecSize + 8 then Exit;

  Stream.Seek(RecSize - 8, soFromCurrent);

  GffStream		:= TMemoryStream.Create;
  try

  GffStream.CopyFrom(Stream, Stream.Size-512);
  GffStream.Seek(0, 0);

  if GffStream.Size < RecSize then Exit;

  Building		:= True;

  while Building and (Result = Success) do begin

    ObjType		:= Swap32(ReadInt(GffStream));
    Length		:= Swap32(ReadInt(GffStream));

    if GffStream.Size < (GffStream.Position + Length) then begin
      Result		:= Err_Bad_Header;
      break;
    end;

    if ObjType = $01 then begin
      GffObj		:= TMemoryStream.Create;
      GffObj.CopyFrom(GffStream, Length);
      InsertStream(GffObj);
    end;

    if GffStream.Position = GffStream.Size then
      Building		:= False
    else
      GffStream.Seek(RecSize - ((Length+8) mod RecSize), soFromCurrent);

    Building := Building and (GffStream.Position < LastRec * RecSize);

  end;

  finally
    FreeAndNil(GffStream);
  end;

end;

(*------------------------------------------------------------------------------
SaveToFile

------------------------------------------------------------------------------*)
function TFptFile.SaveToFile(AFileName: String): TLetoError;
var
  Stream		: TFileStream;
begin
  if (AFileName = '') and Assigned(Dbf) then
    AFileName		:= Dbf.FileName;

  Result		:= Err_No_Filename;
  if AFileName = '' then Exit;

  try
    Stream		:= TFileStream.Create(AFileName, fmOpenWrite or fmShareDenyNone);
    try
      Result		:= SaveToStream(Stream);
    finally
      FreeAndNil(Stream);
    end;
  except
    Result		:= Err_Locked_File;
  end;

end;

(*------------------------------------------------------------------------------
SaveToStream

------------------------------------------------------------------------------*)
function TFptFile.SaveToStream(const Stream: TStream): TLetoError;
var
  I			: Integer;
  C			: Cardinal;
  W			: Word;
  Obj			: TStream;
begin
  Result		:= Err_Internal;
  if not Assigned(Stream) then Exit;

  Stream.Size		:= NextRec * 512;

  // Zero-out the stream
  C			:= 0;
  for I := 1 to Stream.Size div 4 do
    Stream.Write(C, 4);

  Stream.Seek(0, 0);

  // Next available block
  C			:= Swap32(NextRec);
  Stream.Write(C, 4);

  // Reserved
  Stream.Seek(2, soFromCurrent);

  // RecSize
  W			:= Swap16(RecSize);
  Stream.Write(W, 2);

  // Reserved
  Stream.Seek(504, soFromCurrent);

  for I := 0 to FList.Count-1 do begin

    Obj			:= PFptObj(FList[I]).Stream;

    // Type (01, Memo)
    C			:= Swap32($01);
    Stream.Write(C, 4);

    // Size
    C			:= Swap32(Obj.Size);
    Stream.Write(C, 4);

    // Data
    Obj.Seek(0, 0);
    Stream.CopyFrom(Obj, Obj.Size);

    // Filler
    C			:= RecSize - ((Obj.Size + 8) mod RecSize);
    Stream.Seek(C, soFromCurrent);

  end;

  // Remove the filler for the last block (just in case)
  Stream.Size := Stream.Size - C;

  Result		:= Success;

end;

(*------------------------------------------------------------------------------
BlockToIndex

------------------------------------------------------------------------------*)
function TFptFile.BlockToIndex(const Block: Cardinal): Integer;
var
  I			: Integer;
begin
  for I := 0 to FList.Count-1 do
    if PFptObj(FList[I]).Block = Block then begin
      Result		:= I;
      Exit;
    end;
  Result		:= -1;
end;

(*------------------------------------------------------------------------------
RecalcBlocks

Recalculate the block position of every object, and the NextRec.

------------------------------------------------------------------------------*)
procedure TFptFile.RecalcBlocks;
var
  I			: Integer;
  Obj			: PFptObj;
begin

  NextRec		:= 1;

  for I := 0 to FList.Count-1 do begin
    Obj			:= PFptObj(FList[I]);
    Obj.Block		:= NextRec;
    Inc(FNextRec, Ceil( (Obj.Stream.Size+8) / RecSize ));
  end;

end;

(*------------------------------------------------------------------------------
ExtractGff

AtIndex has been made "convenient" for LetoScript. It refers to the ordinality
of the Gff structure as a whole in the FPT, so 0 is the first Gff, 1 is the
next, etc. This has absolutely no correspondence with how the FPT counts
fields (512 byte blocks) or the "index" given in the DBF (which is based on
the same 512-byte blocks).

The AtIndex form is basically useful for when LetoScript is asked for a Gff
by index, while the ObjName format is useful for when LetoScript is asked
for a Gff by VARNAME. Neither are useful at all in a generic Xbase app -
but then, neither are Gff files.

It is up to the caller to free the Result.

------------------------------------------------------------------------------
function TFptFile.ExtractGff(const AtIndex: Integer): TGffFile;
var
  Stream		: TMemoryStream;
begin
  Result		:= nil;

  if (AtIndex < 0) or (AtIndex > FList.Count-1) then
    Exit;

  Stream		:= PFptObj(FList[AtIndex]).Stream;

  if Stream.Size = 0 then Exit;

  Result		:= TGffFile.Create;
  Result.LoadFromStream(Stream);

end;

function TFptFile.ExtractGff(const ObjName: String): TGffFile;
var
  Addr			: String;
  Block, Index		: Integer;
begin
  Result		:= nil;

  if not Assigned(Dbf) then Exit;

  Addr := Trim( Dbf.ValueOf(Dbf.IndexOf(ObjName), 'MEMO') );

  if not TryStrToInt(Addr, Block) then Exit;

  Index			:= BlockToIndex(Block);
  if Index > -1 then
    Result		:= ExtractGff(Index);

end;
*)

(*------------------------------------------------------------------------------
ExtractStream

------------------------------------------------------------------------------*)
function TFptFile.ExtractStream(const ObjName: String): TStream;
var
  Addr			: String;
  Block, Index		: Integer;
begin
  Result		:= nil;

  if not Assigned(Dbf) then Exit;

  Addr := Trim( Dbf.ValueOf(Dbf.IndexOf(ObjName), 'MEMO') );

  if not TryStrToInt(Addr, Block) then Exit;

  Index			:= BlockToIndex(Block);
  if Index > -1 then begin
    Result		:= PFptObj(FList[Index]).Stream;
    Result.Seek(0, 0);
  end;

end;

(*------------------------------------------------------------------------------
InsertGff

------------------------------------------------------------------------------*)
function TFptFile.InsertGff(const Gff: TGffFile): Cardinal;
var
  Stream		: TMemoryStream;
begin
  Result		:= 0;

  if not Assigned(Gff) then Exit;

  Stream		:= TMemoryStream.Create;
  Gff.SaveToStream(Stream);

  Result		:= InsertStream(Stream);

end;

(*------------------------------------------------------------------------------
InsertStream

------------------------------------------------------------------------------*)
function TFptFile.InsertStream(const Stream: TStream): Cardinal;
var
  Obj			: PFptObj;
begin
  Result		:= 0;

  if not Assigned(Stream) then Exit;

  New(Obj);
  Obj.Block		:= NextRec;
  Obj.Stream		:= Stream;

  FList.Add(Obj);

  Result		:= NextRec;

  Inc(FNextRec, Ceil( (Stream.Size+8) / RecSize ));

end;

(*------------------------------------------------------------------------------
InsertNamed

Does the extra work of updating the MEMO field of an associated record in
the DBF, which has a VARNAME of ObjName - so that back in the game,
RetrieveCampaignObject(ObjName) will retrieve this GFF. If there is no
record found with a VARNAME of ObjName, a new one is created.

------------------------------------------------------------------------------*)
function TFptFile.InsertNamed(
  const Gff		: TGffFile;
  const ObjName		: String
): Cardinal;
var
  Stream		: TMemoryStream;
begin
  Result		:= 0;

  if not Assigned(Gff) then Exit;

  Stream		:= TMemoryStream.Create;
  Gff.SaveToStream(Stream);

  Result		:= InsertNamed(Stream, ObjName);

  if Result = 0 then
    FreeAndNil(Stream);

end;

function TFptFile.InsertNamed(
  const Stream		: TStream;
  const ObjName		: String
): Cardinal;
var
  I			: Integer;
begin
  Result		:= 0;

  if not Assigned(Dbf) then Exit;

  Result		:= InsertStream(Stream);

  I			:= Dbf.IndexOf(ObjName);
  if I = -1 then
    Dbf.AddSimpleNwnRec(ObjName, '', Result)
  else
    Dbf.SetSimpleNwnRec(Dbf.Rec[I], ObjName, '', Result);

end;

(*------------------------------------------------------------------------------
ReplaceNamed

------------------------------------------------------------------------------*)
function TFptFile.ReplaceNamed(
  const Stream		: TStream;
  const ObjName		: String
): Cardinal;
var
  I			: Integer;
  R			: PDbfRecord;
  Addr, PlayerId	: String;
  Obj			: PFptObj;
begin

  Result		:= 0;

  if not Assigned(Dbf) then Exit;

  I			:= Dbf.IndexOf(ObjName);
  if I = -1 then Exit;

  R			:= Dbf.Rec[I];
  PlayerId		:= Dbf.ValueOf(R, 'PlayerId');

  Addr			:= Dbf.ValueOf(R, 'MEMO');
  if not TryStrToInt(Addr, I) then Exit;
  I			:= BlockToIndex(I);
  if I = -1 then Exit;

  Obj			:= PFptObj(FList[I]);
  Obj.Stream.Free;
  Obj.Stream		:= Stream;

  RecalcBlocks;

  Result		:= Obj.Block;

  Dbf.SetSimpleNwnRec(R, ObjName, PlayerId, Result);

end;

(*------------------------------------------------------------------------------
Clear

------------------------------------------------------------------------------*)
procedure TFptFile.Clear;
var
  I			: Integer;
  Obj			: PFptObj;
begin
  for I := 0 to FList.Count-1 do begin
    Obj			:= PFptObj(FList[I]);
    Obj.Stream.Free;
    Dispose(Obj);
  end;

  FList.Clear;

  NextRec		:= 1;

end;

(*------------------------------------------------------------------------------
GetListOfObjNames

------------------------------------------------------------------------------*)
function TFptFile.GetListOfObjNames(const Mask: String): TStringList;
var
  I			: Integer;
begin
  Result		:= TStringList.Create;

  if not Assigned(Dbf) or (Mask = '') then Exit;

  for I := 0 to Dbf.RecordCount-1 do
    if not Dbf.Rec[I].Deleted then
      Result.Add( Trim(Dbf.ValueOf(I, 'varname')) );

end;

(*------------------------------------------------------------------------------
Delete

Marks the record for deletion. The corresponding memo is cleared.

------------------------------------------------------------------------------*)
procedure TFptFile.Delete(const ObjName: String);
var
  I			: Integer;
  R			: PDbfRecord;
begin
  if not Assigned(Dbf) then Exit;

  I			:= Dbf.IndexOf(ObjName);

  if I = -1 then Exit;

  R			:= Dbf.Rec[I];

  R.Deleted		:= True;

  I := BlockToIndex( StrToIntDef(Trim( Dbf.ValueOf(R, 'MEMO') ), 0) );

  if I > -1 then
    PFptObj(FList[I]).Stream.Size := 0;

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
