(*------------------------------------------------------------------------------
Class_ErfFile

Provides generic access to ERF files, as a wrapper over TErfStruct.

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

unit Class_ErfFile;
// TODO 4: CHANGE: Overhaul of TErfFile LoadFromStream / SaveToStream

interface

uses
  SysUtils, Classes, DateUtils,
  Header_Leto,
  Class_MultiEvent,
  Class_ErfStruct, Class_GffFile;

type

  TErfFile		= class

  private

    FFileName		: String;
    FCount		: Integer;

    FSize		: Int64;

    FStrings		: TStringList;

    FFirstStruct	: TErfStruct;
    FLastStruct		: TErfStruct;

    FOpenStructs	: TStringList;
    FOpened		: TErfStruct;

    ExportAs		: String;

    FOnDestroy		: TMultiNotifyEvent;

    function GetCount: Integer;

    function GetString(const Lang: Integer): String;
    function GetFirstString: String;
    function GetLang(const Index: Integer): Integer;
    function GetStruct(const Index: Integer): TErfStruct;
    function GetStructByName(const Name: String): TErfStruct;
    function GetName(const Index: Integer): String;
    function GetType(const Index: Integer): String;
    function GetTypeId(const Index: Integer): Word;

    procedure SetString(const Lang: Integer; const Value: String);
    procedure SetFirstString(const Value: String);
    procedure SetLang(const Index: Integer; const Value: Integer);
    procedure SetName(const Index: Integer; const Value: String);
    procedure SetType(const Index: Integer; const Value: String);
    procedure SetTypeId(const Index: Integer; const Value: Word);

    procedure DumpStruct(const Struct: TErfStruct);

    procedure AddStruct(
      Result, AtStruct	: TErfStruct;
      InsertAsNext	: Boolean
    );

    procedure DeleteStruct(const Struct: TErfStruct);

    function ExportStruct(
      const Struct	: TErfStruct;
      const SaveAs	: String
    ): TLetoError;

    function OpenStruct(const Name: String): TLetoError;

    procedure CloseStruct(const Struct: TErfStruct);

    function NamedOp(Name: String; Op: Byte): TLetoError;
    function IndexedOp(const Index: Integer; Op: Byte): TLetoError;
    function StructOp(const Struct: TErfStruct; Op: Byte): TLetoError;

  protected

    procedure DoDestroy; virtual;

  public

    Loaded		: Boolean;
    Modified		: Boolean;

    FileType		: TLetoFileType;
    Header		: TErfHeader;

    Signature		: String;
    Version		: String;

    DescStrRef		: Cardinal;

    constructor Create;
    constructor CreateFrom(const AFileName: String);
    destructor Destroy; override;

    procedure Clear;

    function LoadFromFile(
      const AFileName	: String = '';
      OnProgress	: TLetoProgressEvent = nil
    ): TLetoError;

    function LoadFromStream(
      const Stream	: TStream;
      OnProgress	: TLetoProgressEvent = nil
    ): TLetoError;

    function SaveToFile(
      const AFileName	: String = '';
      OnProgress	: TLetoProgressEvent = nil
    ): TLetoError;

    function SaveToStream(
      Stream		: TStream;
      OnProgress	: TLetoProgressEvent = nil
    ): TLetoError;

    property FileName: String read FFileName write FFileName;

    property Count: Integer read GetCount;

    property Strings[const Lang: Integer]: String
      read GetString write SetString;

    property Description: String
      read GetFirstString write SetFirstString;

    property Langs[const Index: Integer]: Integer
      read GetLang write SetLang;

    property Structs[const Index: Integer]: TErfStruct
      read GetStruct;

    property StructsByName[const Name: String]: TErfStruct
      read GetStructByName; default;

    property First: TErfStruct read FFirstStruct;
    property Last: TErfStruct read FLastStruct;

    property Names[const Index: Integer]: String
      read GetName write SetName;

    property Types[const Index: Integer]: String
      read GetType write SetType;

    property TypeIds[const Index: Integer]: Word
      read GetTypeId write SetTypeId;

    property OpenStructs: TStringList read FOpenStructs;

    function Insert(
      Gff		: TGffFile;
      Name		: String;
      ResType		: String = 'BIC';
      AtStruct		: TErfStruct = nil;
      InsertAsNext	: Boolean = True
    ): TErfStruct; overload;

    function Insert(
      FileName		: String;
      Name		: String = '';
      AtStruct		: TErfStruct = nil;
      InsertAsNext	: Boolean = True
    ): TErfStruct; overload;

    function Insert(
      Stream		: TMemoryStream;
      Name		: String;
      ResType		: String = 'BIC';
      AtStruct		: TErfStruct = nil;
      InsertAsNext	: Boolean = True
    ): TErfStruct; overload;

    function Remove(
      const Name	: String
    ):TLetoError; overload;
    procedure Remove(
      const Index	: Integer
    ); overload;
    procedure Remove(
      const Struct	: TErfStruct
    ); overload;

    function ExportToFile(
      const Name	: String;
      const SaveAs	: String = ''
    ): TLetoError; overload;
    function ExportToFile(
      const Index	: Integer;
      const SaveAs	: String = ''
    ): TLetoError; overload;
    function ExportToFile(
      const Struct	: TErfStruct;
      const SaveAs	: String = ''
    ): TLetoError; overload;

    function Open(const Name: String): TLetoError;
    property Opened: TErfStruct read FOpened;

    procedure Close(const Name: String = '*.*'); overload;
    procedure Close(const Index: Integer); overload;
    procedure Close(const Struct: TErfStruct); overload;

    function GetListOfNames(
      ResName, ResType	: String
    ) : TStringList; overload;
    function GetListOfNames(
      const Mask	: String = '*.*'
    ): TStringList; overload;

    property OnDestroy: TMultiNotifyEvent read FOnDestroy;

  end;


implementation

const
  ERF_MINSIZE		= $A0; // 160 bytes
  ERF_RESERVED_SIZE	= $74; // 116 bytes
  ERF_HEADER_SIZE	= $2C; // 44 bytes

  ERFOP_REMOVE		= 0;
  ERFOP_EXPORT		= 1;
  ERFOP_OPEN		= 2;
  ERFOP_CLOSE		= 3;


{ TErfFile }


(*------------------------------------------------------------------------------

	Private

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
property Count

------------------------------------------------------------------------------*)
function TErfFile.GetCount: Integer;
begin
  Result		:= FCount;
end;

(*------------------------------------------------------------------------------
property Strings

------------------------------------------------------------------------------*)
function TErfFile.GetString(const Lang: Integer): String;
begin
  Result		:= FStrings.Values[IntToStr(Lang)];
end;

procedure TErfFile.SetString(const Lang: Integer; const Value: String);
begin
  FStrings.Values[IntToStr(Lang)] := Value;
end;

(*------------------------------------------------------------------------------
property Description

------------------------------------------------------------------------------*)
function TErfFile.GetFirstString: String;
begin
  if FStrings.Count > 0 then
    Result		:= FStrings.ValueFromIndex[0]
  else
    Result		:= '';
end;

procedure TErfFile.SetFirstString(const Value: String);
begin
  if FStrings.Count < 1 then
    FStrings.Values['0']	:= Value
  else
    FStrings.ValueFromIndex[0]	:= Value;
end;

(*------------------------------------------------------------------------------
property Langs

------------------------------------------------------------------------------*)
function TErfFile.GetLang(const Index: Integer): Integer;
begin
  Result		:= -1;
  if (Index < 0) or (Index > FStrings.Count-1) then
    Exit;
  if not TryStrToInt(FStrings.Names[Index], Result) then
    Result		:= -1;
end;

procedure TErfFile.SetLang(const Index: Integer; const Value: Integer);
begin
  if (Index < 0) or (Index > FStrings.Count-1) then
    Exit;
  FStrings[Index] := IntToStr(Value) + '=' + FStrings.ValueFromIndex[Index];
end;

(*------------------------------------------------------------------------------
property Structs

------------------------------------------------------------------------------*)
function TErfFile.GetStruct(const Index: Integer): TErfStruct;
var
  I			: Integer;
begin
  if Index = -1 then begin
    Result		:= Last;
    Exit;
  end;

  Result		:= First;
  I			:= 0;
  while (Result <> nil) and (I < Index) do begin
    Result		:= Result.Next;
    Inc(I);
  end;
end;

(*------------------------------------------------------------------------------
property StructsByName

------------------------------------------------------------------------------*)
function TErfFile.GetStructByName(const Name: String): TErfStruct;
var
  Pt			: Integer;
  N, T			: String;
  Id			: Word;
begin

  N			:= Name;
  Pt			:= Pos('.', Name);
  if Pt > 0 then begin
    N			:= Copy(Name, 0, Pt-1);
    T			:= Copy(Name, Pt+1, Length(Name));
  end;

  N			:= Lowercase(N);
  Id			:= GetResTypeId(T);

  Result		:= First;
  while Assigned(Result) do
    if
      ( N = Lowercase(Result.Name) ) and
      ( (T = '') or (Id = Result.ResTypeId) )
    then
      Break
    else
      Result		:= Result.Next;

end;

(*------------------------------------------------------------------------------
property Names

------------------------------------------------------------------------------*)
function TErfFile.GetName(const Index: Integer): String;
begin
  Result		:= '';
  if (Index < 0) or (Index > Count-1) then
    Exit;

  Result		:= Structs[Index].Name;

end;

procedure TErfFile.SetName(const Index: Integer; const Value: String);
begin
  if (Index < 0) or (Index > Count-1) then
    Exit;

  Structs[Index].Name	:= Value;

end;

(*------------------------------------------------------------------------------
property Types

------------------------------------------------------------------------------*)
function TErfFile.GetType(const Index: Integer): String;
begin
  Result		:= '---';
  if (Index < 0) or (Index > Count-1) then
    Exit;

  Result		:= Structs[Index].ResType;

end;

procedure TErfFile.SetType(const Index: Integer; const Value: String);
begin
  if (Index < 0) or (Index > Count-1) then
    Exit;

  Structs[Index].ResType := Value;

end;

(*------------------------------------------------------------------------------
property TypeIds

------------------------------------------------------------------------------*)
function TErfFile.GetTypeId(const Index: Integer): Word;
begin
  Result		:= $FFFF;
  if (Index < 0) or (Index > Count-1) then
    Exit;

  Result		:= Structs[Index].ResTypeId;

end;

procedure TErfFile.SetTypeId(const Index: Integer; const Value: Word);
begin
  if (Index < 0) or (Index > Count-1) then
    Exit;

  Structs[Index].ResTypeId := Value;

end;

(*------------------------------------------------------------------------------
DumpStruct

Synchronize Struct.Data when Struct is a GFF, is loaded, and is modified.

------------------------------------------------------------------------------*)
procedure TErfFile.DumpStruct(const Struct: TErfStruct);
begin
  // TODO 4: CHANGE: Guarantee the Modified prop and skip SaveToStream if !
  if Struct.Loaded then begin
    Struct.Data.Clear;
    Struct.Gff.SaveToStream(Struct.Data);
  end;
end;

(*------------------------------------------------------------------------------
AddStruct

Handles the tricky linked list management after Insert actually creates
the new ErfStruct.

------------------------------------------------------------------------------*)
procedure TErfFile.AddStruct(
  Result, AtStruct	: TErfStruct;
  InsertAsNext		: Boolean
);
var
  ASNext, ASPrev	: TErfStruct;
begin

  if not Assigned(Result) then
    Exit;

  if not Assigned(AtStruct) then
    AtStruct		:= Last;

  ASNext		:= nil;
  ASPrev		:= nil;
  if Assigned(AtStruct) then begin
    ASNext		:= AtStruct.Next;
    ASPrev		:= AtStruct.Prev;
  end;

  if InsertAsNext then begin
    Result.Prev		:= AtStruct;
    Result.Next		:= ASNext;
    if Assigned(AtStruct) then
      AtStruct.Next	:= Result;
    if Assigned(ASNext) then
      ASNext.Prev	:= Result;
  end else begin
    Result.Next		:= AtStruct;
    Result.Prev		:= ASPrev;
    if Assigned(AtStruct) then
      AtStruct.Prev	:= Result;
    if Assigned(ASPrev) then
      ASPrev.Next	:= Result;
  end;

  if not Assigned(Result.Next) then
    FLastStruct		:= Result;
  if not Assigned(Result.Prev) then
    FFirstStruct	:= Result;

  Modified		:= True;

  Inc(FCount);

end;

(*------------------------------------------------------------------------------
DeleteStruct

Since Remove can be called in three flavors, each acts as a wrapper for
this procedure, that does the actual deleting, once the given Struct is
located.

------------------------------------------------------------------------------*)
procedure TErfFile.DeleteStruct(const Struct: TErfStruct);
var
  Prev, Next		: TErfStruct;
begin
  if not Assigned(Struct) then Exit;

  Prev			:= Struct.Prev;
  Next			:= Struct.Next;

  if Assigned(Prev) then
    Prev.Next		:= Next
  else
    FFirstStruct	:= Next;

  if Assigned(Next) then
    Next.Prev		:= Prev
  else
    FLastStruct		:= Prev;

  Dec(FCount);
  Struct.Free;

  Modified		:= True;

end;

(*------------------------------------------------------------------------------
ExportStruct

------------------------------------------------------------------------------*)
function TErfFile.ExportStruct(
  const Struct		: TErfStruct;
  const SaveAs		: String
): TLetoError;
var
  Stream		: TFileStream;
  Name			: String;
begin
  Result		:= Success;

  DumpStruct(Struct);

  if SaveAs = '' then
    Name		:= Struct.Name + '.' + Struct.ResType
  else
    Name		:= SaveAs;

  try
    Stream		:= TFileStream.Create(Name, fmCreate);
    try
      Struct.Data.SaveToStream(Stream);
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
OpenStruct

------------------------------------------------------------------------------*)
function TErfFile.OpenStruct(const Name: String): TLetoError;
var
  I			: Integer;
begin
  { Already open? }
  I			:= OpenStructs.IndexOf(Name);
  if I > -1 then begin
    Result		:= Success;
    FOpened		:= TErfStruct(OpenStructs.Objects[I]);
    Exit;
  end;

  FOpened		:= GetStructByName(Name);
  if Assigned(FOpened) then
    Result		:= FOpened.Parse
  else
    Result		:= Err_Invalid_Resource;

  if Result = Success then
    OpenStructs.AddObject(Name, FOpened);

end;

(*------------------------------------------------------------------------------
CloseStruct

------------------------------------------------------------------------------*)
procedure TErfFile.CloseStruct(const Struct: TErfStruct);
var
  I			: Integer;
begin
  if not Assigned(OpenStructs) then Exit;
  for I := 0 to OpenStructs.Count-1 do
    if TErfStruct(OpenStructs.Objects[I]) = Struct then begin
      OpenStructs.Delete(I);
      Break;
    end;

  DumpStruct(Struct);
  Struct.Gff.Free;
  Struct.Gff		:= nil;
end;

(*------------------------------------------------------------------------------
NamedOp

These three functions do the redundant work for Remove, Export, Close.

------------------------------------------------------------------------------*)
function TErfFile.NamedOp(Name: String; Op: Byte): TLetoError;
var
  Wild			: Boolean;
  Struct, Next		: TErfStruct;
  S			: String;
begin
  Result		:= Err_Invalid_Resource;

  if Name = '' then Exit;

  Result		:= Success;

  Name			:= Lowercase(Name);
  Wild			:= (Pos('*', Name) > 0) or (Pos('?', Name) > 0);

  Struct		:= First;

  while Assigned(Struct) do begin
    S			:= Lowercase(Struct.Name + '.' + Struct.ResType);

    if (Name = S) or StringMatch(Name, S) then begin
      Next		:= Struct.Next;

      case Op of

        ERFOP_REMOVE:
          DeleteStruct(Struct);

        ERFOP_EXPORT:
          if Result = Success then
            Result	:= ExportStruct(Struct, ExportAs)
          else
            ExportStruct(Struct, ExportAs);

        ERFOP_OPEN:
          Result	:= OpenStruct(Name);

        ERFOP_CLOSE:
          CloseStruct(Struct);

      end;

      if not Wild then Exit;

      Struct		:= Next;

    end else
      Struct		:= Struct.Next;

  end;

  if not Wild then
    Result		:= Err_Invalid_Resource;

end;

(*------------------------------------------------------------------------------
IndexedOp

------------------------------------------------------------------------------*)
function TErfFile.IndexedOp(const Index: Integer; Op: Byte): TLetoError;
var
  Struct		: TErfStruct;
begin
  Result		:= Err_Invalid_Resource;

  Struct		:= Structs[Index];

  if not Assigned(Struct) then Exit;

  Result		:= Success;

  case Op of
    ERFOP_REMOVE:
      DeleteStruct(Struct);
    ERFOP_EXPORT:
      Result := ExportStruct(Struct, ExportAs);
    ERFOP_CLOSE:
      CloseStruct(Struct);
  end;

end;

(*------------------------------------------------------------------------------
StructOp

------------------------------------------------------------------------------*)
function TErfFile.StructOp(const Struct: TErfStruct; Op: Byte): TLetoError;
begin
  Result		:= Err_Internal;

  if not Assigned(Struct) then Exit;

  Result		:= Success;

  case Op of
    ERFOP_REMOVE:
      DeleteStruct(Struct);
    ERFOP_EXPORT:
      Result := ExportStruct(Struct, ExportAs);
    ERFOP_CLOSE:
      CloseStruct(Struct);
  end;

end;


(*------------------------------------------------------------------------------

	Public

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
DoDestroy

------------------------------------------------------------------------------*)
procedure TErfFile.DoDestroy;
begin
  OnDestroy.Signal(self);
end;


(*------------------------------------------------------------------------------

	Public

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
constructors

------------------------------------------------------------------------------*)
constructor TErfFile.Create;
begin
  Loaded		:= False;
  FStrings		:= TStringList.Create;
  FOpenStructs		:= TStringList.Create;
  FCount		:= 0;
  FOnDestroy		:= TMultiNotifyEvent.Create;
end;

constructor TErfFile.CreateFrom(const AFileName: String);
begin
  Create;
  LoadFromFile(AFileName);
end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TErfFile.Destroy;
begin
  DoDestroy;

  Clear;

  FreeAndNil(FStrings);
  FreeAndNil(FOpenStructs);
  FreeAndNil(FOnDestroy);

  inherited;
end;

(*------------------------------------------------------------------------------
Clear

------------------------------------------------------------------------------*)
procedure TErfFile.Clear;
begin
  FStrings.Clear;

  while Assigned(First) do
    DeleteStruct(First);

  Loaded		:= False;
  Modified		:= False;
  Signature		:= '';
  Version		:= '';

end;

(*------------------------------------------------------------------------------
LoadFromFile

------------------------------------------------------------------------------*)
function TErfFile.LoadFromFile(
  const AFileName	: String;
  OnProgress		: TLetoProgressEvent
): TLetoError;
var
  Stream		: TMemoryStream;
begin

  if AFileName <> '' then
    FFileName		:= AFileName;

  if not FileExists(FFileName) then begin
    Result		:= Err_Missing_File;
    Exit;
  end;

  Stream		:= TMemoryStream.Create;
  Stream.LoadFromFile(FFileName);

  Result		:= LoadFromStream(Stream, OnProgress);

  FreeAndNil(Stream);

end;

(*------------------------------------------------------------------------------
LoadFromStream

------------------------------------------------------------------------------*)
function TErfFile.LoadFromStream(
  const Stream		: TStream;
  OnProgress		: TLetoProgressEvent
): TLetoError;
var
  I			: Integer;
  L, Offset, Size	: Cardinal;
  Pos			: Int64;
  Names			: TStringList;
  ResTypes		: array of Word;
  Name, ResType		: String;
  Struct		: TErfStruct;
  Payload		: TMemoryStream;

  Done			: Integer;

begin

  if not Assigned(Stream) then begin
    Result		:= Err_Internal;
    Exit;
  end;

  if not IsErfStream(Stream) or (Stream.Size < ERF_MINSIZE) then begin
    Result		:= Err_Not_Erf;
    Exit;
  end;

  Done			:= 0;

  if Assigned(OnProgress) then
    OnProgress(self, psStarting, Done, 0, '(Examining) ');

  FSize			:= Stream.Size;
  Names			:= TStringList.Create;
  Payload		:= TMemoryStream.Create;

  try

  // Read the header
  //
  Stream.Seek(0, soFromBeginning);
  with Header do begin
    Sig				:= ReadString(Stream, 4);
    Ver				:= ReadString(Stream, 4);
    LanguageCount		:= ReadLong(Stream);
    LocalizedStringSize		:= ReadLong(Stream);
    EntryCount			:= ReadLong(Stream);
    OffsetToLocalizedString	:= ReadLong(Stream);
    OffsetToKeyList		:= ReadLong(Stream);
    OffsetToResourceList	:= ReadLong(Stream);
    BuildYear			:= 1900 + ReadLong(Stream);
    BuildDay			:= ReadLong(Stream);
    DescriptionStrRef		:= ReadLong(Stream);
    FileType			:= GuessFileTypeFromSig(Sig);
    Signature			:= Sig;
    Version			:= Ver;
    DescStrRef			:= DescriptionStrRef;
  end; // with Header

  TryStrToFileType(Copy(Header.Sig, 1, 3), FileType);
  Signature			:= Header.Sig;
  Version			:= Header.Ver;

  // The reserved data is just skipped.
  Stream.Seek(ERF_RESERVED_SIZE, soFromCurrent);

  // Strings (Descriptions)
  //
  FStrings.Clear;
  for I := 0 to Header.LanguageCount-1 do begin
    L			:= ReadLong(Stream);
    Size		:= ReadLong(Stream);
    Strings[L]		:= ReadString(Stream, Size);
  end;

  // For sanity
  Stream.Seek(Header.OffsetToKeyList, 0);

  // Names, ResTypes
  SetLength(ResTypes, Header.EntryCount);
  for I := 0 to Header.EntryCount-1 do begin
    // The name (ResRef) of the resource
    Names.Add( ReadString(Stream, 16) );
    // This is the "redundant" ResId
    ReadLong(Stream);
    // The ResType, which is a WORD (so ReadInt is not used)
    Stream.Read(ResTypes[I], 2);
    // The remaining two "unused" bytes
    ReadInt(Stream, 2);
  end;

  { Although it is undocumented by BioWare (it is documented elsewhere
    in the community, at least), there apparently exists in MOD and
    NWM files an amount of 'blank space' immediately after the
    Key List. The length is 8 x EntryCount bytes. Therefore, the
    Seek below is to ensure that if this blank space exists, we
    skip past it.
  }
  Stream.Seek(Header.OffsetToResourceList, soFromBeginning);

  // Resources
  Struct		:= nil;
  for I := 0 to Header.EntryCount-1 do begin

    Name		:= Names[I];
    ResType		:= GetResTypeName(ResTypes[I]);

    if Assigned(OnProgress) then
      OnProgress(
        self, psRunning,
        Done, Count,
        Name + '.' + ResType
      );

    Offset		:= ReadLong(Stream);
    Size		:= ReadLong(Stream);

    { DROP this resource if it indicates a bad (or BAAD) offset,
      and check using an Int64 to avoid an EIntOverflow for
      really fooey values.
    }
    if Stream.Size < Int64(Offset) + Int64(Size) then
      Continue;

    Pos			:= Stream.Position;

    Stream.Seek(Offset, soFromBeginning);
    Payload.Clear;
    if Size > 0 then
      Payload.CopyFrom(Stream, Size);

    Struct		:= Insert(Payload, Name, ResType, Struct);

    Stream.Seek(Pos, soFromBeginning);

    Inc(Done);

  end;

  Result		:= Success;

  Loaded		:= Result = Success;
  Modified		:= False;

  finally
    FreeAndNil(Names);
    FreeAndNil(Payload);
  end;

end;

(*------------------------------------------------------------------------------
SaveToFile

------------------------------------------------------------------------------*)
function TErfFile.SaveToFile(
  const AFileName	: String;
  OnProgress		: TLetoProgressEvent
): TLetoError;
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
      Result		:= SaveToStream(Stream, OnProgress);
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
// TODO 4: CHANGE: Should the Structs be organized alphabetically?
function TErfFile.SaveToStream(
  Stream		: TStream;
  OnProgress		: TLetoProgressEvent
): TLetoError;
var
  Struct		: TErfStruct;

  HeaderStream,
    StringStream,
    KeyStream,
    ListStream		: TMemoryStream;

  Header		: TErfHeader;
  I			: Integer;
  L			: Cardinal;
  Offset, Size		: Cardinal;
  EmptyChar		: Char;
  ResID			: Cardinal;
  Unused		: Word;
  S			: String;

  Done			: Integer;

begin
  Result		:= Err_Internal;
  if not Assigned(Stream) then Exit;

  Result		:= Success;

  //Stream.Size		:= FSize;

  Done			:= 0;

  if Assigned(OnProgress) then
    OnProgress(self, psStarting, Done, Count, '(Preparing)');

  HeaderStream		:= TMemoryStream.Create;
  StringStream		:= TMemoryStream.Create;
  KeyStream		:= TMemoryStream.Create;
  ListStream		:= TMemoryStream.Create;

  Unused		:= 0;

  // Construct Localized String List
  //
  for I := 0 to FStrings.Count-1 do begin
    S			:= FStrings.ValueFromIndex[I];
    L			:= StrToInt(FStrings.Names[I]);
    Size		:= Length(S);

    StringStream.Write(L, 4);
    StringStream.Write(Size, 4);
    StringStream.Write(PChar(S)^, Size);
  end;

  // Prepare all Structs, construct Key List and Resource List
  //
  // TODO 4: CHANGE: Sort resources alphabetically (convenience)
  Struct		:= First;
  ResId			:= 0;
  EmptyChar		:= #0;
    Offset :=
    ERF_HEADER_SIZE + ERF_RESERVED_SIZE +
    StringStream.Size +
    (Count * 24) + (Count * 8);

  // TODO 4: CHANGE: Optimize
  while Assigned(Struct) do begin

    if Assigned(OnProgress) then
      OnProgress(
        self, psRunning,
        Done, Count,
        Struct.Name + '.' + Struct.ResType
      );

    // Prep the stream - if Struct is not Loaded, it is assumed that
    // Data already holds the arbitrary Stream destined for output.
    DumpStruct(Struct);

    // ResRef
    for I := 1 to 16 do
      if I > Length(Struct.Name) then
        KeyStream.Write(EmptyChar, 1)
      else
        KeyStream.Write(Struct.Name[I], 1);
    // ResID
    KeyStream.Write(ResId, 4);
    Inc(ResId);
    // ResType (and the 'unused' two bytes)
    KeyStream.Write(Struct.ResTypeId, 2);
    KeyStream.Write(Unused, 2);

    // OffsetToResource, ResourceSize
    Size		:= Struct.Data.Size;
    ListStream.Write(Offset, 4);
    ListStream.Write(Size, 4);
    Inc(Offset, Size);

    Inc(Done);

    Struct		:= Struct.Next;

  end;

  if Assigned(OnProgress) then
    OnProgress(self, psEnding, Done, Count, '(Saving)');

  if Signature = '' then
    Signature		:= 'ERF '
  else if Length(Signature) < 4 then
    Signature		:= Signature + StringOfChar(' ', 4-Length(Signature));
  if Version = '' then
    Version		:= 'V1.0'
  else if Length(Version) < 4 then
    Version		:= Version + StringOfChar(' ', 4-Length(Version));

  // Construct Header, HeaderStream
  //
  with Header do begin
    Sig				:= Signature;
    Ver				:= Version;
    LanguageCount		:= FStrings.Count;
    LocalizedStringSize		:= StringStream.Size;
    EntryCount			:= Count;
    Offset			:= ERF_HEADER_SIZE + ERF_RESERVED_SIZE;
    OffsetToLocalizedString	:= Offset;
    Offset			:= Offset + LocalizedStringSize;
    OffsetToKeyList		:= Offset;
    Offset			:= Offset + KeyStream.Size;
    OffsetToResourceList	:= Offset;
    BuildYear			:= YearOf(Date) - 1900;
    BuildDay			:= DayOfTheYear(Date) - 1;
    DescriptionStrRef		:= DescStrRef;

    HeaderStream.Write(PChar(Sig)^, 4);
    HeaderStream.Write(PChar(Ver)^, 4);
    HeaderStream.Write(LanguageCount, 4);
    HeaderStream.Write(LocalizedStringSize, 4);
    HeaderStream.Write(EntryCount, 4);
    HeaderStream.Write(OffsetToLocalizedString, 4);
    HeaderStream.Write(OffsetToKeyList, 4);
    HeaderStream.Write(OffsetToResourceList, 4);
    HeaderStream.Write(BuildYear, 4);
    HeaderStream.Write(BuildDay, 4);
    HeaderStream.Write(DescriptionStrRef, 4);
    // The reserved portion of the Header
    for I := 1 to ERF_RESERVED_SIZE do
      HeaderStream.Write(Unused, 1);
  end; // with Header

  // Construct Result
  //
  Stream.CopyFrom(HeaderStream, 0);
  Stream.CopyFrom(StringStream, 0);
  Stream.CopyFrom(KeyStream, 0);
  Stream.CopyFrom(ListStream, 0);
  Struct		:= First;
  while Assigned(Struct) do begin
    Stream.CopyFrom(Struct.Data, 0);
    Struct		:= Struct.Next;
  end;

  // Cleanup
  //
  FreeAndNil(HeaderStream);
  FreeAndNil(StringStream);
  FreeAndNil(KeyStream);
  FreeAndNil(ListStream);

end;

(*------------------------------------------------------------------------------
Insert

Add a new Struct to the ERF. There are three versions, for inserting a GFF
from Filename or instance (TGffFile), or to insert a non-GFF (any arbitrary
data), a TMemoryStream can be given.

If the given GFF, FileName, or Stream is not valid, the Result is nil.

The Name (ResName) and ResType should both be specified, though ResType is
optional (it will be 'BIC' if not given). Note how ResType is specified
as a string, rather than a WORD ResTypeId. This is intended as a convenience
for the user; the functions GetResTypeName and GetResTypeId can convert
back and forth between the string and WORD representations. The ResType is
not case-sensitive. (You could say either 'BIC' or 'bic'.)

Specify AtStruct to indicate where the new Struct should be added. If this
is not specified, the Struct is appended to the end of the current list.

InsertAsNext can be toggled to insert a Struct before or after the given
AtStruct. The default behavior is inserting the Struct after AtStruct.
Set InsertAsNext to False to insert the Struct *before* AtStruct.

Insert does not replace. (Use a combination of Insert and Delete.)

Examples:

Add a new Struct to the end of the ERF:
  Insert(Gff, 'Module', 'IFO');

Add a new Struct to the beginning of the ERF:
  Insert(Gff, 'Module', 'IFO', First, False);

Add a new Struct as the third Struct in the ERF:
  Insert(Gff, 'Module', 'IFO', Structs[1]);
   - or -
  Insert(Gff, 'Module', 'IFO', Structs[2], False);

------------------------------------------------------------------------------*)
function TErfFile.Insert(
  Gff			: TGffFile;
  Name			: String;
  ResType		: String;
  AtStruct		: TErfStruct;
  InsertAsNext		: Boolean
): TErfStruct;
begin
  Result		:= TErfStruct.CreateAs(Gff, Name, ResType);

  AddStruct(Result, AtStruct, InsertAsNext);

end;

function TErfFile.Insert(
  FileName		: String;
  Name			: String;
  AtStruct		: TErfStruct;
  InsertAsNext		: Boolean
): TErfStruct;
var
  Ext			: String;
begin
  if Name = '' then
    Name		:= FileName;

  Ext			:= ExtractFileExt(Name);
  Name			:= StringReplace(ExtractFileName(Name), Ext, '', []);
  if Ext = '' then
    Ext			:= '.bic';
  System.Delete(Ext, 1, 1);

  Result		:= TErfStruct.CreateAs(FileName, Name, Uppercase(Ext));

  AddStruct(Result, AtStruct, InsertAsNext);

end;

function TErfFile.Insert(
  Stream		: TMemoryStream;
  Name			: String;
  ResType		: String;
  AtStruct		: TErfStruct;
  InsertAsNext		: Boolean
): TErfStruct;
begin
  Result		:= TErfStruct.CreateAs(Stream, Name, ResType);

  AddStruct(Result, AtStruct, InsertAsNext);

end;

(*------------------------------------------------------------------------------
Remove

This comes in three flavors - you can specify a Name, an Index, or pass in a
reference to the ErfStruct you want deleted.

If the Index is invalid or the Struct given is nil / not part of this ERF,
nothing happens. Name can include wildcards. If it doesn't match any structs,
nothing happens.

------------------------------------------------------------------------------*)
function TErfFile.Remove(const Name: String): TLetoError;
begin
  Result := NamedOp(Name, ERFOP_REMOVE);
end;

procedure TErfFile.Remove(const Index: Integer);
begin
  DeleteStruct(Structs[Index]);
end;

procedure TErfFile.Remove(const Struct: TErfStruct);
begin
  DeleteStruct(Struct);
end;

(*------------------------------------------------------------------------------
ExportToFile

Saves the resource / data to a file on disk.

------------------------------------------------------------------------------*)
function TErfFile.ExportToFile(
  const Name		: String;
  const SaveAs		: String
): TLetoError;
begin
  ExportAs		:= SaveAs;
  Result		:= NamedOp(Name, ERFOP_EXPORT);
end;

function TErfFile.ExportToFile(
  const Index		: Integer;
  const SaveAs		: String
): TLetoError;
begin
  ExportAs		:= SaveAs;
  Result		:= IndexedOp(Index, ERFOP_EXPORT);
end;

function TErfFile.ExportToFile(
  const Struct		: TErfStruct;
  const SaveAs		: String
): TLetoError;
begin
  ExportAs		:= SaveAs;
  Result		:= StructOp(Struct, ERFOP_EXPORT);
end;

(*------------------------------------------------------------------------------
Open

Opens a struct and places it on the OpenStructs list. Allows for simultaneous
multiple open structs. This is intended strictly for GFF structs. That is,
"open" in this sense means, "locate then parse".

If the Result is Success, the opened struct can be referenced with Opened.
It in turn should have a valid .Gff.

------------------------------------------------------------------------------*)
function TErfFile.Open(const Name: String): TLetoError;
begin
  Result		:= NamedOp(Name, ERFOP_OPEN);
end;

(*------------------------------------------------------------------------------
Close

Push the struct out of memory to free resources.

------------------------------------------------------------------------------*)
procedure TErfFile.Close(const Name: String);
begin
  NamedOp(Name, ERFOP_CLOSE);
end;

procedure TErfFile.Close(const Index: Integer);
begin
  IndexedOp(Index, ERFOP_CLOSE);
end;

procedure TErfFile.Close(const Struct: TErfStruct);
begin
  StructOp(Struct, ERFOP_CLOSE);
end;

(*------------------------------------------------------------------------------
GetListOfNames

This remits a list of resources matching ResName and ResType, which can
both contain wildcards ('?' and '*'). For instance,
  GetListOfNames('module', 'ifo');
would remit a list of one string ('module.ifo') if module.IFO is a resource
in the ERF;
  GetListOfNames('*', 'are');
remits all of the ARE resources. Etc.

None of the identified resources are actually loaded, this function merely
relies on the information provided by ResName and ResType of all the
member Structs.

------------------------------------------------------------------------------*)
function TErfFile.GetListOfNames(
  ResName, ResType	: String
): TStringList;
var
  Struct		: TErfStruct;
begin
  Result		:= TStringList.Create;

  ResName		:= Lowercase(ResName);
  ResType		:= Lowercase(ResType);

  Struct		:= First;
  while Assigned(Struct) do begin
    if
      StringMatch(ResName, Lowercase(Struct.Name)) and
      StringMatch(ResType, Lowercase(Struct.ResType))
    then
      Result.Add(Lowercase(Struct.Name) + '.' + Lowercase(Struct.ResType));

    Struct		:= Struct.Next;
  end;

end;

function TErfFile.GetListOfNames(const Mask: String): TStringList;
var
  ResName, ResType	: String;
  P			: Integer;
begin
  ResName		:= Mask;
  ResType		:= '*';
  P			:= Pos('.', Mask);
  if P > 0 then begin
    ResType		:= Copy(ResName, P+1, Length(ResName));
    ResName		:= Copy(ResName, 1, P-1);
  end;

  Result		:= GetListOfNames(ResName, ResType);
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
