(*------------------------------------------------------------------------------
Class_GffField

The atomic unit of data in a GFF is a Field, and this class is Leto's
implementation of it. This is the engine I speak of when I speak of Leto's
"engine", and it has been entirely re-written for version 4 for improved
speed and efficiency.

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)
unit Class_GffField;

interface

uses
  SysUtils, Classes, Math, Variants,
  Header_Leto;

type

  PGffVarData		= ^TGffVarData;
  TGffField		= class;
  TGffLocString		= class;

  TGffTracker		= record
    StructIndex		: Cardinal;
    FieldIndex		: Cardinal;
    LabelIndex		: Cardinal;
    Names		: array of String;
    DataSize		: Cardinal;
    FieldIndicesSize	: Cardinal;
    ListIndicesSize	: Cardinal;
    { These are relative, in bytes, to Header.*Offset }
    DataOffset		: Cardinal;
    FieldIndicesOffset	: Cardinal;
    ListIndicesOffset	: Cardinal;
  end;

  TMatchBehavior	= (
    mbCIValue,
    mbNoChildCheck, mbMustHaveChild, mbMustHaveNoChild,
    mbWildName, mbWildValue,
    mbReplace, mbSetValue, mbStrRefCheck, mbSetStrRef,
    mbDelete, mbDeleteParent, mbDeleteMe
  );

  TGffMatchEvent	= procedure(
    Sender		: TObject;
    Field		: TGffField;
    var Matches		: Boolean
  ) of object;

  TGffMatchParams	= record
    Root		: TGffField;
    Name		: String;
    Value		: String;
    Lang		: Integer;
    Types		: TGffVarSet;
    StrRef		: Cardinal;
    Depth		: Integer;
    Target		: Integer;
    SetType		: TGffVarType;
    SetLang		: Integer;
    SetValue		: String;
    SetStrRef		: Cardinal;
    Behavior		: set of TMatchBehavior;
    Results		: TStringList;
    ChangesMade		: Boolean;
    OnMatch		: TGffMatchEvent;
  end;


  TGffVarData		= record
    AsString		: String;
    AsResRef		: String;
    AsLocString		: TGffLocString;
    AsVoid		: array of Byte;
    case TGffVarType of
      gffBYTE		: (AsByte: Byte);
      gffCHAR		: (AsChar: Shortint);
      gffWORD		: (AsWord: Word);
      gffSHORT		: (AsShort: Smallint);
      gffDWORD		: (AsDWord: Longword);
      gffINT		: (AsInt: Longint);
      gffDWORD64,
      gffINT64		: (AsInt64: Int64);
      gffFLOAT		: (AsFloat: Single);
      gffDOUBLE		: (AsDouble: Double);
  end;


  TGffField		= class

  private

    FType		: TGffVarType;
    FIsAnonymous	: Boolean;
    FLabel		: String;
    FLabelIndex		: Integer;
    FOffset		: Cardinal;
    FStructId		: Cardinal;
    FCount		: Integer;
    FSpare		: Integer;
    FData		: PGffVarData;

    FModified		: Boolean;
    FChanged		: Boolean;

    FFieldIndex		: Cardinal;
    FStructIndex	: Cardinal;

    FParent		: TGffField;

    //FOnChange		: TMultiNotifyEvent;
    //FOnDelete		: TMultiNotifyEvent;

    procedure SetName(Value: String);
    function GetChanged: Boolean;
    function GetHasChildren: Boolean;
    procedure SetParent(const Value: TGffField);

    function GetIndex: Integer;
    function GetDepth: Integer;
    function GetPath: String;

    function TryMatch(
      var Params	: TGffMatchParams;
      const ChildCheck,
        HaveChild	: Boolean
    ): Boolean;

    function GetAsVariant(Default: Variant): Variant;
    function GetAsByte: Byte;
    function GetAsChar: Shortint;
    function GetAsWord: Word;
    function GetAsShort: Smallint;
    function GetAsDWord: Longword;
    function GetAsInt: Longint;
    function GetAsInt64: Int64;
    function GetAsFloat: Single;
    function GetAsDouble: Double;
    function GetAsString: String;
    function GetAsLocString: TGffLocString;
    function GetAsResRef: String;

    procedure SetAsByte(const Value: Byte);
    procedure SetAsChar(const Value: Shortint);
    procedure SetAsWord(const Value: Word);
    procedure SetAsShort(const Value: Smallint);
    procedure SetAsDWord(const Value: Longword);
    procedure SetAsInt(const Value: Longint);
    procedure SetAsInt64(const Value: Int64);
    procedure SetAsFloat(const Value: Single);
    procedure SetAsDouble(const Value: Double);
    procedure SetAsString(const Value: String);
    procedure SetAsResRef(const Value: String);

    function GetIsValid: Boolean;
    function GetIsField: Boolean;
    function GetIsStruct: Boolean;
    function GetIsList: Boolean;
    function GetIsSimple: Boolean;
    function GetIsComplex: Boolean;
    function GetIsString: Boolean;
    function GetIsSterile: Boolean;

    procedure DoChange; virtual;
    procedure DoDelete; virtual;

  public

    FirstChild		: TGffField;
    LastChild		: TGffField;
    NextSibling		: TGffField;
    PrevSibling		: TGffField;

    Root		: TGffField;

    property VarType: TGffVarType read FType write FType;
    property Name: String read FLabel write SetName;
    property LabelIndex: Integer read FLabelIndex write FLabelIndex;
    property Offset: Cardinal read FOffset write FOffset;
    property Id: Cardinal read FStructId write FStructId;
    property Count: Integer read FCount;
    property Spare: Integer read FSpare write FSpare;
    property Data: PGffVarData read FData write FData;

    property Modified: Boolean read FModified write FModified;
    property Changed: Boolean read GetChanged write FChanged;

    property HasChildren: Boolean read GetHasChildren;

    property Parent: TGffField read FParent write SetParent;

    property Index: Integer read GetIndex;
    property Depth: Integer read GetDepth;
    property Path: String read GetPath;

    constructor Create(
      const AName	: String;
      const AParent	: TGffField = nil
    );
    destructor Destroy; override;

    function LoadFromStream(
      const LoadLevel	: TGffLoadLevel;
      const Header	: TGffHeader;
      const Stream	: TStream;
      const Names	: TStringList
    ): TLetoError;

    procedure ReIndex(var Tracker: TGffTracker);
    procedure SaveToStream(
      const Stream	: TStream;
      const Header	: TGffHeader;
      var Tracker	: TGffTracker
    );

    function AddChild(
      const AName	: String;
      const FieldType	: TGffVarType = gffInvalid
    ): TGffField; overload;

    function AddChild(
      const Child	: TGffField;
      const SameIndex	: Boolean;
      const AtPos	: Integer = -1;
      const MakeCopy	: Boolean = True
    ): TGffField; overload;

    function AddPrevSibling(
      const AName	: String;
      const FieldType	: TGffVarType = gffInvalid
    ): TGffField;

    procedure CopyFrom(
      const Source	: TGffField;
      const SameIndex	: Boolean
    );

    function CopyTo(
      const ToParent	: TGffField;
      const SameIndex	: Boolean
    ): TGffField;

    procedure Move(const ToIndex: Integer);

    //procedure Delete;

    procedure Clear;

    function GetAttrib(const Attr: String; var Value: String): Boolean; overload;
    function GetAttrib(const Attr, Key: String; var Value: String): Boolean; overload;

    function SetAttrib(const Attr, Value: String): Boolean; overload;
    function SetAttrib(
      const Attr, Key,
        Value		: String;
      const NilKey	: Boolean
    ): Boolean; overload;

    function GetNames: TStringList;

    function Find(
      const Name	: String;
      const FieldType	: TGffVarType = gffANY;
      const Recursive	: Boolean = True
    ): TGffField;

    procedure Match(var Params: TGffMatchParams);

    function Next: TGffField;
    function Previous: TGffField;
    function ChildOfIndex(const Index: Cardinal): TGffField;

    property AsVariant[Default: Variant]: Variant read GetAsVariant;
    property AsByte: Byte read GetAsByte write SetAsByte;
    property AsChar: Shortint read GetAsChar write SetAsChar;
    property AsWord: Word read GetAsWord write SetAsWord;
    property AsShort: Smallint read GetAsShort write SetAsShort;
    property AsDWord: Longword read GetAsDWord write SetAsDWord;
    property AsInt: Longint read GetAsInt write SetAsInt;
    property AsInt64: Int64 read GetAsInt64 write SetAsInt64;
    property AsFloat: Single read GetAsFloat write SetAsFloat;
    property AsDouble: Double read GetAsDouble write SetAsDouble;
    property AsString: String read GetAsString write SetAsString;
    property AsLocString: TGffLocString read GetAsLocString;
    property AsResRef: String read GetAsResRef write SetAsResRef;

    procedure SetValue(
      const Value	: Integer;
      const FieldType	: TGffVarType = gffUNSPECIFIED
    ); overload;
    procedure SetValue(
      const Value	: Int64;
      const FieldType	: TGffVarType = gffUNSPECIFIED
    ); overload;
    procedure SetValue(
      const Value	: Real;
      const FieldType	: TGffVarType = gffUNSPECIFIED
    ); overload;
    procedure SetValue(
      const Value	: String;
      const Lang	: Integer = -1;
      const FieldType	: TGffVarType = gffUNSPECIFIED
    ); overload;
    procedure SetValue(
      const Value	: Variant;
      const FieldType	: TGffVarType = gffUNSPECIFIED;
      const Lang	: Integer = -1
    ); overload;

    function Rename(const Newname: String): Boolean;

    property IsValid: Boolean read GetIsValid;
    property IsAnonymous: Boolean read FIsAnonymous write FIsAnonymous;
    property IsField: Boolean read GetIsField;
    property IsStruct: Boolean read GetIsStruct;
    property IsList: Boolean read GetIsList;
    property IsSimple: Boolean read GetIsSimple;
    property IsComplex: Boolean read GetIsComplex;
    property IsString: Boolean read GetIsString;
    property IsSterile: Boolean read GetIsSterile;

    //property OnChange: TMultiNotifyEvent read FOnChange;
    //property OnDelete: TMultiNotifyEvent read FOnDelete;

  end;


  TGffLocString		= class

  private

    FStringRef		: Cardinal;
    FLangs		: TStringList;

    FTotalSize		: Cardinal;

    function GetByLang(const Lang: Integer): String;
    procedure SetByLang(
      const Lang	: Integer;
      const Value	: String
    );

    function GetByIndex(const Index: Cardinal): String;
    procedure SetByIndex(
      const Index	: Cardinal;
      const Value	: String
    );

    function GetLanguage(const Index: Cardinal): Integer;
    procedure SetLanguage(
      const Index	: Cardinal;
      const Value	: Integer
    );

    function GetCount: Cardinal;

    function GetFirst: String;
    procedure SetFirst(const Value: String);

    function GetTotalSize: Cardinal;

  public

    property Langs: TStringList read FLangs;

    property ByLang[const Lang: Integer]: String
      read GetByLang write SetByLang; default;

    property ByIndex[const Index: Cardinal]: String
      read GetByIndex write SetByIndex;

    property Language[const Index: Cardinal]: Integer
      read GetLanguage write SetLanguage;

    property StringRef: Cardinal read FStringRef write FStringRef;
    // TODO 4: ADD: TGffLocString .TlkFile and .StrRef props

    property Count: Cardinal read GetCount;

    property First: String read GetFirst write SetFirst;

    constructor Create;
    destructor Destroy; override;

    function LoadFromStream(const Stream: TStream): Cardinal;
    function SaveToStream(const Stream: TStream): Cardinal;

    procedure Add(
      const Text	: String;
      const Lang	: Integer = 0
    );

    function Delete(const Index: Cardinal): Boolean;

    function DeleteLang(const Lang: Integer): Boolean;

    procedure Clear(const Finalize: Boolean = True);

    procedure CopyFrom(const Source: TGffLocString);

    function HasLang(const Lang: Cardinal): Boolean;

    function IndexOf(const Lang: Cardinal): Integer;

    function StrRefIsCustom: Boolean;

    function GetLangs: TStringList;

  end;


implementation


{ TGffField }


(*------------------------------------------------------------------------------

	Private

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
property Name

Some discussion is needed here.

BioWare's documentation on GFF describes adequately how Labels and Label
Indices operate. What the documentation fails to address are two crucial
problems any robust GFF editor must address:

1. Any real GFF editor can add and remove Fields. When a Field is removed,
what must become of its Label? Should it be removed, and consequently the
Label Indices of all other Fields re-organized? When a Field is added, if
it shares the same Label as an existing Field, should it re-use that
LabelIndex?

2. Neverwinter Nights, for which Leto was originally designed, is apparently
quite picky about certain Fields having certain Label Indices. The exact
how and why of this is undocumented, and appears so arbitrary that it is
probably in fact a bug. For this reason, even two different Fields which
have the same Label but different Label Indices must *never* be consolidated
into one Label / LabelIndex, or NWN will exhibit peculiar behavior.

Historically, Leto addressed the issue by de-compressing the entire Label
list - that is, every Field had a unique Label / Label Index. This causes
bloat in the file, because the LabelStream is now (n * 16) bytes large,
where before *most* of the Fields with same-Label re-used the same
LabelIndex.

The current behavior (although possibly not the finalized behavior for
this engine version) is something of a kludge - but no more "buggy" than
NWN's own LabelIndex algorithm.

When the GFF is first read in, the LabelIndex of everything is recorded.

If a Field is added or its Label is changed, its LabelIndex is set to -1
(the procedure below).

If a Field is deleted, nothing further is done to reconcile its Label (thus,
if that Field was the last Field to be using the Label, the Label is now
"lost".)

The ReIndex procedure, called before any Save operation, tracks the highest
LabelIndex in use (which was the last LabelIndex from when the file was
*loaded*, since everything new has -1).

When the GFF is written back to file, every Field's LabelIndex is checked.
If the LabelIndex is -1, it is assigned a new LabelIndex (using the Tracker,
which ReIndex gave a LabelIndex to). A StringList is then used to coordinate
index=label pairs, using StringList's name=value mechanism to 'track'
Fields using the same LabelIndex. Kludgy, but functional.

The Labels themselves are not written to the file here, but in TGffFile.
It loops over Names, again taking advantage of name=value. In this fashion,
the Labels are written in order, the original order is preserved, new Labels
are added to the end, and Fields may share or have unique Labels.

There's only one catch.

Since a Label is "lost" when a Field is deleted, and Label Indices are *not*
defragmented, there will be holes in the Label Array. TGffFile will still
loop over the LabelIndex, but since it will have no value, an empty Label
will be written in its place. No Field in the file references this Label.
During testing, this appeared to present no problem to the game.

Technically, this lost label is a waste of space. However, the root of the
whole damn problem could be described identically anyhow. The AE may
inevitably have a mechanism for editing the Label Array directly, which would
further alleviate any possible symptoms of this kludge, and furthermore the
problem it stems from.

------------------------------------------------------------------------------*)
procedure TGffField.SetName(Value: String);
begin
  FLabelIndex		:= -1;
  FLabel		:= Value;
  DoChange;
end;

(*------------------------------------------------------------------------------
property Changed

The Modified property indicates the Field has been changed, at all. The
Changed property indicates that the Field has recently changed. After checking
Changed, the value of Changed becomes False again. It will then be True after
another change occurs.

------------------------------------------------------------------------------*)
function TGffField.GetChanged: Boolean;
begin
  Result		:= FChanged;
  FChanged		:= False;
end;

(*------------------------------------------------------------------------------
property HasChildren

------------------------------------------------------------------------------*)
function TGffField.GetHasChildren: Boolean;
begin
  Result		:= FCount > 0;
end;

(*------------------------------------------------------------------------------
property Parent

This additionally performs the tasks of updating the Parent's First/LastChild,
and notifies siblings - in essence, this procedure adds the current field
as the last (newest) field beneath Parent (or divorces, when Value is nil).

------------------------------------------------------------------------------*)
procedure TGffField.SetParent(const Value: TGffField);
begin

  // When nil'ing the parent, this is a divorce operation:
  if not Assigned(Value) then begin
    if Assigned(PrevSibling) then
      PrevSibling.NextSibling := NextSibling
    else if Assigned(FParent) then
      FParent.FirstChild := NextSibling;

    if Assigned(NextSibling) then
      NextSibling.PrevSibling := PrevSibling
    else if Assigned(FParent) then
      FParent.LastChild := PrevSibling;

    if Assigned(FParent) then
      Dec(FParent.FCount);
    FParent		:= nil;

  // Otherwise (valid Parent value) this field becomes the new LastChild:
  end else begin
    FParent		:= Value;
    PrevSibling		:= FParent.LastChild;
    FParent.LastChild	:= self;

    if not Assigned(FParent.FirstChild) then
      FParent.FirstChild := self;

    if Assigned(PrevSibling) then
      PrevSibling.NextSibling := self;

    Inc(FParent.FCount);

  end;

  DoChange;

end;

(*------------------------------------------------------------------------------
property Index

------------------------------------------------------------------------------*)
function TGffField.GetIndex: Integer;
var
  Sib			: TGffField;
begin
  Result		:= 0;
  if not Assigned(PrevSibling) or not Assigned(Parent) then
  else if not Assigned(NextSibling) then
    Result		:= Parent.Count - 1
  else begin
    Sib			:= PrevSibling;
    while Assigned(Sib) do begin
      Inc(Result);
      Sib		:= Sib.PrevSibling;
    end;
  end;
end;

(*------------------------------------------------------------------------------
property Depth

------------------------------------------------------------------------------*)
function TGffField.GetDepth: Integer;
var
  P			: TGffField;
begin
  Result		:= 0;
  P			:= Parent;
  while Assigned(P) do begin
    Inc(Result);
    P			:= P.Parent;
  end;
end;

(*------------------------------------------------------------------------------
property Path

The full path of this Field, e.g,

LvlStatList/[0]/FeatList/[1]/Feat

------------------------------------------------------------------------------*)
function TGffField.GetPath: String;
begin
  if not Assigned(Parent) then
    Result		:= '@'
  else if not Assigned(Parent.Parent) then
    Result		:= FLabel
  else if not IsStruct then
    Result		:= Parent.Path + '/' + FLabel
  else
    Result		:= Parent.Path + '/[' + IntToStr(Index) + ']';
end;

(*------------------------------------------------------------------------------
TryMatch

------------------------------------------------------------------------------*)
function TGffField.TryMatch(
  var Params		: TGffMatchParams;
  const ChildCheck,
    HaveChild		: Boolean
): Boolean;
var
  HaveChildren		: Boolean;
  V			: String;
begin

  { Matching type? }
  Result		:= FType in Params.Types;

  if not Result then Exit;

  { Children }
  if FType = gffLocString then
    HaveChildren	:= AsLocString.Count > 0
  else
    HaveChildren	:= HasChildren;
  Result :=
    not ChildCheck or
    (HaveChild and HaveChildren) or
    (not HaveChild and not HaveChildren);

  if not Result then Exit;

  { Matching name? }
  V			:= FLabel;
  if V = '' then
    V			:= '*'
  else
    V			:= Lowercase(V);
  Result :=
    (Params.Name = '*') or
    (V = Params.Name) or
    (
      (mbWildName in Params.Behavior) and
      StringMatch(Params.Name, V)
    );

  if not Result then Exit;

  { Matching StrRef? }
  if (FType = gffLocString) and (mbStrRefCheck in Params.Behavior) then
    Result		:= AsLocString.StringRef = Params.StrRef;

  if not Result then Exit;

  { Matching value? }
  if FType = gffStruct then
    V		:= IntToStr(FStructId)
  else if (FType <> gffLocString) or (Params.Lang = -1) then
    V		:= AsString
  else
    V		:= AsLocString[Params.Lang];
  if mbCIValue in Params.Behavior then
    V		:= Lowercase(V);
  Result :=
    (Params.Value = '*') or (Params.Value = V) or
    ( (mbWildValue in Params.Behavior) and StringMatch(Params.Value, V) );

end;

(*------------------------------------------------------------------------------
property AsVariant

------------------------------------------------------------------------------*)
function TGffField.GetAsVariant(
  Default		: Variant
): Variant;
begin
  case FType of
    gffBYTE:
      Result		:= Data.AsByte;
    gffCHAR:
      Result		:= Data.AsChar;
    gffWORD:
      Result		:= Data.AsWord;
    gffSHORT:
      Result		:= Data.AsShort;
    gffDWORD:
      Result		:= Data.AsDWord;
    gffINT:
      Result		:= Data.AsInt;
    gffDWORD64, gffINT64:
      Result		:= Data.AsInt64;
    gffFLOAT:
      Result		:= Data.AsFloat;
    gffDOUBLE:
      Result		:= Data.AsDouble;
    gffString:
      Result		:= Data.AsString;
    gffResRef:
      Result		:= Data.AsResRef;
    gffLocString:
      Result		:= AsLocString.First;
    gffVoid:
      Result		:= Data.AsVoid;
  else
    Result		:= Default;
  end;
end;

(*------------------------------------------------------------------------------
property AsByte

------------------------------------------------------------------------------*)
function TGffField.GetAsByte: Byte;
begin
  if IsString then
    Result		:= 0
  else
    Result		:= AsVariant[0];
end;

procedure TGffField.SetAsByte(const Value: Byte);
begin
  Data.AsByte		:= Value;
  DoChange;
end;

(*------------------------------------------------------------------------------
property AsChar

------------------------------------------------------------------------------*)
function TGffField.GetAsChar: Shortint;
begin
  if IsString then
    Result		:= 0
  else
    Result		:= AsVariant[0];
end;

procedure TGffField.SetAsChar(const Value: Shortint);
begin
  Data.AsChar		:= Value;
  DoChange;
end;

(*------------------------------------------------------------------------------
property AsWord

------------------------------------------------------------------------------*)
function TGffField.GetAsWord: Word;
begin
  if IsString then
    Result		:= 0
  else
    Result		:= AsVariant[0];
end;

procedure TGffField.SetAsWord(const Value: Word);
begin
  Data.AsWord		:= Value;
  DoChange;
end;

(*------------------------------------------------------------------------------
property AsShort

------------------------------------------------------------------------------*)
function TGffField.GetAsShort: Smallint;
begin
  if IsString then
    Result		:= 0
  else
    Result		:= AsVariant[0];
end;

procedure TGffField.SetAsShort(const Value: Smallint);
begin
  Data.AsShort		:= Value;
  DoChange;
end;

(*------------------------------------------------------------------------------
property AsDWord

------------------------------------------------------------------------------*)
function TGffField.GetAsDWord: Longword;
begin
  if IsString then
    Result		:= 0
  else
    Result		:= AsVariant[0];
end;

procedure TGffField.SetAsDWord(const Value: Longword);
begin
  Data.AsDWord		:= Value;
  DoChange;
end;

(*------------------------------------------------------------------------------
property AsInt

------------------------------------------------------------------------------*)
function TGffField.GetAsInt: Longint;
begin
  if IsString then
    Result		:= 0
  else
    Result		:= AsVariant[0];
end;

procedure TGffField.SetAsInt(const Value: Longint);
begin
  Data.AsInt		:= Value;
  DoChange;
end;

(*------------------------------------------------------------------------------
property AsInt64

------------------------------------------------------------------------------*)
function TGffField.GetAsInt64: Int64;
begin
  if IsString then
    Result		:= 0
  else
    Result		:= AsVariant[0];
end;

procedure TGffField.SetAsInt64(const Value: Int64);
begin
  Data.AsInt64		:= Value;
  DoChange;
end;

(*------------------------------------------------------------------------------
property AsFloat

------------------------------------------------------------------------------*)
function TGffField.GetAsFloat: Single;
begin
  if IsString then
    Result		:= 0
  else
    Result		:= AsVariant[0];
end;

procedure TGffField.SetAsFloat(const Value: Single);
begin
  Data.AsFloat		:= Value;
  DoChange;
end;

(*------------------------------------------------------------------------------
property AsDouble

------------------------------------------------------------------------------*)
function TGffField.GetAsDouble: Double;
begin
  if IsString then
    Result		:= 0
  else
    Result		:= AsVariant[0];
end;

procedure TGffField.SetAsDouble(const Value: Double);
begin
  Data.AsDouble		:= Value;
  DoChange;
end;

(*------------------------------------------------------------------------------
property AsString

------------------------------------------------------------------------------*)
function TGffField.GetAsString: String;
var
  I			: Integer;
begin
  Result		:= '';
  case FType of
    gffFloat:
      Result		:= FloatToStrF(Data.AsFloat, ffFixed, 7, 7);
    gffDouble:
      Result		:= FloatToStrF(Data.AsDouble, ffFixed, 15, 15);
    gffVoid:
      for I := Low(Data.AsVoid) to High(Data.AsVoid) do
        Result		:= Result + Char(Data.AsVoid[I])
  else
    Result		:= AsVariant[''];
  end;
end;

procedure TGffField.SetAsString(const Value: String);
begin
  Data.AsString		:= Value;
  DoChange;
end;

(*------------------------------------------------------------------------------
property AsLocString

------------------------------------------------------------------------------*)
function TGffField.GetAsLocString: TGffLocString;
begin
  Result		:= Data.AsLocString;
end;

(*------------------------------------------------------------------------------
property AsResRef

------------------------------------------------------------------------------*)
function TGffField.GetAsResRef: String;
begin
  Result		:= AsVariant[''];
end;

procedure TGffField.SetAsResRef(const Value: String);
begin
  if Length(Value) < 17 then
    Data.AsResRef	:= Value
  else
    Data.AsResRef	:= Copy(Value, 1, 16);
  DoChange;
end;

(*------------------------------------------------------------------------------
property IsValid

------------------------------------------------------------------------------*)
function TGffField.GetIsValid: Boolean;
begin
  Result		:= FType in ValidGffVars;
end;

(*------------------------------------------------------------------------------
property IsField

------------------------------------------------------------------------------*)
function TGffField.GetIsField: Boolean;
begin
  Result :=
    ( FType in (SimpleGffVars + ComplexGffVars + [gffList]) ) or
    // Named Structs are "CAPREFs", and should be considered Fields
    ((FType = gffStruct) and (FLabel <> ''));
end;

(*------------------------------------------------------------------------------
property IsStruct

------------------------------------------------------------------------------*)
function TGffField.GetIsStruct: Boolean;
begin
  Result		:= FType = gffStruct;
end;

(*------------------------------------------------------------------------------
property IsList

------------------------------------------------------------------------------*)
function TGffField.GetIsList: Boolean;
begin
  Result		:= FType = gffList;
end;

(*------------------------------------------------------------------------------
property IsSimple

------------------------------------------------------------------------------*)
function TGffField.GetIsSimple: Boolean;
begin
  Result		:= FType in SimpleGffVars;
end;

(*------------------------------------------------------------------------------
property IsComplex

------------------------------------------------------------------------------*)
function TGffField.GetIsComplex: Boolean;
begin
  Result		:= FType in ComplexGffVars;
end;


(*------------------------------------------------------------------------------
property IsString

------------------------------------------------------------------------------*)
function TGffField.GetIsString: Boolean;
begin
  Result		:= FType in StringGffVars;
end;

(*------------------------------------------------------------------------------
property IsSterile

------------------------------------------------------------------------------*)
function TGffField.GetIsSterile: Boolean;
begin
  Result		:= FType in SterileGffVars;
end;

(*------------------------------------------------------------------------------
DoChange

------------------------------------------------------------------------------*)
procedure TGffField.DoChange;
begin
  //OnChange.Signal(self);
  { TODO 4: CHANGE: Split up TGffField with polymorphism,
    use a TGffRootField type to keep track of Modified / Changed,
    root field can't be deleted or modified, either.
  }
  if not Assigned(Root) then Exit;
  Root.Modified		:= True;
  Root.Changed		:= True;
end;

(*------------------------------------------------------------------------------
DoDelete

------------------------------------------------------------------------------*)
procedure TGffField.DoDelete;
begin
  //OnDelete.Signal(self);
  if not Assigned(Root) then Exit;
  Root.Modified		:= True;
  Root.Changed		:= True;
end;


(*------------------------------------------------------------------------------

	Public

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TGffField.Create(const AName: String; const AParent: TGffField);
begin
  FType			:= gffINVALID;
  FIsAnonymous		:= False;
  FLabel		:= AName;
  FLabelIndex		:= -1;
  FData			:= New(PGffVarData);

  //FOnChange		:= TMultiNotifyEvent.Create;
  //FOnDelete		:= TMultiNotifyEvent.Create;
  Modified		:= False;
  Changed		:= False;

  // Initialize members of Data
  FillChar(FData^, SizeOf(FData^), 0);
  // TODO 4: CHANGE: Optimize, create LocString only as necessary
  FData.AsLocString	:= TGffLocString.Create;

  if Assigned(AParent) then
    Parent		:= AParent;
end;

(*------------------------------------------------------------------------------
destructor

This additionally performs the necessary notifications to parent and siblings
and children, so that they are not left with pointers to data which no longer
exists.

------------------------------------------------------------------------------*)
destructor TGffField.Destroy;
begin
  DoDelete;
  Clear;
  Parent		:= nil;

  FreeAndNil(FData.AsLocString);
  Dispose(FData);

  //FreeAndNil(FOnChange);
  //FreeAndNil(FOnDelete);

  inherited;
end;

(*------------------------------------------------------------------------------
Assign

This performs a deep copy, but only for the sake of cloning the source, not
overwriting the source: so although this Field will have a pointer to the
Source's next sibling, previous sibling, etc., those siblings and parent will
*not* point back to this Field, they will continue to point to Source.

Care must be taken with any Field cloned as such.

Current notes:

Disabled until actually needed. (A dangerous function with no obvious immediate
purpose is better left unexposed.)

------------------------------------------------------------------------------*)
(*
function TGffField.Assign(const Source: TGffField): TGffField;
var
  I			: Integer;
begin
  FType			:= Source.FType;
  FName			:= Source.FName;
  FOffset		:= Source.FOffset;
  FStructId		:= Source.FStructId;
  FSpare		:= Source.FSpare;

  case FType of
    gffBYTE:
      AsByte		:= Source.AsByte;
    gffCHAR:
      AsChar		:= Source.AsChar;
    gffWORD:
      AsWord		:= Source.AsWord;
    gffSHORT:
      AsShort		:= Source.AsShort;
    gffDWORD:
      AsDWord		:= Source.AsDWord;
    gffINT4:
      AsInt		:= Source.AsInt;
    gffDWORD64, gffINT64:
      AsInt64		:= Source.AsInt64;
    gffFLOAT:
      AsFloat		:= Source.AsFloat;
    gffDOUBLE:
      AsDouble		:= Source.AsDouble;
    gffString:
      AsString		:= Source.AsString;
    gffResRef:
      AsResRef		:= Source.AsResRef;
    gffLocString:
      Data.AsLocString.Assign(Source.Data.AsLocString);
    gffVoid:
    begin
      SetLength(Data.AsVoid, Length(Source.Data.AsVoid));
      for I := 0 to Length(Data.AsVoid)-1 do
        Data.AsVoid[I]	:= Source.Data.AsVoid[I];
    end;
    gffStruct:
      Data.AsStruct	:= Source.Data.AsStruct;
    gffList:
      Data.AsList	:= Source.Data.AsList;
  end;

  FParent		:= Source.FParent;
  FFirstChild		:= Source.FFirstChild;
  FLastChild		:= Source.FLastChild;
  FNextSib		:= Source.FNextSib;
  FPrevSib		:= Source.FPrevSib;

end;
*)

(*------------------------------------------------------------------------------
LoadFromStream

The revised v4 version. We're back to using Seek, since that really is
much more efficient than copying, throwing around, and expanding multiple
memory stream.

LoadFromStream operates on a Struct, and all its child Fields. The Stream
should be set to the position of this Struct before LoadFromStream is
called.

The function requires that the Label Array has already been read, in advance,
and a TStringList has been populated with the values. This is more efficient
than constantly re-seeking through the Label Array.

------------------------------------------------------------------------------*)
// TODO 4: CHANGE: GffField.LoadFromStream needs more optimization
// TODO 3: ADD: Stream.Position of Error
function TGffField.LoadFromStream(
  const LoadLevel	: TGffLoadLevel;
  const Header		: TGffHeader;
  const Stream		: TStream;
  const Names		: TStringList
): TLetoError;
var
  ACount, I, L		: Cardinal;
  C			: Byte;
  Field, Struct		: TGffField;

  { Given the LoadLevel, is this a List that should be parsed? }
  function _ParseThis_: Boolean;
  var
    S			: String;
  begin
    S			:= Lowercase(Field.FLabel);
    Result :=
      (LoadLevel = LoadFull) or
      (
        (LoadLevel = LoadMedium) and
        (
          (S = 'combatinfo') or
          (S = 'classlist') or
          (S = 'featlist') or
          (S = 'skilllist')
        )
      );
  end;

begin

  { Read the Struct }
  FStructId		:= ReadLong(Stream);
  FOffset		:= ReadLong(Stream);
  ACount		:= ReadLong(Stream);

  { Read child offsets }
  if ACount = 1 then
    AddChild('UNLABELED').FOffset := FOffset
  else if ACount > 1 then begin
    Stream.Seek(Header.FieldIndicesOffset + FOffset, 0);
    for I := 0 to ACount-1 do
      AddChild('UNLABELED').FOffset := ReadLong(Stream);
  end;

  { Read Fields }

  Field			:= FirstChild;
  while Assigned(Field) do begin

    if Field.FOffset * 12 > Stream.Size then begin
      Result		:= Err_Bad_FieldIndex;
      Exit;
    end;

    Stream.Seek(Header.FieldOffset + (Field.FOffset * 12), 0);

    { Type }
    Field.FType		:= TGffVarType(ReadInt(Stream));

    { Label }
    Field.FLabelIndex	:= ReadInt(Stream); // Hmm, because of -1...
    if (Field.FLabelIndex < 0) or (Field.FLabelIndex > Names.Count-1) then begin
      Result		:= Err_Bad_LabelIndex;
      Exit;
    end;
    Field.FLabel	:= Names[Field.FLabelIndex];

    if Field.IsComplex then begin
      Field.FOffset	:= ReadLong(Stream);
      if Header.FieldDataOffset + Field.FOffset > Stream.Size then begin
        Result		:= Err_Bad_DataOffset;
        Exit;
      end;
      Stream.Seek(Header.FieldDataOffset + Field.Offset, 0);
    end;

    { Data }
    case Field.FType of

      gffBYTE, gffCHAR, gffWORD, gffSHORT, gffINT:
        Field.SetValue(ReadInt(Stream));

      gffDWORD:
        Field.SetValue(ReadLong(Stream));

      gffDWORD64, gffINT64:
        Stream.Read(Field.Data.AsInt64, 8);

      gffFLOAT:
        Stream.Read(Field.Data.AsFloat, 4);

      gffDOUBLE:
        Stream.Read(Field.Data.AsDouble, 8);

      gffString:
      begin
        Stream.Read(L, 4);
        Field.AsString := ReadString(Stream, L);
      end;

      gffResRef:
      begin
        Stream.Read(C, 1);
        Field.AsResRef := ReadString(Stream, C);
      end;

      gffLocString:
      begin
        L		:= Field.AsLocString.LoadFromStream(Stream);
        if L < 1 then begin
          Result	:= Err_Bad_LocString;
          Exit;
        end;
      end;

      gffVoid:
      begin
        L		:= ReadLong(Stream);
        SetLength(Field.Data.AsVoid, L);
        if L > 0 then
          for I := 0 to L-1 do
            Stream.Read(Field.Data.AsVoid[I], 1);
      end;

      { This is a "CAPREF" style Struct, being a child (Field) in a Struct }
      gffStruct:
      if _ParseThis_ then begin
        L		:= ReadLong(Stream);
        if L * 12 > Stream.Size then begin
          Result	:= Err_Bad_StructIndex;
          Exit;
        end;
        Stream.Seek(Header.StructOffset + (L * 12), 0);
        Result := Field.LoadFromStream(
          LoadLevel, Header, Stream, Names
        );
        if Result <> Success then Exit;
      end;

      gffList:
      if _ParseThis_ then begin
        L		:= ReadLong(Stream);
        if L > Stream.Size then begin
          Result	:= Err_Bad_ListIndex;
          Exit;
        end;
        Stream.Seek(Header.ListIndicesOffset + L, 0);
        L		:= ReadLong(Stream);
        if L > 0 then
          for I := 0 to L-1 do
            Field.AddChild('', gffStruct).FStructIndex :=
              ReadLong(Stream);
        Struct		:= Field.FirstChild;
        while Assigned(Struct) do begin
          L		:= Struct.FStructIndex;
          if L * 12 > Stream.Size then begin
            Result	:= Err_Bad_StructIndex;
            Exit;
          end;
          Stream.Seek(Header.StructOffset + (L * 12), 0);
          Result := Struct.LoadFromStream(
            LoadLevel, Header, Stream, Names
          );
          if Result <> Success then Exit;
          Struct	:= Struct.NextSibling;
        end;
      end;

    end; { case FType of }

    Field		:= Field.NextSibling;

  end;

  Result		:= Success;

end;

(*------------------------------------------------------------------------------
ReIndex

Re-indexing the complete file is necessary before any Save operation, because
certain parts of the stream must know *in advance* what the index of forward
Fields and Structs will be. This can't be done in-progress, because of the
recursive nature of the SaveToStream function.

ReIndex only needs to be called immediately prior to any call to SaveToStream.
The information it updates is not exposed publically, because the information
is only ever needed for the SaveToStream function itself.

------------------------------------------------------------------------------*)
procedure TGffField.ReIndex(var Tracker: TGffTracker);
var
  Field			: TGffField;
  Unindexed		: array of TGffField;
  I			: Cardinal;
begin
  Field			:= self;

  while Assigned(Field) do begin

    if Field.IsField then begin
      Field.FFieldIndex := Tracker.FieldIndex;
      Inc(Tracker.FieldIndex);
      if Field.IsList then
        Inc(Tracker.ListIndicesSize, (Field.Count * 4) + 4);
      { Complex types }
      case Field.FType of
        gffDWord64, gffInt64, gffDouble:
          Inc(Tracker.DataSize, 8);
        gffString:
          Inc(Tracker.DataSize, Length(Field.Data.AsString) + 4);
        gffResRef:
          Inc(Tracker.DataSize, Length(Field.Data.AsResRef) + 1);
        gffLocString:
          Inc(Tracker.DataSize, Field.Data.AsLocString.GetTotalSize);
        gffVoid:
          Inc(Tracker.DataSize, Length(Field.Data.AsVoid) + 4);
      end;
      { Label }
      if Field.FLabelIndex = -1 then begin
        if Field.Name = '' then
          Field.Name	:= 'NameMeDammit';
        SetLength(Unindexed, Length(Unindexed)+1);
        Unindexed[High(Unindexed)] := Field;
      end else begin
        if Cardinal(Field.FLabelIndex) >= Tracker.LabelIndex then
          Tracker.LabelIndex := Field.FLabelIndex + 1;
        if Length(Tracker.Names) < Field.FLabelIndex+1 then
          SetLength(Tracker.Names, Field.FLabelIndex + 100);
        if Tracker.Names[Field.FLabelIndex] = '' then
          Tracker.Names[Field.FLabelIndex] := Field.FLabel;
      end;

    end;

    if Field.IsStruct then begin
      Field.FStructIndex := Tracker.StructIndex;
      Inc(Tracker.StructIndex);
      if Field.Count > 1 then
        Inc(Tracker.FieldIndicesSize, Field.Count * 4);
    end;

    Field		:= Field.Next;
  end;

  { Handle -1 Label Index }
  I			:= Tracker.LabelIndex + Cardinal(Length(Unindexed));
  if Cardinal(Length(Tracker.Names)) < I then
    SetLength(Tracker.Names, I);
  if Length(Unindexed) > 0 then
    for I := 0 to High(Unindexed) do begin
      Field		:= Unindexed[I];
      Field.FLabelIndex	:= Tracker.LabelIndex;
      Tracker.Names[Tracker.LabelIndex] := Field.FLabel;
      Inc(Tracker.LabelIndex);
    end;

end;

(*------------------------------------------------------------------------------
SaveToStream

The revised v4 version. Saves this Struct (using this procedure on a Field
would result in a corrupt file structure) across several parts of the Stream,
using Seek to get back and forth (the Stream's Size has already been set
to the exact size necessary), and the Header and Tracker to identify where
to Seek to and where our current position is in several different parts of
the stream.

------------------------------------------------------------------------------*)
procedure TGffField.SaveToStream(
  const Stream		: TStream;
  const Header		: TGffHeader;
  var Tracker		: TGffTracker
);
var
  I, L			: Cardinal;
  B			: Integer;
  F			: Single;
  S			: String;
  Field, Child		: TGffField;
begin

  { Procedure is only designed for Structs }
  if FType <> gffStruct then
    Exit;

  { Record the Struct information }
  { From BioWare docs:
    Struct.Type
      Programmer-defined integer ID.
    Struct.DataOrDataOffset
      If Struct.FieldCount = 1, this is an index into the Field Array.
      If Struct.FieldCount > 1, this is a byte offset into the Field Indices
        array, where there is an array of DWORDs having a number of elements
        equal to Struct.FieldCount. Each one of these DWORDs is an index into
        the Field Array.
      [DS: If Struct.FieldCount = 0, this is 0xFFFFFFFF.]
    Struct.FieldCount
      Number of fields in this Struct.
  }
  if FCount = 1 then
    I			:= Tracker.FieldIndex
  else if FCount > 1 then
    I			:= Tracker.FieldIndicesOffset
  else
    I			:= $FFFFFFFF;
  Stream.Seek(Header.StructOffset + (Tracker.StructIndex * 12), 0);
  Stream.Write(FStructId, 4);
  Stream.Write(I, 4);
  Stream.Write(FCount, 4);

  { Field Indices (when Struct has multiple children) }
  if FCount > 1 then begin
    Stream.Seek(Header.FieldIndicesOffset + Tracker.FieldIndicesOffset, 0);
    Inc(Tracker.FieldIndicesOffset, FCount * 4);
    Child		:= FirstChild;
    while Assigned(Child) do begin
      Stream.Write(Child.FFieldIndex, 4);
      Child		:= Child.NextSibling;
    end;
  end;
  Inc(Tracker.StructIndex);

  { Record all Fields (recursively) }
  Field			:= FirstChild;
  while Assigned(Field) do begin

    if not Field.IsValid then begin
      Field		:= Field.NextSibling;
      Continue;
    end;

    Stream.Seek(Header.FieldOffset + (Tracker.FieldIndex * 12), 0);
    Inc(Tracker.FieldIndex);

    { Type }
    I			:= Cardinal(Field.FType);
    Stream.Write(I, 4);

    { Label }
    Stream.Write(Field.FLabelIndex, 4);

    { Data... }

    if Field.IsComplex then begin
      Stream.Write(Tracker.DataOffset, 4);
      Stream.Seek(Header.FieldDataOffset + Tracker.DataOffset, 0);
    end;

    case Field.FType of

      gffByte, gffChar, gffWord, gffShort, gffInt:
      begin
        B		:= Field.AsInt;
        Stream.Write(B, 4);
      end;

      gffDWord:
      begin
        I		:= Field.AsDWord;
        Stream.Write(I, 4);
      end;

      gffDWord64, gffInt64:
      begin
        Stream.Write(Field.Data.AsInt64, 8);
        Inc(Tracker.DataOffset, 8);
      end;

      gffFloat:
      begin
        F		:= Field.AsFloat;
        Stream.Write(F, 4);
      end;

      gffDouble:
      begin
        Stream.Write(Field.Data.AsDouble, 8);
        Inc(Tracker.DataOffset, 8);
      end;

      gffString:
      begin
        S		:= Field.Data.AsString;
        I		:= Length(S);
        Stream.Write(I, 4);
        Stream.Write(PChar(S)^, I);
        Inc(Tracker.DataOffset, 4 + I);
      end;

      gffResRef:
      begin
        S		:= Field.Data.AsResRef;
        I		:= Byte( Length(S) );
        Stream.Write(I, 1);
        Stream.Write(PChar(S)^, I);
        Inc(Tracker.DataOffset, 1 + I);
      end;

      gffLocString:
        Inc(
          Tracker.DataOffset,
          Field.AsLocString.SaveToStream(Stream)
        );

      gffVoid:
      begin
        I		:= Length(Field.Data.AsVoid);
        Stream.Write(I, 4);
        Inc(Tracker.DataOffset, 4 + I);
        if I > 0 then
          for L := 0 to I-1 do
            Stream.Write(Field.Data.AsVoid[L], 1);
      end;

      { "CAPREF" style Struct }
      gffStruct:
      begin
        Stream.Write(Tracker.StructIndex, 4);
        Field.SaveToStream(Stream, Header, Tracker);
      end;

      gffList:
      begin
        Stream.Write(Tracker.ListIndicesOffset, 4);
        Stream.Seek(Header.ListIndicesOffset + Tracker.ListIndicesOffset, 0);
        Stream.Write(Field.FCount, 4);
        Child		:= Field.FirstChild;
        while Assigned(Child) do begin
          Stream.Write(Child.FStructIndex, 4);
          Child		:= Child.NextSibling;
        end;
        Inc(Tracker.ListIndicesOffset, 4 + (Field.FCount*4));
        Child		:= Field.FirstChild;
        while Assigned(Child) do begin
          Child.SaveToStream(Stream, Header, Tracker);
          Child		:= Child.NextSibling;
        end;
      end;

    end;

    Field		:= Field.NextSibling;
  end;

end;

(*------------------------------------------------------------------------------
AddChild

------------------------------------------------------------------------------*)
function TGffField.AddChild(
  const AName		: String;
  const FieldType	: TGffVarType
): TGffField;
begin
  // This assigns the new child's parentage, and self's First/LastChild.
  Result		:= TGffField.Create(AName, self);
  Result.Root		:= Root;
  Result.FType		:= FieldType;
  DoChange;
end;

function TGffField.AddChild(
  const Child		: TGffField;
  const SameIndex	: Boolean;
  const AtPos		: Integer;
  const MakeCopy	: Boolean
): TGffField;
begin
  Result		:= nil;

  if
    not Assigned(Child) or
    (not IsList and not IsStruct) or
    (IsList and not Child.IsStruct) or
    (IsStruct and ((Name = '') or not Child.IsStruct))
  then
    Exit;

  if MakeCopy then
    Result		:= Child.CopyTo(self, SameIndex)
  else begin
    Child.Parent	:= self;
    Result		:= Child;
  end;

  if (AtPos > -1) and (AtPos < Count-1) then
    Result.Move(AtPos);

  DoChange;

end;

(*------------------------------------------------------------------------------
AddPrevSibling

Combines the utility of Parent.AddChild and Move, in one function. The Result
is inserted before this Field, as its new PrevSibling.

------------------------------------------------------------------------------*)
function TGffField.AddPrevSibling(
  const AName		: String;
  const FieldType	: TGffVarType
): TGffField;
var
  _Last			: TGffField;
begin
  Result		:= nil;
  if not Assigned(Parent) then Exit;

  _Last			:= Parent.LastChild;
  Result		:= TGffField.Create(AName, Parent);
  Result.Root		:= Root;
  Result.FType		:= FieldType;

  Parent.LastChild	:= _Last;
  _Last.NextSibling	:= nil;

  if Assigned(PrevSibling) then
    PrevSibling.NextSibling := Result
  else
    Parent.FirstChild	:= Result;

  Result.PrevSibling	:= PrevSibling;
  Result.NextSibling	:= self;
  PrevSibling		:= Result;

  DoChange;
end;

(*------------------------------------------------------------------------------
CopyFrom

------------------------------------------------------------------------------*)
procedure TGffField.CopyFrom(
  const Source		: TGffField;
  const SameIndex	: Boolean
);
var
  Child			: TGffField;
  I			: Integer;
begin

  if Source = self then Exit;

  FType			:= Source.FType;
  FIsAnonymous		:= Source.FIsAnonymous;
  FLabel		:= Source.FLabel;
  FOffset		:= Source.FOffset;
  FStructId		:= Source.FStructId;
  FSpare		:= Source.FSpare;

  //Root			:= Source.Root;

  if SameIndex then
    FLabelIndex		:= Source.FLabelIndex
  else
    FLabelIndex		:= -1;

  case FType of

    gffBYTE:
      AsByte		:= Source.AsByte;
    gffCHAR:
      AsChar		:= Source.AsChar;
    gffWORD:
      AsWord		:= Source.AsWord;
    gffSHORT:
      AsShort		:= Source.AsShort;
    gffDWORD:
      AsDWord		:= Source.AsDWord;
    gffINT:
      AsInt		:= Source.AsInt;
    gffINT64, gffDWORD64:
      AsInt64		:= Source.AsInt64;
    gffFLOAT:
      AsFloat		:= Source.AsFloat;
    gffDOUBLE:
      AsDouble		:= Source.AsDouble;
    gffString:
      AsString		:= Source.AsString;
    gffLocString:
      AsLocString.CopyFrom(Source.AsLocString);
    gffResRef:
      AsResRef		:= Source.AsResRef;

    gffVoid:
    begin
      I			:= Length(Source.Data.AsVoid);
      SetLength(Data.AsVoid, I);
      for I := 0 to I-1 do
        Data.AsVoid[I]	:= Source.Data.AsVoid[I];
    end;

  end;

  Child			:= Source.FirstChild;
  while Assigned(Child) do begin
    AddChild('').CopyFrom(Child, SameIndex);
    Child		:= Child.NextSibling;
  end;

end;

(*------------------------------------------------------------------------------
CopyTo

------------------------------------------------------------------------------*)
function TGffField.CopyTo(
  const ToParent	: TGffField;
  const SameIndex	: Boolean
): TGffField;
begin
  Result		:= TGffField.Create(Name, ToParent);
  Result.CopyFrom(self, SameIndex);
end;

(*------------------------------------------------------------------------------
Move

------------------------------------------------------------------------------*)
procedure TGffField.Move(const ToIndex: Integer);
var
  I			: Integer;
  Place			: TGffField;
begin
  if
    (ToIndex < 0) or
    not Assigned(Parent) or
    (ToIndex > Parent.Count-1)
  then
    Exit;

  I			:= 0;
  Place			:= Parent.FirstChild;

  while Assigned(Place) do begin
    if I = ToIndex then Break;
    Inc(I);
    Place		:= Place.NextSibling;
  end;

  if not Assigned(Place) or (Place = self) then Exit;

  // Move out

  if Assigned(PrevSibling) then
    PrevSibling.NextSibling := NextSibling;
  if Assigned(NextSibling) then
    NextSibling.PrevSibling := PrevSibling;

  // Move in

  PrevSibling		:= Place.PrevSibling;
  NextSibling		:= Place;

  if Assigned(PrevSibling) then
    PrevSibling.NextSibling := self;
  Place.PrevSibling	:= self;

  if not Assigned(PrevSibling) then
    Parent.FirstChild	:= self;
  if not Assigned(NextSibling) then
    Parent.LastChild	:= self;

  DoChange;

end;

(*------------------------------------------------------------------------------
Delete

------------------------------------------------------------------------------
procedure TGffField.Delete;
begin
  Destroy;
end;
*)

(*------------------------------------------------------------------------------
Clear

------------------------------------------------------------------------------*)
procedure TGffField.Clear;
begin
  if not Assigned(LastChild) then Exit;

  while Assigned(LastChild) do
    FreeAndNil(LastChild);

  DoChange;
end;

(*------------------------------------------------------------------------------
GetAttrib

For use with the Find syntax that allows returning attributes of a Field,
not just the Field's value.

------------------------------------------------------------------------------*)
function TGffField.GetAttrib(const Attr: String; var Value: String): Boolean;
var
  _Next, _Prev		: TGffField;
begin

  Result		:= IsValid;
  if not Result then
    Exit;

  { These can be costly functions. Do them once, here, instead of
    multiple times, below. }
  _Next			:= Next;
  _Prev			:= Previous;

  if (Attr = 'count') or (Attr = 'len') or (Attr = 'length') then begin
    if VarType = gffLocString then
      Value		:= IntToStr(AsLocString.Count)
    else if VarType = gffVoid then
      Value		:= IntToStr(Length(Data.AsVoid))
    else
      Value		:= IntToStr(Count);
  end
  else if Attr = 'haschildren' then begin
    if VarType = gffLocString then
      Value		:= BoolToStr(AsLocString.Count > 0)
    else
      Value		:= BoolToStr(HasChildren);
  end
  else if Attr = 'haslangs' then begin
    if VarType = gffLocString then
      Value		:= BoolToStr(AsLocString.Count > 0)
    else
      Result		:= False;
  end
  else if Attr = 'strref' then begin
    if VarType = gffLocString then
      Value		:= IntToStr(AsLocString.StringRef)
    else
      Result		:= False;
  end
  else if Attr = 'value' then
    Value		:= AsString
  else if Attr = 'type' then
    Value		:= IntToStr( Floor(Power(2, Integer(VarType))) )
  else if Attr = 'typestr' then
    Value		:= FieldTypeToStr(VarType)
  else if Attr = 'name' then begin
    if Name <> '' then
      Value		:= Name
    else
      Value		:= IntToStr(Id)
  end
  else if Attr = 'index' then
    Value		:= IntToStr(Index)
  else if Attr = 'depth' then
    Value		:= IntToStr(Depth)
  else if Attr = 'id' then
    Value		:= IntToStr(Id)
  else if Attr = 'path' then
    Value		:= Path
  else if Attr = 'hasparent' then
    Value		:= BoolToStr(Assigned(Parent), True)
  else if (Attr = 'parent') and Assigned(Parent) then
    Value		:= Parent.Path
  else if (Attr = 'parentindex') and Assigned(Parent) then
    Value		:= IntToStr(Parent.Index)
  else if (Attr = 'firstchild') and Assigned(FirstChild) then
    Value		:= FirstChild.Path
  else if (Attr = 'lastchild') and Assigned(LastChild) then
    Value		:= LastChild.Path
  else if (Attr = 'next') and Assigned(_Next) then
    Value		:= _Next.Path
  else if (Attr = 'previous') and Assigned(_Prev) then
    Value		:= _Prev.Path
  else if Attr = 'hasnext' then
    Value		:= BoolToStr(Assigned(_Next), True)
  else if Attr = 'hasprevious' then
    Value		:= BoolToStr(Assigned(_Prev), True)
  else if (Attr = 'nextsibling') and Assigned(NextSibling) then
    Value		:= NextSibling.Path
  else if (Attr = 'prevsibling') and Assigned(PrevSibling) then
    Value		:= PrevSibling.Path
  else if (Attr = 'hasnextsib') then
    Value		:= BoolToStr(Assigned(NextSibling), True)
  else if (Attr = 'hasprevsib') then
    Value		:= BoolToStr(Assigned(PrevSibling), True)

  else
    Result		:= False;

end;

function TGffField.GetAttrib(
  const Attr, Key	: String;
  var Value		: String
): Boolean;
var
  I, L			: Integer;
begin
  if Key = '' then begin
    Result		:= GetAttrib(Attr, Value);
    Exit;
  end;

  Result		:= False;

  case VarType of

    gffLocString:
    if Attr = 'lang' then begin
      Result		:= TryStrToInt(Key, I);
      if Result then
        Value		:= AsLocString.GetByLang(I);
    end;

    gffVoid:
    if Attr = 'byte' then begin
      Result		:= TryStrToInt(Key, I);
      L			:= Length(Data.AsVoid);
      if I < 0 then
        I		:= L + I;
      if Result and (I < L) and (I > -1) then
        Value		:= IntToStr(Data.AsVoid[I]);
    end;

  end;

end;

(*------------------------------------------------------------------------------
SetAttrib

And for setting a Field by attribute. Not everything in GetAttrib is
supported, here. The result will be False if either the attrib is invalid,
or it cannot be set.

------------------------------------------------------------------------------*)
function TGffField.SetAttrib(const Attr, Value: String): Boolean;
begin

  Result		:= IsValid;
  if not Result then
    Exit;

  if Attr = 'strref' then begin
    if VarType = gffLocString then
      AsLocString.StringRef := StrToInt64Def(Value, NO_STRREF)
    else
      Result		:= False;
  end
  else if (Attr = 'len') or (Attr = 'length') then begin
    if VarType = gffVoid then
      SetLength( Data.AsVoid, StrToIntDef(Value, Length(Data.AsVoid)) )
    else
      Result		:= False;
  end
  else if Attr = 'value' then
    SetValue(Value)
  // Experimental!
  else if Attr = 'type' then
    Result		:= TryBFTtoFT( StrToIntDef(Value, 0), FType )
  else if Attr = 'typestr' then
    Result		:= TryStrToFieldType(Value, FType)
  else if Attr = 'name' then
    Name		:= Value
  else if Attr = 'id' then
    Id			:= StrToInt64Def(Value, 0)

  else
    Result		:= False;

end;

function TGffField.SetAttrib(
  const Attr, Key,
    Value		: String;
  const NilKey		: Boolean
): Boolean;
var
  I, L			: Integer;
begin
  if Key = '' then begin
    Result		:= SetAttrib(Attr, Value);
    Exit;
  end;

  Result		:= False;

  case VarType of

    gffLocString:
    if Attr = 'lang' then begin
      Result		:= TryStrToInt(Key, I);
      if not Result then
      // TODO 4: CHANGE: Use undef to remove Lang? (Presently, blank string does it - which means no blank langs)
      else if NilKey then
        AsLocString.DeleteLang(I)
      else
        AsLocString.SetByLang(I, Value);
    end;

    gffVoid:
    if Attr = 'byte' then begin
      Result		:= TryStrToInt(Key, I);
      L			:= Length(Data.AsVoid);
      if I < 0 then
        I		:= L + I;
      if not Result or (I > L-1) or (I < 0) then
      else if Value = '' then
        Data.AsVoid[I]	:= 0
      else
        Data.AsVoid[I]	:= Ord( Value[1] );
    end;

  end;

end;

(*------------------------------------------------------------------------------
GetNames

This is the .Names attribute. It only works for Structs; Fields which have
named children. The Result is a list of those names, non-recursive.

------------------------------------------------------------------------------*)
function TGffField.GetNames: TStringList;
var
  Node			: TGffField;
begin
  Result		:= TStringList.Create;

  Node			:= FirstChild;
  while Assigned(Node) do begin
    Result.Add(Node.Name);
    Node		:= Node.NextSibling;
  end;
end;

(*------------------------------------------------------------------------------
Find

Search for a Field named as indicated. If Recursive is True (default) all
children of this Struct/Field (and their children) will be searched, as well.
Performing a recursive Find on the Root Struct of a file will search the
entire file (so to speak).

Name is case-insensitive.

Name can contain the following syntax to assist in locating a field downstream
from a List, or returning a specific Struct child of the List:

[n]
Return the n'th child. [0] returns the first child, for instance. If n is
less than 0, it is treated as 0. If n is beyond the range of children, no
result is nil.

[_]
Return the last child. (Technically, -1 can also be used, though this is
an "unpublished" part of the spec.)

[*]
Search all children for a match. This is the default behavior. If any other
character or sequence appears between the brackets (than those listed above),
it is treated as *.

The Recursive toggle used in conjunction with the above syntax indicates
a recursive search should be run beneath only the indicated Struct (unless
* is used, of course).

Find can only return a single Field (or Struct), it cannot return
multiple results, and hence has no wildcard syntax.

If no child is found with the indicated name, the Result is nil.

------------------------------------------------------------------------------*)
function TGffField.Find(
  const Name		: String;
  const FieldType	: TGffVarType;
  const Recursive	: Boolean
): TGffField;
var
  Node			: TGffField;
  I, LBracket, RBracket	: Integer;
  S			: String;
  Index			: Integer;
begin
  Result		:= nil;

  if (Name = '') or
    not Assigned(FirstChild) or
    not( FieldType in ValidGffVars + [gffANY] )
  then Exit;

  LBracket		:= Pos('[', Name);
  RBracket		:= Pos(']', Name);
  if (LBracket > 0) and (RBracket > LBracket) then
    S			:= Copy(Name, LBracket+1, RBracket-LBracket-1);
  if S = '_' then
    S			:= '-1';

  { Search only one child }
  if HasChildren and TryStrToInt(S, Index) then begin
    { '*', and any other unrecognized syntax, invokes normal search }
    { Lower bounds is actually 0... }
    if Index < -1 then Index := 0;
    { ... -1, [_], conversion }
    if Index = -1 then Index := Count-1;
    { If beyond upper bounds, return nil }
    if Index > Count-1 then Exit;
    { Bounds-check complete, now search the given child }
    Result		:= FirstChild;
    for I := 1 to Index do
      Result		:= Result.NextSibling;
    {	This is what is being sought }
    S			:= Lowercase( Copy(Name, 1, LBracket-1) );
    {	Return the child itself when only brackets were given }
    if S = '' then Exit;
    {	Else, start searching from the first Field in that Struct }
    Result		:= Result.FirstChild;
  end

  { Search all immediate children }
  else begin
    { Default [*] behavior }
    if IsList and HasChildren then
      Result		:= FirstChild.FirstChild
    else
      Result		:= FirstChild;
    if S = '' then
      S			:= Lowercase(Name)
    else
      S			:= Lowercase( Copy(Name, 1, LBracket-1) );
  end;

  while Assigned(Result) do begin
    if (Lowercase(Result.FLabel) = S) and
      ((FieldType = gffANY) or (Result.FType = FieldType))
    then Break;
    Result		:= Result.NextSibling;
  end;

  if Assigned(Result) or not Recursive then Exit;

  { Search recursively }
  Node			:= FirstChild;
  while Assigned(Node) and not Assigned(Result) do begin
    Result		:= Node.Find(Name, FieldType);
    Node		:= Node.NextSibling;
  end;

end;

(*------------------------------------------------------------------------------
Match

A much more powerful (and correspondingly not as fast) version of Find,
which accepts a plethora of parameters to control how and when a Field is
matched, using the TGffMatchParam record type. Match is also capable of
returning multiple results. The Results of the match are given in the
Results field of Params. This is a TList. Every item will be a pointer
to a Field. The caller is responsible for Creating and Destroying this list.
The caller is also responsible for observing these Fields post-match, as
they may be deleted at any time.

The values in the Params record are as follows:

Name
The name of the Field to search for. Use '*' to match any name; wildcards
are supported in every fashion. Name is never case-sensitive.
  CAVEAT: The caller MUST Lowercase Params.Name. This prevents Match
  from having to re-lowercase it for every single pass. TGffFile.Match does so.

Value
A Field must have the given value (when converted to a string) in order to
match. Value may contain wildcards. Use '*' to match any value.
  CAVEAT: The caller MUST Lowercase Params.Value if mbCIValue is being
  set. This prevents Match from having to re-lowercase it for every
  single pass. TGffFile.Match does so.

Lang
Is only useful in conjunction with Value, and when Types includes LocString.
For the Field to match, the Language specified must have the value specified.
Use -1 to match any Language (the default).

Types
A Field must have the indicated GffVarType in order to match. To match any
type, simply use a set of [gffANY]. Otherwise, the set is treated as an
or-list; e.g., [gffString, gffResref] would match a Field of *either* type
gffString or gffResref. Note that if Types includes gffANY, all other
members of the set are ignored.

NotTypes
A Field must *not* be any of these types to match. A complex filter can be
designed by combining Types and NotTypes. To filter for only Fields of a
particular type, include that list in Types, and use an empty set for
NotTypes. To match any Field except those of particular types, use [gffANY]
for Types and fill in NotTypes with the types the Field should not be.

Depth
Search only to a certain recursion depth. 0 indicates only the children
of this Field. 1 would indicate children of this Field's children may be
searched. Use -1 for indefinite recursion.

Count
Find the nth instance (default is first, which is 1, not 0). Reverse order
instancing (-1 for last instance) is not yet supported. If Count exceeds the
number of matches, no result is returned. This is not particularly optimal;
in a loop finding every matching instance (by incrementing Count),
Match will start over from the beginning of its children every time.

Behavior
This is a set of additional specifiers that modify how the match is executed,
*overall*. It is a set which may include the following:

  mbCIValue
  Value will be matched case-insensitively. (The default is case-sensitive.)

  mbCountOnly
  All possible matches will be found, and the Count field in the record will
  be set to the number matched. (The Result will be the last Field found,
  or nil when Count is 0). This precludes the use of Count to find the
  nth instance matching (which will not apply, since all instances are
  found), however, Count will *not* be reset before Match executes, only
  incremented for every match. (This may be useful, but to get an absolute
  count, initialize Count to 0 before executing Match.)

  mbNoChildCheck
  The alternative to mbMustHaveChild and mbMustHaveNoChild is mbNoChildCheck.
  Match doesn't care whether a Field has children or not with this behavior.

  mbMustHaveChild
  A Field must have children in order to match. For Lists and Structs, this
  means at least one child is actually present. For a LocString, this means
  there is at least one LangSpec.

  mbMustHaveNoChild
  A Field must *not* have any children, either because it is a type that does
  not have children, or is a List/Struct with no children, or is a LocString
  with no LangSpecs.

  Multiple mb*Child* behaviorisms will cause unpredictable results.

Results
A TList of all matching Fields. The original caller must initialize and
finalize (TGffFile.Match does not do so).

(The remaining members of TMatchBehavior are for internal use.)

------------------------------------------------------------------------------*)
procedure TGffField.Match(
  var Params		: TGffMatchParams
);
var
  Node, Next		: TGffField;
  ChildCheck, HaveChild	: Boolean;
  Matches		: Boolean;
begin
  if not Assigned(Params.Results) then Exit;

  ChildCheck		:= not(mbNoChildCheck in Params.Behavior);
  HaveChild :=
    ChildCheck and
    (mbMustHaveChild in Params.Behavior) and
    not(mbMustHaveNoChild in Params.Behavior);

  Node			:= FirstChild;

  while Assigned(Node) do begin

    if Assigned(Params.OnMatch) then
      Params.OnMatch(self, Node, Matches)
    else
      Matches		:= Node.TryMatch(Params, ChildCheck, HaveChild);

    if not Matches then
    else if Params.Target > 1 then
      Dec(Params.Target)
    else begin
      Params.Results.Add(Node.Path);
      Dec(Params.Target);
    end;

    { Replace }
    if Matches and (mbReplace in Params.Behavior) then begin

      { Delete }
      if mbDeleteParent in Params.Behavior then begin
        Include(Params.Behavior, mbDeleteMe);
        Exit;
      end
      else if mbDelete in Params.Behavior then begin
        Next		:= Node.NextSibling;
        FreeAndNil(Node);
        DoChange;
        Node		:= Next;
        if Params.Target = 0 then
          Exit
        else
          Continue;
      end;

      { SetType }
      if Params.SetType <> gffINVALID then begin
        Node.FType	:= Params.SetType;
        DoChange;
      end;

      { SetLang }
      if
        (Params.SetLang <> -1) and
        (Node.FType = gffLocString) and
        not Node.AsLocString.HasLang(Params.SetLang)
      then begin
        Node.AsLocString.Language[Params.Lang] := Params.SetLang;
        DoChange;
      end;

      { SetStrRef }
      if
        (Node.FType = gffLocString) and
        (mbSetStrRef in Params.Behavior)
      then begin
        Node.AsLocString.StringRef := Params.SetStrRef;
        DoChange;
      end;

      { SetValue }
      if mbSetValue in Params.Behavior then
        Node.SetValue(Params.SetValue, Params.Lang);

    end;

    if Params.Target = 0 then Exit;

    { Recurse down }
    if Params.Depth <> 0 then begin
      Dec(Params.Depth);
      Node.Match(Params);
      Inc(Params.Depth);
    end;

    { Child wanted this parent deleted }
    if mbDeleteMe in Params.Behavior then begin
      Exclude(Params.Behavior, mbDeleteMe);
      Next		:= Node.NextSibling;
      FreeAndNil(Node);
      DoChange;
      Node		:= Next;
      if Params.Target = 0 then
        Exit
      else
        Continue;
    end;

    if Params.Target = 0 then Exit;

    Node		:= Node.NextSibling;

  end;

end;

(*------------------------------------------------------------------------------
Next

Get the next (absolute) field. This will be the first child of this field,
if it exists. If not, it's the next sibling of this field. If not, it
will be the closest next sibling of an ancestor. If this is the last field,
Result is nil.

------------------------------------------------------------------------------*)
function TGffField.Next: TGffField;
var
  Node			: TGffField;
begin
  Node			:= Parent;
  Result		:= nil;

  // Try first child
  if Assigned(FirstChild) then
    Result		:= FirstChild

  // Try immediate next sibling
  else if Assigned(NextSibling) then
    Result		:= NextSibling

  // Find the first upstream instance of a parent with next sibling
  else
    while (Result = nil) and (Node <> nil) do begin
      Result		:= Node.NextSibling;
      Node		:= Node.Parent;
    end;

end;

(*------------------------------------------------------------------------------
Previous

Get the previous (absolute) field. This will be either the last child of the
previous sibling, or the parent. If this is the first field, Result is nil.

------------------------------------------------------------------------------*)
function TGffField.Previous: TGffField;
var
  Node			: TGffField;
begin
  Result		:= PrevSibling;
  // If I'm the first child, result is Parent
  if not Assigned(Result) then
    Result		:= FParent
  // Result should be the last child of the last child of the prev sib
  else begin
    Node		:= Result;
    repeat
      Result		:= Node;
      Node		:= Result.LastChild;
    until Node = nil;
  end;
end;

(*------------------------------------------------------------------------------
ChildOfIndex

------------------------------------------------------------------------------*)
function TGffField.ChildOfIndex(const Index: Cardinal): TGffField;
var
  I			: Cardinal;
begin
  Result		:= nil;
  if (Count = 0) or (Index > Cardinal(Count)-1) then
    Exit;
  Result		:= FirstChild;
  I			:= 0;
  while Assigned(Result) and (I < Index) do begin
    Result		:= Result.NextSibling;
    Inc(I);
  end;
end;

(*------------------------------------------------------------------------------
Setalue (Integer)

The calling function wants to abstractly set the value of this Field,
without having a lengthy case statement to determine what the Field's type
is. So it passes an Integer, and this function assigns the correct
record field to that value.

This function does not provide unlimited casting, only for those Field
types that make sense.

------------------------------------------------------------------------------*)
procedure TGffField.SetValue(
  const Value		: Integer;
  const FieldType	: TGffVarType
);
begin
  if FieldType <> gffUNSPECIFIED then
    FType		:= FieldType;

  case FType of
    gffBYTE:	AsByte	:= Byte(Value);
    gffCHAR:	AsChar	:= Shortint(Value);
    gffWORD:	AsWord	:= Word(Value);
    gffSHORT:	AsShort	:= Smallint(Value);
    gffDWORD:	AsDWord	:= Longword(Value);
    gffINT:	AsInt	:= Value;

    gffDWORD64, gffINT64:
      AsInt64		:= Int64(Value);

    gffString:	AsString := IntToStr(Value);
    gffResRef:	AsResRef := IntToStr(Value);

    gffLocString:
    begin
      AsLocString.First := IntToStr(Value);
      DoChange;
    end;

  end;

end;

(*------------------------------------------------------------------------------
SetValue (Int64)

This version actually handles three types: DWORD64 and INT64, of course,
but it also handles DWORDs, since their values can go beyond the bounds
of Integer (like, say, 3 billion), and having a separate function just
for one type would be wasteful.

The string types are also supported, FWIW.

------------------------------------------------------------------------------*)
procedure TGffField.SetValue(
  const Value		: Int64;
  const FieldType	: TGffVarType
);
begin
  if FieldType <> gffUNSPECIFIED then
    FType		:= FieldType;

  case FType of
    gffDWORD:
      AsDword		:= Value;
    gffDWORD64, gffINT64:
      AsInt64		:= Value;
    gffString:
      AsString		:= IntToStr(Value);
    gffResRef:
      AsResRef		:= IntToStr(Value);
    gffLocString:
    begin
      AsLocString.First := IntToStr(Value);
      DoChange;
    end;
  end;

end;

(*------------------------------------------------------------------------------
SetValue (Real)

For setting a Field of type FLOAT or DOUBLE.

The string types are also supported, FWIW.

------------------------------------------------------------------------------*)
procedure TGffField.SetValue(
  const Value		: Real;
  const FieldType	: TGffVarType
);
begin
  if FieldType <> gffUNSPECIFIED then
    FType		:= FieldType;

  case FType of
    gffFLOAT:
      AsFloat		:= Value;
    gffDOUBLE:
      AsDouble		:= Value;
    gffString:
      AsString		:= FloatToStrF(Value, ffFixed, 15, 15);
    gffResRef:
      AsResRef		:= FloatToStrF(Value, ffFixed, 7, 7);
    gffLocString:
    begin
      AsLocString.First := FloatToStrF(Value, ffFixed, 15, 15);
      DoChange;
    end;
  end;

end;

(*------------------------------------------------------------------------------
SetValue (String)

This is a big bulky beast of a proc, because it acts as a central location
for anonymously setting the value of a Field - since most often, user input
will be in the form of a string.

------------------------------------------------------------------------------*)
procedure TGffField.SetValue(
  const Value		: String;
  const Lang		: Integer;
  const FieldType	: TGffVarType
);
var
  TryInt		: Integer;
  Try64			: Int64;
  TryFloat		: Double;
begin
  if FieldType <> gffUNSPECIFIED then
    FType		:= FieldType;

  case FType of

    // The string types themselves
    gffString:
      AsString		:= Value;
    gffResRef:
      AsResRef		:= Value;
    gffLocString:
    begin
      if Lang = -1 then
        AsLocString.First := Value
      else
        AsLocString[Lang] := Value;
      DoChange;
    end;

    // These all need casting
    gffBYTE, gffCHAR, gffWORD, gffSHORT, gffINT:
      if TryStrToInt(Value, TryInt) then
        SetValue(TryInt);

    gffDWORD, gffDWORD64, gffINT64:
      if TryStrToInt64(Value, Try64) then
        SetValue(Try64);

    gffFLOAT, gffDOUBLE:
      if TryStrToFloat(Value, TryFloat) then
        SetValue(TryFloat);

  end;

end;

(*------------------------------------------------------------------------------
SetValue (Variant)

This version is handy when the calling function wants to pass the value
of an existing Field - which can be done anonymously with the AsVariant
property of that Field.

All it does is convert the variant to a string, so that the all-inclusive
functionality of the above version can be leveraged.

------------------------------------------------------------------------------*)
procedure TGffField.SetValue(
  const Value		: Variant;
  const FieldType	: TGffVarType;
  const Lang		: Integer
);
var
  S			: String;
begin
  S			:= Value;
  SetValue(S, Lang, FieldType);
end;

(*------------------------------------------------------------------------------
Rename

This behaves exactly as if you'd deleted the Field and re-created it, with
a new name. If no other Fields are pointing to the old name, it becomes
orphaned. The new name (Label) is unique; even if another Field already has
the same Label, this Field will not try to 'borrow' the existing LabelIndex.

Rename does not allow names with the characters /[] because these are part
of the internal syntax Leto uses to look up Fields. The Result of Rename
is False if Newname contains any of these characters.

The Advanced Editor should be used to micromanage LabelIndex if necessary.

------------------------------------------------------------------------------*)
function TGffField.Rename(const Newname: String): Boolean;
var
  I			: Cardinal;
begin
  Result		:= Newname <> '';

  if not Result or (Lowercase(Newname) = Lowercase(FLabel)) then Exit;

  for I := 1 to Length(Newname) do
    Result		:= Result and not(Newname[I] in ['/', '[', ']']);

  if not Result then Exit;

  FLabelIndex		:= -1;
  FLabel		:= Newname;

  DoChange;

end;


{ TGffLocString }


(*------------------------------------------------------------------------------

	Private

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
property ByLang

------------------------------------------------------------------------------*)
function TGffLocString.GetByLang(const Lang: Integer): String;
begin
  if Lang < 0 then
    Result		:= First
  else
    Result		:= FLangs.Values[IntToStr(Lang)];
end;

procedure TGffLocString.SetByLang(const Lang: Integer; const Value: String);
begin
  if Lang < 0 then
    First		:= Value
  else
    FLangs.Values[IntToStr(Lang)] := Value
end;

(*------------------------------------------------------------------------------
property ByIndex

------------------------------------------------------------------------------*)
function TGffLocString.GetByIndex(const Index: Cardinal): String;
begin
  if (Count = 0) or (Index > Count-1) then
    Result		:= ''
  else
    Result		:= FLangs.ValueFromIndex[Index];
end;

procedure TGffLocString.SetByIndex(const Index: Cardinal; const Value: String);
begin
  if (Count = 0) or (Index > Count-1) then
    Exit
  else
    FLangs.ValueFromIndex[Index] := Value;
end;

(*------------------------------------------------------------------------------
property Language

------------------------------------------------------------------------------*)
function TGffLocString.GetLanguage(const Index: Cardinal): Integer;
begin
  if (Count = 0) or (Index > Count-1) then
    Result		:= -1
  else
    Result		:= StrToIntDef(FLangs.Names[Index], -1);
end;

procedure TGffLocString.SetLanguage(
  const Index		: Cardinal;
  const Value		: Integer
);
begin
  if (Value < 0) or (Index > Count-1) or HasLang(Value) then
    Exit
  else
    FLangs[Index] := IntToStr(Value) + '=' + FLangs.ValueFromIndex[Index];
end;

(*------------------------------------------------------------------------------
property Count

------------------------------------------------------------------------------*)
function TGffLocString.GetCount: Cardinal;
begin
  Result		:= FLangs.Count;
end;

(*------------------------------------------------------------------------------
property First

------------------------------------------------------------------------------*)
function TGffLocString.GetFirst: String;
begin
  Result		:= ByIndex[0];
end;

procedure TGffLocString.SetFirst(const Value: String);
begin
  ByIndex[0]		:= Value;
end;

(*------------------------------------------------------------------------------
GetTotalSize

Used exclusively by TGffField.ReIndex. Determines the total size in bytes
of this LocString as it will be written to Stream (which is more than just
the size of all the strings added together). Calling GetTotalSize sets
FTotalSize, which the LocString then re-uses in its own SaveToStream, so
that it doesn't have to re-calculate.

------------------------------------------------------------------------------*)
function TGffLocString.GetTotalSize: Cardinal;
var
  C, L			: Cardinal;
begin
  Result		:= 12;
  C			:= Count;
  if C > 0 then
    for L := 0 to C-1 do
      Inc( Result, 8 + Length(ByIndex[L]) );
  FTotalSize		:= Result;
end;


(*------------------------------------------------------------------------------

	Public

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
constructors

------------------------------------------------------------------------------*)
constructor TGffLocString.Create;
begin
  StringRef		:= NO_STRREF;
  FLangs		:= TStringList.Create;
end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TGffLocString.Destroy;
begin
  FreeAndNil(FLangs);
  inherited;
end;

(*------------------------------------------------------------------------------
LoadFromStream

The Result is the total number of bytes read from the Stream, which is
four bytes plus the value of the "total length", contained in those first
four bytes.

------------------------------------------------------------------------------*)
function TGffLocString.LoadFromStream(const Stream: TStream): Cardinal;
var
  Len, Lang, I		: Cardinal;
  ACount		: Cardinal;
  S			: String;
begin

  Result		:= 0;

  if not Assigned(Stream) then
    Exit;

  Len			:= ReadLong(Stream);
  StringRef		:= ReadLong(Stream);
  ACount		:= ReadLong(Stream);

  if ACount > 0 then
    for I := 0 to ACount-1 do begin
      Lang		:= ReadLong(Stream);
      S			:= ReadString(Stream, ReadLong(Stream));
      Add(S, Lang);
    end;

  Result		:= Len + 4;

end;

(*------------------------------------------------------------------------------
SaveToStream

The Result is the number of bytes written out to the Stream, which includes
the four bytes written for the "total length" of the LocString.

------------------------------------------------------------------------------*)
function TGffLocString.SaveToStream(const Stream: TStream): Cardinal;
var
  C			: Cardinal;
  L			: Cardinal;
  Lang			: Cardinal;
  Len			: Cardinal;
begin

  Result		:= 0;

  if not Assigned(Stream) then
    Exit;

  C			:= Count;
  Result		:= FTotalSize;
  L			:= Result - 4;

  { Total len }
  Stream.Write(L, 4);

  { StringRef }
  Stream.Write(StringRef, 4);

  { LangSpecCount }
  Stream.Write(C, 4);

  { Languages }
  if C > 0 then
    for L := 0 to C-1 do begin
      Lang		:= Language[L];
      Len		:= Length(ByIndex[L]);
      Stream.Write(Lang, 4);
      Stream.Write(Len, 4);
      Stream.Write(PChar(ByIndex[L])^, Len);
    end;

end;

(*------------------------------------------------------------------------------
Add

------------------------------------------------------------------------------*)
procedure TGffLocString.Add(
  const Text		: String;
  const Lang		: Integer
);
begin
  if FLangs.Count = 256 then
    raise ERangeError.CreateFmt(
      'Only a maximum of %d languages in any LocString is supported.',
      [256]
    );

  FLangs.Values[IntToStr(Lang)] := Text;

end;

(*------------------------------------------------------------------------------
Delete

------------------------------------------------------------------------------*)
function TGffLocString.Delete(const Index: Cardinal): Boolean;
begin
  Result		:= Index < Count;
  if Result then
    FLangs.Delete(Index);
end;

(*------------------------------------------------------------------------------
DeleteLang

------------------------------------------------------------------------------*)
function TGffLocString.DeleteLang(const Lang: Integer): Boolean;
var
  I			: Integer;
begin
  I			:= IndexOf(Lang);
  Result		:= I > -1;
  if Result then
    FLangs.Delete(I);
end;

(*------------------------------------------------------------------------------
Clear

If Finalize is False, index 0 is left in-tact and blank.

------------------------------------------------------------------------------*)
procedure TGffLocString.Clear(const Finalize: Boolean);
var
  I			: Integer;
begin
  if Finalize then
    FLangs.Clear
  else
    for I := 1 to Count-1 do
      FLangs.Delete(I);
end;

(*------------------------------------------------------------------------------
CopyFrom

------------------------------------------------------------------------------*)
procedure TGffLocString.CopyFrom(const Source: TGffLocString);
var
  I			: Integer;
begin
  StringRef		:= Source.StringRef;

  Clear;

  for I := 0 to Source.Count-1 do
    Add(Source.ByIndex[I], Source.Language[I]);

end;

(*------------------------------------------------------------------------------
HasLang

------------------------------------------------------------------------------*)
function TGffLocString.HasLang(const Lang: Cardinal): Boolean;
begin
  Result		:= IndexOf(Lang) > -1;
end;

(*------------------------------------------------------------------------------
IndexOf

------------------------------------------------------------------------------*)
function TGffLocString.IndexOf(const Lang: Cardinal): Integer;
begin
  Result		:= FLangs.IndexOfName(IntToStr(Lang));
end;

(*------------------------------------------------------------------------------
StrRefIsCustom

Is this LocString's StringRef flagged for an alt TLK?

------------------------------------------------------------------------------*)
function TGffLocString.StrRefIsCustom: Boolean;
begin
  Result		:= FStringRef and TLK_FLAG_CUSTOM = TLK_FLAG_CUSTOM;
end;


(*------------------------------------------------------------------------------
GetLangs

------------------------------------------------------------------------------*)
function TGffLocString.GetLangs: TStringList;
var
  I			: Integer;
begin
  Result		:= TStringList.Create;
  for I := 0 to Langs.Count-1 do
    Result.Add(Langs.Names[I]);
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
