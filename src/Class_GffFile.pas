(*------------------------------------------------------------------------------
Class_GffFile

Provides generic access to GFF files, as a wrapper over TGffField.

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

unit Class_GffFile;

interface

uses
  SysUtils, Variants, Classes, DateUtils, StrUtils,
  Header_Leto,
  Class_MultiEvent,
  Class_GffField;

type

  TGffFile		= class

  private

    FSize		: Int64;

    FRootStruct		: TGffField;
    FNotFound		: TGffField;
    FAnonymous		: TGffField;

    FOnDestroy		: TMultiNotifyEvent;

    function GetModified: Boolean;
    procedure SetModified(const Value: Boolean);

    function GetChanged: Boolean;

    function Anonymous(
      Value		: String
    ) : TGffField; overload;
    function Anonymous(
      Value		: Integer
    ) : TGffField; overload;

    function AddField(
      Path		: String;
      Context		: TGffField;
      const SetIfEx	: Boolean;
      const Duplicate	: Boolean;
      const Lang	: Integer = 0
    ) : TGffField; overload;

  protected

    procedure DoDestroy; virtual;

  public

    Filename		: String;
    FileType		: TLetoFileType;
    Signature		: String;
    Version		: String;

    Loaded		: Boolean;

    LoadLevel		: TGffLoadLevel;

    Header		: TGffHeader;

    Logfile		: String;
    Loglevel		: TLetoLogLevel;

    property Modified: Boolean read GetModified write SetModified;
    property Changed: Boolean read GetChanged;

    property Root: TGffField read FRootStruct;

    constructor Create;
    constructor CreateFrom(
      const AFileName	: String;
      const ALoadLevel	: TGffLoadLevel = LoadFull
    );
    destructor Destroy; override;

    function LoadFromFile(
      const AFileName	: String;
      const ALoadLevel	: TGffLoadLevel = LoadFull
    ): TLetoError;

    function LoadFromStream(
      const Stream	: TStream;
      const ALoadLevel	: TGffLoadLevel = LoadFull
    ): TLetoError;

    function SaveToFile(
      const AFileName	: String = '';
      const Struct	: TGffField = nil;
      const ASig	: String = '';
      const AVer	: String = ''
    ): TLetoError;

    function SaveToStream(
      const Stream	: TStream;
      Struct		: TGffField = nil;
      const ASig	: String = '';
      const AVer	: String = ''
    ): TLetoError;

    procedure BoilNames;

    procedure Clear;

    function Find(
      Name		: String;
      Context		: TGffField = nil;
      const FieldType	: TGffVarType = gffANY;
      const ErrAsString	: Boolean = False
    ): TGffField;

    procedure Match(
      var Params	: TGffMatchParams;
      Context		: TGffField = nil
    );

    function AddField(
      const FieldType	: TGffVarType;
      const Path	: String;
      const Value	: String;
      Context		: TGffField = nil;
      const Lang	: Integer = 0;
      const SetIfEx	: Boolean = True;
      const Duplicate	: Boolean = False
    ) : TGffField; overload;
    function AddField(
      const FieldType	: TGffVarType;
      const Path	: String;
      const Value	: Integer;
      Context		: TGffField = nil;
      const SetIfEx	: Boolean = True;
      const Duplicate	: Boolean = False
    ) : TGffField; overload;
    function AddField(
      const FieldType	: TGffVarType;
      const Path	: String;
      const Value	: Int64;
      Context		: TGffField = nil;
      const SetIfEx	: Boolean = True;
      const Duplicate	: Boolean = False
    ) : TGffField; overload;
    function AddField(
      const FieldType	: TGffVarType;
      const Path	: String;
      const Value	: Real;
      Context		: TGffField = nil;
      const SetIfEx	: Boolean = True;
      const Duplicate	: Boolean = False
    ) : TGffField; overload;
    function AddField(
      const FieldType	: TGffVarType;
      const Path	: String;
      Context		: TGffField = nil;
      const SetIfEx	: Boolean = True;
      const Duplicate	: Boolean = False
    ) : TGffField; overload;

    procedure DeleteField(
      const Path	: String;
      Context		: TGffField = nil;
      const FieldType	: TGffVarType = gffANY
    );

    property OnDestroy: TMultiNotifyEvent read FOnDestroy;

  end;


implementation

const
  GFF_HEADER_SIZE	= $38; // 56 bytes


{ TGffFile }


(*------------------------------------------------------------------------------

	Private

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
property Modified

------------------------------------------------------------------------------*)
function TGffFile.GetModified: Boolean;
begin
  Result		:= Root.Modified;
end;

procedure TGffFile.SetModified(const Value: Boolean);
begin
  Root.Modified		:= Value;
end;

(*------------------------------------------------------------------------------
property Changed

------------------------------------------------------------------------------*)
function TGffFile.GetChanged: Boolean;
begin
  Result		:= Root.Changed;
end;

(*------------------------------------------------------------------------------
Anonymous

Useful when some explicit data needs to be returned (such as an error
string, or the child count of a List), but the expected Result is TGffField.
This uses FAnonymous to satisfy Result, but stuffs it full of the given
Value for the sake of the explicit data desired.

------------------------------------------------------------------------------*)
function TGffFile.Anonymous(Value: String): TGffField;
begin
  FAnonymous.VarType	:= gffString;
  FAnonymous.SetValue(Value);
  Result		:= FAnonymous;
end;

function TGffFile.Anonymous(Value: Integer): TGffField;
begin
  FAnonymous.VarType	:= gffInt;
  FAnonymous.SetValue(Value);
  Result		:= FAnonymous;
end;

(*------------------------------------------------------------------------------
AddField

Private version that works as the back-end for all of the exposed, front-
end versions, which handle sorting out the different possible types,
while this handles the actual Field creation. The many:one Value is
transitioned here by taking advantage of the variant-like functionality
of TGffVarData, in the form of our handy local instance of Anonymous.

------------------------------------------------------------------------------*)
function TGffFile.AddField(
  Path			: String;
  Context		: TGffField;
  const SetIfEx		: Boolean;
  const Duplicate	: Boolean;
  const Lang		: Integer
): TGffField;
var
  Parent		: TGffField;
  LToken, RToken,
    Index, I		: Integer;
  S			: String;

  procedure SetResultValue;
  begin
    if FAnonymous.VarType = gffLocString then
      Result.SetValue(FAnonymous.AsLocString[Lang], Lang)
    else if FAnonymous.VarType = gffStruct then
      Result.Id		:= FAnonymous.Id
    else
      Result.SetValue(FAnonymous.AsVariant[0]);
    Modified		:= True;
  end;

begin

  Result		:= Find(Path, Context);
  if not Result.IsValid or Duplicate then
  else if Result.VarType = gffLocString then begin
    Result.SetValue(FAnonymous.AsLocString[Lang], Lang);
    Exit;
  end else if SetIfEx then begin
    SetResultValue;
    Exit;
  end;

  // Determine the parent from Path
  LToken		:= LastDelimiter('/', Path);
  RToken		:= LastDelimiter('[', Path);
  if LToken > 1 then begin
    Parent		:= Find( Copy(Path, 1, LToken-1), Context );
    Path		:= Copy(Path, LToken+1, Length(Path));
    if not Parent.IsValid then
      Exit;
  end

  // Parent is Root - shortcut rest of procedure
  else begin
    if LToken = 1 then
      System.Delete(Path, 1, 1);
    if RToken > 0 then
      Path		:= Copy(Path, 1, RToken-1);
    Result		:= Root.AddChild(Path, FAnonymous.VarType);
    SetResultValue;
    Exit;
  end;

  // In which Struct?
  Index			:= -1;
  LToken		:= LastDelimiter('[', Path);
  RToken		:= LastDelimiter(']', Path);
  if (LToken > 0) and (RToken > LToken + 1) then begin
    S			:= Copy(Path, LToken+1, RToken-LToken-1);
    if S = '_' then
      Index		:= Parent.Count-1
    else
      TryStrToInt(S, Index);
    Path		:= Copy(Path, 1, LToken-1);
  end;

  if (Index < 0) or (Index > Parent.Count-1) then
    Index		:= -1;

  // DONE 1: ADD: [4.0.6] gff.add a Struct
  // New Struct in existing List
  if FAnonymous.VarType = gffStruct then begin
    if not Parent.IsList then
      Exit;
  end

  // New Field in existing Struct
  else if Index > -1 then begin
    // Parent is a List, so find the child (Struct)
    // at the given Index.
    Parent		:= Parent.FirstChild;
    for I := 1 to Index do
      Parent		:= Parent.NextSibling;
  end

  // New Field in new Struct
  else if not Parent.IsStruct then
    Parent		:= Parent.AddChild('', gffStruct);

  // (else if Parent is a "CAPREF", add directly to that Parent)

  // Now add the new Field to the given parent
  Result		:= Parent.AddChild(Path, FAnonymous.VarType);

  // And set its value
  SetResultValue;

end;


(*------------------------------------------------------------------------------

	Protected

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
DoDestroy

------------------------------------------------------------------------------*)
procedure TGffFile.DoDestroy;
begin
  OnDestroy.Signal(self);
end;


(*------------------------------------------------------------------------------

	Public methods

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
constructors

------------------------------------------------------------------------------*)
constructor TGffFile.Create;
begin
  FileType		:= ftUnknown;

  FNotFound		:= TGffField.Create('');
  FAnonymous		:= TGffField.Create('');
  FAnonymous.IsAnonymous := True;

  FRootStruct		:= TGffField.Create('');
  FRootStruct.Root	:= FRootStruct;
  FRootStruct.VarType	:= gffStruct;

  FOnDestroy		:= TMultiNotifyEvent.Create;
end;

constructor TGffFile.CreateFrom(
  const AFileName	: String;
  const ALoadLevel	: TGffLoadLevel
);
begin
  Create;
  LoadFromFile(AFileName, ALoadLevel);
end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TGffFile.Destroy;
begin
  DoDestroy;

  FreeAndNil(FNotFound);
  FreeAndNil(FAnonymous);
  FreeAndNil(FRootStruct);

  FreeAndNil(FOnDestroy);

  inherited;
end;

(*------------------------------------------------------------------------------
LoadFromFile

------------------------------------------------------------------------------*)
function TGffFile.LoadFromFile(
  const AFileName	: String;
  const ALoadLevel	: TGffLoadLevel
): TLetoError;
var
  Stream		: TMemoryStream;
begin
  Clear;

  if not FileExists(AFileName) then begin
    Result		:= Err_Missing_File;
    Exit;
  end;

  FileName		:= AFileName;

  Stream		:= TMemoryStream.Create;

  try
    Stream.LoadFromFile(FileName);
    Result		:= LoadFromStream(Stream, ALoadLevel);
  finally
    FreeAndNil(Stream);
  end;

end;

(*------------------------------------------------------------------------------
LoadFromStream

------------------------------------------------------------------------------*)
function TGffFile.LoadFromStream(
  const Stream		: TStream;
  const ALoadLevel	: TGffLoadLevel
): TLetoError;
var
  Names			: TStringList;
  I			: Cardinal;
  DataStream		: TMemoryStream;
begin
  Clear;

  LoadLevel		:= ALoadLevel;

  if not Assigned(Stream) then begin
    Result		:= Err_Internal;
    Exit;
  end;

  if not IsGffStream(Stream) then begin
    Result		:= Err_Not_Gff;
    Exit;
  end;

  FSize			:= Stream.Size;

  Names			:= TStringList.Create;
  try

  { Header }
  Stream.Seek(0, 0);
  with Header do begin
    Sig			:= ReadString(Stream, 4);
    Ver			:= ReadString(Stream, 4);
    StructOffset	:= ReadLong(Stream);
    StructCount		:= ReadLong(Stream);
    FieldOffset		:= ReadLong(Stream);
    FieldCount		:= ReadLong(Stream);
    LabelOffset		:= ReadLong(Stream);
    LabelCount		:= ReadLong(Stream);
    FieldDataOffset	:= ReadLong(Stream);
    FieldDataCount	:= ReadLong(Stream);
    FieldIndicesOffset	:= ReadLong(Stream);
    FieldIndicesCount	:= ReadLong(Stream);
    ListIndicesOffset	:= ReadLong(Stream);
    ListIndicesCount	:= ReadLong(Stream);
    if
      (FSize <> ListIndicesOffset + ListIndicesCount) or
      (StructOffset > FSize) or
      (FieldOffset > FSize) or
      (LabelOffset > FSize) or
      (FieldDataOffset > FSize) or
      (FieldIndicesOffset > FSize)
    then begin
      Result		:= Err_Bad_Header;
      Exit;
    end;
  end;


  TryStrToFileType(Copy(Header.Sig, 1, 3), FileType);
  Signature		:= Header.Sig;
  Version		:= Header.Ver;

  { For the Minimum LoadLevel, all we want is the first two LocStrings }
  if LoadLevel = LoadMinimum then begin

    { Go directly to the FieldData }
    Stream.Seek(Header.FieldDataOffset, soFromBeginning);

    { This makes the ASSUMPTION that FirstName LastName are the
      first two LocStrings. Quick, but potentially dangerous. }
    Root.AddChild('FirstName', gffLocString).AsLocString.LoadFromStream(Stream);
    Root.AddChild('LastName', gffLocString).AsLocString.LoadFromStream(Stream);

    Result		:= Success;
    Loaded		:= True;

    Exit;

  end;

  { Populate Names }
  if Header.LabelCount > 0 then begin
    Stream.Seek(Header.LabelOffset, 0);
    for I := 0 to Header.LabelCount-1 do
      Names.Add(ReadString(Stream, 16));
  end;

  DataStream		:= TMemoryStream.Create;
  Stream.Seek(Header.FieldDataOffset, 0);
  DataStream.Size	:= Header.FieldDataCount;
  DataStream.CopyFrom(Stream, Header.FieldDataCount);

  Stream.Seek(Header.StructOffset, 0);
  Result := Root.LoadFromStream(
    LoadLevel, Header, Stream, Names
  );
  Loaded		:= Result = Success;

  FreeAndNil(DataStream);

  finally
    FreeAndNil(Names);
  end;

end;

(*------------------------------------------------------------------------------
SaveToFile

------------------------------------------------------------------------------*)
function TGffFile.SaveToFile(
  const AFileName	: String;
  const Struct		: TGffField;
  const ASig, AVer	: String
): TLetoError;
var
  Stream		: TFileStream;
begin
  // TODO 4: CHANGE: Idiom change - become SaveAs or not?
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
function TGffFile.SaveToStream(
  const Stream		: TStream;
  Struct		: TGffField;
  const ASig, AVer	: String
): TLetoError;
var
  Tracker		: TGffTracker;
  EmptyChar		: Char;
  L, I			: Integer;
  S			: String;
  FSig, FVer		: String;
begin
  Result		:= Err_Internal;
  if not Assigned(Stream) then Exit;

  Result		:= Success;

  if not Assigned(Struct) then
    Struct		:= Root;

  { Initialization }
  FillChar(Tracker, SizeOf(Tracker), 0);
  SetLength(Tracker.Names, 100);
  FSize			:= 0;

  { Re-index and catalog }
  Struct.ReIndex(Tracker);

  { Construct Header }
  FSize				:= GFF_HEADER_SIZE;
  Header.StructOffset		:= FSize;
  Header.StructCount		:= Tracker.StructIndex;
  Inc(FSize, Tracker.StructIndex * 12);
  Header.FieldOffset		:= FSize;
  Header.FieldCount		:= Tracker.FieldIndex;
  Inc(FSize, Tracker.FieldIndex * 12);
  Header.LabelOffset		:= FSize;
  Header.LabelCount		:= Tracker.LabelIndex;
  Inc(FSize, Tracker.LabelIndex * 16);
  Header.FieldDataOffset	:= FSize;
  Header.FieldDataCount		:= Tracker.DataSize;
  Inc(FSize, Tracker.DataSize);
  Header.FieldIndicesOffset	:= FSize;
  Header.FieldIndicesCount	:= Tracker.FieldIndicesSize;
  Inc(FSize, Tracker.FieldIndicesSize);
  Header.ListIndicesOffset	:= FSize;
  Header.ListIndicesCount	:= Tracker.ListIndicesSize;
  Inc(FSize, Tracker.ListIndicesSize);
  if ASig <> '' then FSig	:= ASig
  else FSig			:= Signature;
  if AVer <> '' then FVer	:= AVer
  else FVer			:= Version;
  { Just in case... }
  if FSig = '' then FSig	:= 'BIC '
  else if Length(FSig) < 4 then
    FSig			:= FSig + StringOfChar(' ', 4 - Length(FSig));
  FSig				:= Uppercase(FSig);
  if FVer = '' then FVer	:= 'V3.2'
  else if Length(FVer) < 4 then
    FVer			:= FVer + StringOfChar(' ', 4 - Length(FVer));
  FVer				:= Uppercase(FVer);

  { Initialize the Stream }
  Stream.Size			:= FSize;
  Stream.Seek(0, 0);

  { Write Header }
  // TODO 4: CHANGE: Optimize with Stream.Write(Header, 56)
  Stream.Write(PChar(FSig)^, 4);
  Stream.Write(PChar(FVer)^, 4);
  Stream.Write(Header.StructOffset, 4);
  Stream.Write(Header.StructCount, 4);
  Stream.Write(Header.FieldOffset, 4);
  Stream.Write(Header.FieldCount, 4);
  Stream.Write(Header.LabelOffset, 4);
  Stream.Write(Header.LabelCount, 4);
  Stream.Write(Header.FieldDataOffset, 4);
  Stream.Write(Header.FieldDataCount, 4);
  Stream.Write(Header.FieldIndicesOffset, 4);
  Stream.Write(Header.FieldIndicesCount, 4);
  Stream.Write(Header.ListIndicesOffset, 4);
  Stream.Write(Header.ListIndicesCount, 4);

  { Write the Label Array }
  EmptyChar		:= #0;
  if Tracker.LabelIndex > 0 then begin
    Stream.Seek(Header.LabelOffset, 0);
    for L := 0 to Tracker.LabelIndex-1 do begin
      S			:= Tracker.Names[L];
      for I := 1 to 16 do
        if I > Length(S) then
          Stream.Write(EmptyChar, 1)
        else
          Stream.Write(S[I], 1);
    end;
  end;

  { Reset Tracker }
  SetLength(Tracker.Names, 0);
  FillChar(Tracker, SizeOf(Tracker), 0);

  Struct.SaveToStream(Stream, Header, Tracker);

end;

(*------------------------------------------------------------------------------
BoilNames

This procedure is important for Gff files that have been built from scratch,
but should be used with EXTREME caution for files that were originally
produced by NWN and are being edited by Leto - ESPECIALLY BIC FILES.

The function of BoilNames is to make a list of all the names (Labels) Fields
are using (from Root down, IN ORDER), and assign Fields a LabelIndex based
on the position of their Label in the list. This means two things:

1. There will be no duplicate Labels. In the case of some BIC files, this
is a Bad Thing(tm).

2. No Fields will have a LabelIndex of -1.

Because of (1), the file will be considerably smaller than if it were not
boiled, first. This becomes significant when you're building an entire GFF
from scratch, like for instance a DLG. You don't want a thousand Labels all
for "Index", you just want one. HOWEVER, some BIC files NEED duplicate Labels,
or NWN will do silly things.

Because of (2), SaveToStream will run much faster.

------------------------------------------------------------------------------*)
procedure TGffFile.BoilNames;
var
  List			: TStringList;
  Field			: TGffField;
begin
  List			:= TStringList.Create;
  { First pass: collect }
  Field			:= Root;
  while Assigned(Field) do begin
    if (Field.Name <> '') and (List.IndexOf(Field.Name) = -1) then
      List.Add(Field.Name);
    Field		:= Field.Next;
  end;
  { Second pass: assign }
  Field			:= Root;
  while Assigned(Field) do begin
    if Field.Name <> '' then
      Field.LabelIndex	:= List.IndexOf(Field.Name);
    Field		:= Field.Next;
  end;
  FreeAndNil(List);
end;

(*------------------------------------------------------------------------------
Clear

------------------------------------------------------------------------------*)
procedure TGffFile.Clear;
begin
  Root.Clear;

  Loaded		:= False;
  Modified		:= False;
  Signature		:= '';
  Version		:= '';

end;

(*------------------------------------------------------------------------------
Find

** The notes below are outdated. Attribs are now handled separately, and some
of the syntax has changed. **

Does the work for GetNamedElement, by hunting for a named Element
inside of a given parent Element. (Or Entry 0, if parent is nil.)

The characters [] are recognized as special syntax, and allow for
the result to target a specific Entry when the named Element occurs
multiple times in the given parent: for instance, when trying to find
'Rank' in the SkillList Element, it is useful to specify exactly
which one you mean - as in, SkillList/Rank[5] (for the *6th* skill).

Special syntax:
	[*]	First match in any Entry
	[_]	Search the last existing Entry
Unrecognized syntax, such as [?] or [-1] is treated as [*].

Current notes:

The syntax of Name is important, when searching within structured types (such
as a List or Struct). To indicate a child of a List or Struct, use a slash:
  ClassList/Class

Furthermore, to distinguish between multiple Structs within a List, use
the square brackets and a number, to indicate absolute index - in much the
same way as an array member would be indicated, in most programming syntax:
  ClassList/Class[0]
This would indicate the first (0) Struct within ClassList, and the Field
named Class, there. Note the positioning of the square-brackets: after
the *Field*, not the List. This is because there are multiple instances of
Class (in each of the Structs beneath ClassList), whereas there is only
a singular instance of ClassList itself - so the brackets come after the
element which is multiple - the Field after the List.

This syntax can be nested as deep as needed:
  LvlStatList/FeatList[0]/Feat[3]
Indicates the first Struct in LvlStatList (by the [0]) and the Field named
FeatList there. FeatList in turn is a List, so we look for the Field named
Feat, in the *fourth* Struct beneath FeatList. ([3] means fourth.)

Another special element of the syntax is using the number-sign after a List
or Struct:
  LvlStatList#
This returns the number of children of that List / Struct. This could be used
after brackets if necessary:
  LvlStatList/FeatList[0]#

Where Structs occur within Structs, treat the Struct as if it were just another
Field:
  CombatInfo/NumAttacks
There are no square brackets because there are no Structs beneath CombatInfo -
it is a Struct itself. (Although occuring in the "CAPREF" style, here.)

To locate and return an actual Struct (non-CAPREF), use only square brackets
followed by a slash:
  LvlStatList/[0]/
The square brackets can be ommited in the case of the first Struct, so the
above could be abbreviated to:
  LvlStatList/

Direct children of the root Struct can be returned with anonymous brackets:
  [0]?name
The root Struct itself can be queried using '@':
  @?count

Finally, '~' can be used at the beginning of a Name to indicate the current
Proxy (which can be set to any Field) - this allows for limiting a search
to a particular branch.

The ErrAsString param is used when Find should indicate an error as a string,
in the Result, rather than remitting a Result of type gffINVALID.

------------------------------------------------------------------------------*)
function TGffFile.Find(
  Name			: String;
  Context		: TGffField;
  const FieldType	: TGffVarType;
  const ErrAsString	: Boolean
): TGffField;
var
  NameList		: TStringList;
  S			: String;
  I			: Integer;
begin
  if ErrAsString then
    Result		:= Anonymous('Invalid Field.')
  else
    Result		:= FNotFound;

  if (Name = '') then Exit;

  { Shortcut for the Root itself }
  if (Name = '/') or (Name = '@') then begin
    Result		:= Root;
    Exit;
  end;

  { Shortcut for first-Child }
  if (RightStr(Name, 1) = '/') and (RightStr(Name, 2) <> '//') then
    Name		:= Name + '[0]/';

  Result		:= Root;
  if Context = nil then
    Context		:= Root;

  { Context searching }
  if Name[1] = '~' then begin
    Result		:= Context;
    System.Delete(Name, 1, 1);
    if (Name <> '') and (Name[1] = '/') then
      System.Delete(Name, 1, 1);
  end

  { Return-to-parent }
  // DONE 1: ADD: [4.0.6] Context-parent: /^/FirstName
  else if Name[1] = '^' then begin
    Result		:= Context;
    if Assigned(Result) and Assigned(Result.Parent) then
      Result		:= Result.Parent.Parent;
    System.Delete(Name, 1, 1);
    if (Name <> '') and (Name[1] = '/') then
      System.Delete(Name, 1, 1);
  end;


  NameList		:= TStringList.Create;

  { Split a pathed Name into a list describing the path }
  for I := 1 to Length(Name) do
    if Name[I] <> '/' then
      S			:= S + Name[I]
    else begin
      if S <> '' then
        NameList.Add(S);
      S			:= '';
    end;
  if S <> '' then
    NameList.Add(S);

  { Walk down the path, starting from the Root }
  while Assigned(Result) and (NameList.Count > 0) do begin
    S			:= NameList.Strings[0];
    Result		:= Result.Find(S, FieldType, False);
    NameList.Delete(0);
  end;

  FreeAndNil(NameList);

  if not Assigned(Result) then begin
    if ErrAsString then
      Result		:= Anonymous('Invalid Field: ' + S)
    else
      Result		:= FNotFound;
  end;

end;

(*------------------------------------------------------------------------------
Match

Encapsulates GffField.Match, setting up defaults, conveniences, and caveats.

Old notes (transplant to documentation):

Functionally, name= may include a leading path, which is where the Match should
begin from. This path follows the same syntax as for any named expression
(see Find). Whatever is after the last slash is what's actually given to
Params.Name. E.g.,
  name=FeatList/F*
matches any Fields with a name beginning with F under FeatList (which includes
the Structs under the List, though none of them will match).
  name=FeatList/*
matches the Structs, too.
  name=FeatList/[0]/*
matches any Field under the first Struct in FeatList.

A leading ~/ can be used in Name, as with Find, to begin Match'ing in the
Context passed in (which, if nil, is Root).

As with GffField.Match, the first successful match is returned, except when
count or count= is used (see below).

Here are how the name-value pairs correspond to TGffMatchParams:

  name=S : Params.Name := S (default '*')

  value=S : Params.Value := S (default '*')

  lang=S : Params.Lang := Int(S) (default -1)

  type=
  List the types by their 'names', e.g. 'short' for gffSHORT, 'LocString' for
  gffLocString, etc. Separate entries in the list with |, e.g.,
  'types=string|resref|locstring'
  The default is [gffANY].

  depth=S : Params.Depth := Int(S) (default -1)

  count=S : Params.Count := Int(S) (default 1)

  count
  When bare, this sets mbCountOnly in Behavior. The Result will be a reference
  to the count found (as FAnonymous), which can be evaluated with AsInt
  (or AsString).

  civalue
  Use bare or 'civalue=true'. Sets mbCIValue in Behavior.

  haschildren
  Used bare, it sets mbMustHaveChild, otherwise it sets mbMustHaveNoChild when
  any value other than 'haschildren=true'. When not specified, mbNoChildCheck
  is set.

------------------------------------------------------------------------------*)
procedure TGffFile.Match(
  var Params		: TGffMatchParams;
  Context		: TGffField
);
begin

  { Name }
  if Params.Name = '*' then
    Include(Params.Behavior, mbWildName)
  else begin
    Params.Name		:= Lowercase(Params.Name);
    { Avoid StringMatch when unnecessary, for faster results }
    if (Pos('*', Params.Name) > 0) or (Pos('?', Params.Name) > 0) then
      Include(Params.Behavior, mbWildName);
  end;

  { Value }
  if Params.Value = '*' then
    Include(Params.Behavior, mbWildValue)
  else begin
    if mbCIValue in Params.Behavior then
      Params.Value	:= Lowercase(Params.Value);
    { Avoid StringMatch when unnecessary, for faster results }
    if (Pos('*', Params.Value) > 0) or (Pos('?', Params.Value) > 0) then
      Include(Params.Behavior, mbWildValue);
  end;

  { Match! }
  if Assigned(Params.Root) then
    Params.Root.Match(Params)
  else
    Root.Match(Params);

end;

(*------------------------------------------------------------------------------
AddField

Add a String type; gffString, gffResRef, or gffLocString.

------------------------------------------------------------------------------*)
function TGffFile.AddField(
  const FieldType	: TGffVarType;
  const Path, Value	: String;
  Context		: TGffField;
  const Lang		: Integer;
  const SetIfEx		: Boolean;
  const Duplicate	: Boolean
): TGffField;
begin
  FAnonymous.SetValue(Value, Lang, FieldType);
  if FieldType = gffStruct then
    FAnonymous.Id	:= StrToIntDef(Value, 0);
  Result		:= AddField(Path, Context, SetIfEx, Duplicate, Lang);
end;

(*------------------------------------------------------------------------------
AddField

Add an Integer type; gffBYTE, gffCHAR, gffWORD, gffSHORT, gffINT.

------------------------------------------------------------------------------*)
function TGffFile.AddField(
  const FieldType	: TGffVarType;
  const Path		: String;
  const Value		: Integer;
  Context		: TGffField;
  const SetIfEx		: Boolean;
  const Duplicate	: Boolean
): TGffField;
begin
  FAnonymous.SetValue(Value, FieldType);
  Result		:= AddField(Path, Context, SetIfEx, Duplicate);
end;

(*------------------------------------------------------------------------------
AddField

Add a large Integer type; gffDWORD, gffDWORD64, gffINT64.

------------------------------------------------------------------------------*)
function TGffFile.AddField(
  const FieldType	: TGffVarType;
  const Path		: String;
  const Value		: Int64;
  Context		: TGffField;
  const SetIfEx		: Boolean;
  const Duplicate	: Boolean
): TGffField;
begin
  FAnonymous.SetValue(Value, FieldType);
  Result		:= AddField(Path, Context, SetIfEx, Duplicate);
end;

(*------------------------------------------------------------------------------
AddField

Add a Real type; gffFLOAT, gffDOUBLE.

------------------------------------------------------------------------------*)
function TGffFile.AddField(
  const FieldType	: TGffVarType;
  const Path		: String;
  const Value		: Real;
  Context		: TGffField;
  const SetIfEx		: Boolean;
  const Duplicate	: Boolean
): TGffField;
begin
  FAnonymous.SetValue(Value, FieldType);
  Result		:= AddField(Path, Context, SetIfEx, Duplicate);
end;

(*------------------------------------------------------------------------------
AddField

This version is for valueless types: gffList, gffStruct. It can also be
used for gffVoid.

------------------------------------------------------------------------------*)
function TGffFile.AddField(
  const FieldType	: TGffVarType;
  const Path		: String;
  Context		: TGffField;
  const SetIfEx		: Boolean;
  const Duplicate	: Boolean
): TGffField;
begin
  // TODO 4: ADD: Value for StructId
  FAnonymous.VarType	:= FieldType;
  Result		:= AddField(Path, Context, SetIfEx, Duplicate);
end;

(*------------------------------------------------------------------------------
DeleteField

A quick shortcut for looking up a Field, and deleting it (if found), with
the added convenience of only deleting the Field if it matches FieldType.
This accepts the same path syntax as Find. (Which means Structs can be
deleted, also.)

This carries all of the consequences of TGffField.Delete - any and all
children will be deleted as well.

If the Field isn't found, nothing happens.

------------------------------------------------------------------------------*)
procedure TGffFile.DeleteField(
  const Path		: String;
  Context		: TGffField;
  const FieldType	: TGffVarType
);
var
  Victim		: TGffField;
begin
  Victim		:= Find(Path, Context, FieldType);

  if not Victim.IsValid then Exit;

  FreeAndNil(Victim);

  Modified		:= True;

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
