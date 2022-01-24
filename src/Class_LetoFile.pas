(*------------------------------------------------------------------------------
Class_LetoFile

A wrapper for GffFile and ErfFile, making it possible to access a file of
either type using a single object, which (to an extent) flattens the
differences between the two formats. Additionally implements Parse for
running a LetoScript against a file, and iterator functions for walking
over some or all of the resources in an ERF.

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

unit Class_LetoFile;

interface

uses
  SysUtils, Classes,
  Header_Leto,
  Class_MultiEvent,
  Class_XbaseFile,
  Class_ErfFile, Class_ErfStruct, Class_GffFile, Class_GffField;

type

  TLetoFile		= class

  private

    FFileName		: String;
    FType		: TLetoFileType;
    FLoadLevel		: TGffLoadLevel;

    FIsWrapper		: Boolean;
    FParentErf		: TErfFile;
    FParentStruct	: TErfStruct;

    FErf		: TErfFile;
    FGff		: TGffFile;
    FTxt		: TFileStream;
    FFpt		: TFptFile;

    FGetResName		: String;
    FGetResIndex	: Integer;

    FOnOpen		: TMultiNotifyEvent;
    FOnDataReady	: TMultiNotifyEvent;
    FOnClose		: TMultiNotifyEvent;
    FOnDestroy		: TMultiNotifyEvent;

    procedure ParentErfDestroy(Sender: TObject);
    procedure GffDestroy(Sender: TObject);

    function GetSignature: String;
    procedure SetSignature(const AValue: String);
    function GetVersion: String;

    function GetIsErf: Boolean;
    function GetIsGff: Boolean;
    function GetIsErfOrGff: Boolean;
    function GetIsTxt: Boolean;
    function GetIsFpt: Boolean;

    function GetRoot: TGffField;

    function GetResNamed(const Direction: Shortint): TLetoError;

  protected

    procedure DoOpen; virtual;
    procedure DoDataReady; virtual;
    procedure DoClose; virtual;
    procedure DoDestroy; virtual;

  public

    Loaded		: Boolean;
    Modified		: Boolean;
    Error		: TLetoError;

    CurrResName		: String;
    CurrResType		: String;

    constructor Create(
      const AFileName	: String = '';
      const ALoadLevel	: TGffLoadLevel = LoadFull
    ); overload;

    constructor Create(
      const AFileType	: TLetoFileType;
      const AFileName	: String
    ); overload;

    constructor CreateWrapper(
      const AGff	: TGffFile;
      const AParentErf	: TErfFile = nil;
      const AParentStruct: TErfStruct = nil
    );

    destructor Destroy; override;

    property FileName: String read FFileName write FFileName;
    property FileType: TLetoFileType read FType;
    property LoadLevel: TGffLoadLevel read FLoadLevel;

    property Signature: String read GetSignature write SetSignature;
    property Version: String read GetVersion;

    property Erf: TErfFile read FErf;
    property Gff: TGffFile read FGff;
    property Fpt: TFptFile read FFpt;

    property IsWrapper: Boolean read FIsWrapper;
    property ParentErf: TErfFile read FParentErf write FParentErf;
    property ParentStruct: TErfStruct read FParentStruct write FParentStruct;

    property IsErf: Boolean read GetIsErf;
    property IsGff: Boolean read GetIsGff;
    property IsErfOrGff: Boolean read GetIsErfOrGff;
    property IsTxt: Boolean read GetIsTxt;
    property IsFpt: Boolean read GetIsFpt;

    property Root: TGffField read GetRoot;

    function Open(
      const AFileName	: String;
      const ALoadLevel	: TGffLoadLevel = LoadFull;
      OnProgress	: TLetoProgressEvent = nil
    ): TLetoError;

    function OpenResource(
      const ResName	: String
    ): TLetoError;

    procedure Close;

    function Save(
      const SaveAs	: String = '';
      OnProgress	: TLetoProgressEvent = nil
    ): TLetoError;

    function Find(
      Name		: String;
      Context		: TGffField = nil;
      const FieldType	: TGffVarType = gffANY;
      const ErrAsString	: Boolean = False
    ): TGffField;

    procedure Write(const S: String);

    function GetFirstResNamed(const Name: String = '*.bic'): TLetoError;
    function GetLastResNamed(const Name: String = '*.bic'): TLetoError;
    function GetNextResNamed: TLetoError;
    function GetPrevResNamed: TLetoError;

    property OnOpen: TMultiNotifyEvent read FOnOpen;
    property OnDataReady: TMultiNotifyEvent read FOnDataReady;
    property OnClose: TMultiNotifyEvent read FOnClose;
    property OnDestroy: TMultiNotifyEvent read FOnDestroy;

  end;


implementation


{ TLetoFile }


(*------------------------------------------------------------------------------

	Private

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
ParentErfDestroy

Receives signal that parent ERF (this is a wrapper) has been destroyed.

------------------------------------------------------------------------------*)
procedure TLetoFile.ParentErfDestroy(Sender: TObject);
begin
  FParentErf.OnDestroy.Detach(ParentErfDestroy);
  FParentErf		:= nil;
  FParentStruct		:= nil;
end;

(*------------------------------------------------------------------------------
GffDestroy

Receives signal that Gff (an observed GffFile) has been destroyed.

------------------------------------------------------------------------------*)
procedure TLetoFile.GffDestroy(Sender: TObject);
begin
  FGff.OnDestroy.Detach(GffDestroy);
  FGff			:= nil;
end;

(*------------------------------------------------------------------------------
property Signature

------------------------------------------------------------------------------*)
function TLetoFile.GetSignature: String;
begin
  if IsErf then
    Result		:= Erf.Signature
  else if IsGff then
    Result		:= Gff.Signature;
end;

procedure TLetoFile.SetSignature(const AValue: String);
var
  S, Sig, Ver		: String;
begin
  S			:= Copy(AValue, 1, 8);
  if Length(S) < 8 then Exit;
  Sig			:= Copy(S, 1, 4);
  Ver			:= Copy(S, 5, 4);
  if IsErf then begin
    Erf.Signature	:= Sig;
    Erf.Version		:= Ver;
  end
  else if IsGff then begin
    Gff.Signature	:= Sig;
    Gff.Version		:= Ver;
  end;
end;

(*------------------------------------------------------------------------------
property Version

------------------------------------------------------------------------------*)
function TLetoFile.GetVersion: String;
begin
  if IsErf then
    Result		:= Erf.Version
  else if IsGff then
    Result		:= Gff.Version;
end;

(*------------------------------------------------------------------------------
property IsErf

------------------------------------------------------------------------------*)
function TLetoFile.GetIsErf: Boolean;
begin
  Result		:= Loaded and IsErfType(FileType);
end;

(*------------------------------------------------------------------------------
property IsGff

------------------------------------------------------------------------------*)
function TLetoFile.GetIsGff: Boolean;
begin
  Result		:= Loaded and IsGffType(FileType);
end;

(*------------------------------------------------------------------------------
property IsErfOrGff

------------------------------------------------------------------------------*)
function TLetoFile.GetIsErfOrGff: Boolean;
begin
  Result		:= IsErf or IsGff;
end;

(*------------------------------------------------------------------------------
property IsTxt

------------------------------------------------------------------------------*)
function TLetoFile.GetIsTxt: Boolean;
begin
  Result		:= Loaded and (FileType = ftText);
end;

(*------------------------------------------------------------------------------
property IsFpt

------------------------------------------------------------------------------*)
function TLetoFile.GetIsFpt: Boolean;
begin
  Result		:= Loaded and (FileType = ftFpt);
end;

(*------------------------------------------------------------------------------
property Root

------------------------------------------------------------------------------*)
function TLetoFile.GetRoot: TGffField;
begin
  if Assigned(FGff) then
    Result		:= Gff.Root
  else
    Result		:= nil;
end;

(*------------------------------------------------------------------------------
GetResNamed

Does the work for Get*ResNamed - since all of them operate in nearly identical
fashion (with the exception of which direction they iterate FGetResIndex),
only a single procedure is needed for any.

------------------------------------------------------------------------------*)
function TLetoFile.GetResNamed(const Direction: Shortint): TLetoError;
var
  ThisRes		: String;
begin

  Inc(FGetResIndex, 1 * Direction);

  // No error raised when there is no matching resource
  Error			:= Success;
  Result		:= Error;

  if FGetResIndex < 0 then begin
    // Cap it at -1 so that GetNextResType can 'start over'
    FGetResIndex	:= -1;
    if Assigned(FGff) then
      FGff.OnDestroy.Detach(GffDestroy);
    FGff		:= nil;
    Exit;
  end;

  if FGetResIndex > Erf.Count-1 then begin
    // Likewise, for GetPrevResType
    FGetResIndex	:= Erf.Count;
    if Assigned(FGff) then
      FGff.OnDestroy.Detach(GffDestroy);
    FGff		:= nil;
    Exit;
  end;

  ThisRes :=
    Lowercase(
      Erf.Structs[FGetResIndex].Name + '.' +
      Erf.Structs[FGetResIndex].ResType
    );

  if StringMatch(FGetResName, ThisRes) then
    Result		:= OpenResource(ThisRes)
  else
    Result		:= GetResNamed(Direction);

end;


(*------------------------------------------------------------------------------

	Protected

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
DoOpen

------------------------------------------------------------------------------*)
procedure TLetoFile.DoOpen;
begin
  OnOpen.Signal(self);
end;

(*------------------------------------------------------------------------------
DoDataReady

------------------------------------------------------------------------------*)
procedure TLetoFile.DoDataReady;
begin
  OnDataReady.Signal(self);
end;

(*------------------------------------------------------------------------------
DoClose

------------------------------------------------------------------------------*)
procedure TLetoFile.DoClose;
begin
  OnClose.Signal(self);
end;

(*------------------------------------------------------------------------------
DoDestroy

------------------------------------------------------------------------------*)
procedure TLetoFile.DoDestroy;
begin
  OnDestroy.Signal(self);
end;


(*------------------------------------------------------------------------------

	Public

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoFile.Create(
  const AFileName	: String;
  const ALoadLevel	: TGffLoadLevel
);
begin
  Loaded		:= False;

  FOnOpen		:= TMultiNotifyEvent.Create;
  FOnDataReady		:= TMultiNotifyEvent.Create;
  FOnClose		:= TMultiNotifyEvent.Create;
  FOnDestroy		:= TMultiNotifyEvent.Create;

  if AFileName <> '' then
    Open(AFileName, ALoadLevel);
end;

constructor TLetoFile.Create(
  const AFileType	: TLetoFileType;
  const AFileName	: String
);
begin
  Create('');

  FFileName		:= AFileName;
  FType			:= AFileType;

  Loaded		:= FType in [ftGFF, ftERF, ftText];

  if not Loaded then Exit;

  case FType of

    ftGFF:
    begin
      FGff		:= TGffFile.Create;
      FGff.FileName	:= FileName;
      FGff.OnDestroy.Attach(GffDestroy);
      FGff.Loaded	:= True;
    end;

    ftERF:
    begin
      FErf		:= TErfFile.Create;
      FErf.FileName	:= FileName;
      FErf.Loaded	:= True;
    end;

    ftText:
    begin
      try
        ForceDirectories( ExtractFilePath(FileName) );
        FTxt		:= TFileStream.Create(FileName, fmCreate);
      except
        Loaded		:= False;
      end;
    end;

  end;

end;

(*------------------------------------------------------------------------------
CreateWrapper

The use of TLetoFile is a generic way to refer to any file Leto can work with -
abstracting the significant differences between each of these object types
(until such a time as the object hierarchy is restructured to use inheritance,
perhaps).

Sometimes this convenience is needed simply to hide the fact that an object
has already been instantiated elsewhere, and must now be referred to generically
as a TLetoFile. Use CreateWrapper in these cases: a call to Destroy on
this object will not free the Erf / Gff passed in.

------------------------------------------------------------------------------*)
constructor TLetoFile.CreateWrapper(
  const AGff		: TGffFile;
  const AParentErf	: TErfFile;
  const AParentStruct	: TErfStruct
);
begin
  Create('');
  FIsWrapper		:= True;

  Loaded		:= True;
  FFileName		:= AGff.Filename;
  FType			:= ftGff;
  FGff			:= AGff;

  FParentErf		:= AParentErf;
  FParentStruct		:= AParentStruct;

  // TODO 4: CHANGE: Get rid of wrapper idiom, purify TLetoFile.
  // DONE 1: BUG: [4.0.3] Exception when wrapper destroyed after parent
  { The IsAttached check *must not* be necessary. It was here briefly
    because of a bug in 25+ where the OnDestroy was not Detached. The
    pointer was left dangling, and in a for-resource loop, new calls
    to CreateWrapper would re-use the same memory address, making it
    appear as if they were 'already attached'. }
//  if not FParentErf.OnDestroy.IsAttached(ParentErfDestroy) then
  FParentErf.OnDestroy.Attach(ParentErfDestroy);

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoFile.Destroy;
begin
  Close;

  DoDestroy;

  FreeAndNil(FOnOpen);
  FreeAndNil(FOnDataReady);
  FreeAndNil(FOnClose);
  FreeAndNil(FOnDestroy);

  inherited;
end;

(*------------------------------------------------------------------------------
Open

Use this function for generic access to any GFF or ERF file. You do not need
to know the type of the file in advance. The Result is an indication of
the success or failure of opening the file; including whether or not the
file was not of a recognized type (neither GFF nor ERF). Otherwise, upon
Success, use IsErf / IsGff to determine what type of file was just loaded,
then use the Erf or Gff property to access the data.

When the file is an ERF, the Erf property accesses the TErfFile opened.
The Gff property accesses whichever resource (in Erf) you currently have
open - which is accomplished using OpenResource. (If you do not call
OpenResource, Gff will be nil.) Alternatively, GetFirstResType (or
GetLastResType) may be used, instead of OpenResource.

Regardless of whether the file is an ERF or GFF, the Root property and
Find function are both shortcuts to the relevant Gff versions.

------------------------------------------------------------------------------*)
function TLetoFile.Open(
  const AFileName	: String;
  const ALoadLevel	: TGffLoadLevel;
  OnProgress		: TLetoProgressEvent
): TLetoError;
var
  Stream		: TFileStream;
  Ext			: String;
  ADbf			: TDbfFile;
begin
  Close;

  if Assigned(OnProgress) then
    OnProgress(self, psStarting, 0, 0, '(Opening)');

  FFileName		:= AFileName;
  FLoadLevel		:= ALoadLevel;

  Result		:= Err_Missing_File;

  if not FileExists(FileName) then
    Exit;

  try

  try
    Stream		:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  except
    Result		:= Err_Locked_File;
    Exit;
  end;

  // DBF and FPT are hard-coded, since the files don't have "strong" sigs
  Ext			:= Lowercase(ExtractFileExt(FileName));
  Ext			:= StringReplace(Ext, '.', '', []);
  FType			:= GuessFileTypeFromSig(Ext);

  { Don't rely on just file extension - use Stream as a last resort }
  if not(FType in [ftERF..ft_ERF, ftGFF..ft_GFF, ftDbf, ftFpt]) then
    FType		:= GuessFileTypeFromStream(Stream);

  { Make sure to assign file names to Erf and Gff, so they know where they
    came from (in case their methods are called directly hereafter). Strange
    errors (such as "file in use") can result otherwise. }

  if IsErfType(FType) then begin
    FErf		:= TErfFile.Create;
    FErf.FileName	:= FileName;
    Result		:= Erf.LoadFromStream(Stream, OnProgress);
    if Result <> Success then
      FreeAndNil(FErf);
  end else if IsGffType(FType) then begin
    FGff		:= TGffFile.Create;
    FGff.FileName	:= FileName;
    Result		:= Gff.LoadFromStream(Stream, FLoadLevel);
    if Result <> Success then
      FreeAndNil(FGff)
    else
      FGff.OnDestroy.Attach(GffDestroy);
  end else if FType in [ftDbf, ftFpt] then begin
    FType		:= ftFpt;
    ADbf		:= TDbfFile.Create;
    Result		:= ADbf.LoadFromFile(ChangeFileExt(FileName, '.dbf'));
    if Result <> Success then begin
      FreeAndNil(ADbf);
      Exit;
    end;
    FFpt		:= TFptFile.Create(ADbf);
    Result		:= Fpt.LoadFromFile;
    if Result <> Success then begin
      FreeAndNil(ADbf);
      FreeAndNil(FFpt);
    end;
  end else
    Result		:= Err_Unsupported_File;

  finally
    FreeAndNil(Stream);
    Loaded		:= Result = Success;
    Error		:= Result;
    if Assigned(OnProgress) then
      OnProgress(self, psEnding, 0, 0, '(Done!)');
    if Loaded then begin
      DoOpen;
      if IsGffType(FType) then
        DoDataReady;
    end;
  end;

end;

(*------------------------------------------------------------------------------
OpenResource

Note: this function is technically depreated by TErfFile.Open. It will
gradually be phased out in the next few builds, though this will involve some
restructuring of a large amount of code relying on this function, across
several applications.

The notes below are the original, now outdated.

When the file is an ERF, use this function to open one of its resources.
That resource becomes the current Gff (reference). If the resource has not
yet been Parsed (initially, none are Parsed), it is Parsed at this time -
and remains so. This could potentially eat up a lot of memory if
OpenResource is called a number of times, but in general memory usage
with the 0.4 engine is much slimmer than in days of old.

As a little bit of "undocumented" magic, ResName can also be a number
(though in the form of a String), which acts the same way as Erf.Structs[n],
with the added make-sure-it's-Parsed benefit. This is generally used
internally, but there's no harm in taking advantage of it elsewhere.

Note that if GetFirstResType or GetLastResType is called, Gff will now
point to the results of that current iteration, and not to the resource
named by OpenResource.

------------------------------------------------------------------------------*)
function TLetoFile.OpenResource(const ResName: String): TLetoError;
begin

  Result		:= Err_Not_Erf;
  try

  if not IsErf then
    Exit;

  if Assigned(FGff) then
    FGff.OnDestroy.Detach(GffDestroy);
  FGff			:= nil;
  CurrResName		:= '';
  CurrResType		:= '';

  { Attempt to open and parse it. }
  Result		:= Erf.Open(ResName);

  if Result <> Success then Exit;

  { Get self to point to it - note that .Erf points to it as .Opened. }
  CurrResName		:= Erf.Opened.Name;
  CurrResType		:= Erf.Opened.ResType;
  FGff			:= Erf.Opened.Gff;
  if Assigned(FGff) then
    FGff.OnDestroy.Attach(GffDestroy);

  finally
    Error		:= Result;
  end;

end;

(*------------------------------------------------------------------------------
Close

In general you won't need to use this procedure. It closes and Frees any
files or resources in use. This procedure is used internally whenever you
Open a new file (the old file is Closed).

------------------------------------------------------------------------------*)
procedure TLetoFile.Close;
begin
  DoClose;

  if not FIsWrapper then begin
    if Assigned(FErf) then
      FreeAndNil(FErf)
    else if Assigned(FGff) then
      FGff.Free
    else if Assigned(FTxt) then
      FreeAndNil(FTxt)
    else if Assigned(FFpt) then begin
      FFpt.Dbf.Free;
      FreeAndNil(FFpt);
    end;
  end
  else if Assigned(FParentErf) and Assigned(FParentStruct) then begin
    // DONE 1: BUG: [4.0.6] CreateWrapper would Attach, but Close would not Detach
    FParentErf.OnDestroy.Detach(ParentErfDestroy);
    FParentErf.Close(FParentStruct);
  end;

  FFilename		:= '';
  FType			:= ftUnknown;
  Loaded		:= False;
  Error			:= Err_Missing_File;
  Modified		:= False;
  CurrResName		:= '';
  CurrResType		:= '';

end;

(*------------------------------------------------------------------------------
Save

Whatever kind of file is loaded - save it. If no SaveAs paramater is given,
then the file is saved with its original filename, overwriting the original
if it exists. If SaveAs is specified, then that becomes the new operating
FileName.

Root can be specified in order to save from a given Struct down.

------------------------------------------------------------------------------*)
function TLetoFile.Save(
  const SaveAs		: String;
  OnProgress		: TLetoProgressEvent
): TLetoError;
begin

  Result		:= Err_Missing_File;

  if not Loaded then Exit;

  if SaveAs <> '' then
    FFileName		:= SaveAs;

  if IsErf then
    Result		:= Erf.SaveToFile(FFileName, OnProgress)
  else if IsGff then
    Result		:= Gff.SaveToFile(FFileName)
  else if IsTxt then begin
    // TODO 4: CHANGE: Flush on TFileStream (or just use TMemoryStream?)
    Result		:= Success;
  end
  else if IsFpt then begin
    Result		:= Fpt.SaveToFile( ChangeFileExt(FFileName, '.FPT') );
    if Result = Success then
      Result		:= Fpt.Dbf.SaveToFile( ChangeFileExt(FFileName, '.DBF') );
  end;

end;

(*------------------------------------------------------------------------------
Find

A shortcut to Gff.Find.

------------------------------------------------------------------------------*)
function TLetoFile.Find(
  Name			: String;
  Context		: TGffField;
  const FieldType	: TGffVarType;
  const ErrAsString	: Boolean
): TGffField;
begin
  if Assigned(FGff) then
    Result		:= Gff.Find(Name, Context, FieldType, ErrAsString)
  else
    Result		:= nil;
end;

(*------------------------------------------------------------------------------
Write

For Text files.

------------------------------------------------------------------------------*)
procedure TLetoFile.Write(const S: String);
begin
  FTxt.Write(PChar(S)^, Length(S));
end;

(*------------------------------------------------------------------------------
GetFirstResNamed

Initiates an iteration-search in Erf, looking for the type of resource
specified (it should be specified as a String, use GetResTypeName to
convert ResType from a WORD). This will locate the first resource with a name
matching the mask given (wildcards accepted) in the ERF, and Gff will point to
it. The next resource named so can be located with GetNextResNamed, and so on,
until the result (Gff) of GetNextResNamed is nil, which indicates there are no
more matching resources. If Gff after GetFirstResNamed is nil, there are *no*
resources matching the name, in the ERF.

------------------------------------------------------------------------------*)
function TLetoFile.GetFirstResNamed(
  const Name		: String
): TLetoError;
begin

  if not Assigned(Erf) then begin
    Error		:= Err_Not_Erf;
    Result		:= Error;
    Exit;
  end;

  FGetResName		:= Lowercase(Name);
  FGetResIndex		:= -1;

  Result		:= GetNextResNamed;

end;

(*------------------------------------------------------------------------------
GetLastResNamed

The complement of GetFirstResNamed, this starts from the end of the ERF and
works backwards, using GetPrevResNamed.

(It should be noted that GetNextResNamed and GetPrevResNamed do not have a
mandatory usage - you could use GetFirstResNamed, GetNextResNamed, then
GetPrevResNamed, and you'd be back where you started; or GetLastResNamed,
then GetNextResNamed: but that would just give you nil.)

The same wildcard syntax available to GetFirstResNamed also applies here.

------------------------------------------------------------------------------*)
function TLetoFile.GetLastResNamed(
  const Name		: String
): TLetoError;
begin

  if not Assigned(Erf) then begin
    Error		:= Err_Not_Erf;
    Result		:= Error;
    Exit;
  end;

  FGetResName		:= Lowercase(Name);
  FGetResIndex		:= Erf.Count;

  Result		:= GetPrevResNamed;

end;

(*------------------------------------------------------------------------------
GetNextResNamed

This requires GetFirstResNamed be called in advance, to establish a precedence
for what types of resource you're looking for. Afterward, successive calls
to GetNextResNamed will continually iterate through Erf, locating each
instance of the specified name, and pointing to each with the Gff property.
If Gff is nil, then GetNextResNamed has exhausted its search. (Further calls
to GetNextResNamed will also result in nil, though at any time GetPrevResNamed
could be used to begin iterating backward through the Erf.)

------------------------------------------------------------------------------*)
function TLetoFile.GetNextResNamed: TLetoError;
begin
  Result		:= GetResNamed(1);
end;

(*------------------------------------------------------------------------------
GetPrevResNamed

This requires GetLastResNamed be called in advance, to establish a precedence
for what types of resource you're looking for. Like GetNextResNamed, this
'walks' backward through Erf, locating each instance of name (which may include
wildcards), and pointing to each with the Gff property. Once the search is
complete, calls to GetPrevResNamed will result in Gff being nil. (But the
Result will be Success; Result is only set when there is an error loading
one of the resources found.)

------------------------------------------------------------------------------*)
function TLetoFile.GetPrevResNamed: TLetoError;
begin
  Result		:= GetResNamed(-1);
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
