(*------------------------------------------------------------------------------
Class_ErfStruct

Acts as an interface between TErfFile and TGffFile; TErfStruct implies a
structure in an ERF, and *may* be a GFF, although TErfStruct allows for
arbitrary non-GFF data to be managed as a TMemoryStream as well.

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

unit Class_ErfStruct;

interface

uses
  SysUtils, Classes,
  Header_Leto,
  Class_GffFile;

type

(*------------------------------------------------------------------------------

	TErfStruct

------------------------------------------------------------------------------*)
  TErfStruct		= class

  private

    FName		: String;
    FType		: String;
    FTypeId		: Word;

    FGff		: TGffFile;
    FError		: TLetoError;

    FData		: TMemoryStream;

    FNext		: TErfStruct;
    FPrev		: TErfStruct;

    constructor CreateFrom(
      const Stream	: TMemoryStream;
      const AName	: String;
      const AType	: String
    ); overload;

    constructor CreateFrom(
      const FileName	: String;
      const AName	: String;
      const AType	: String
    ); overload;

    constructor CreateFrom(
      const AGff	: TGffFile;
      const AName	: String;
      const AType	: String
    ); overload;

    function LoadFromFile(const FileName: String): TLetoError;
    function LoadFromStream(Stream: TMemoryStream): TLetoError;

    function GetLoaded: Boolean;

    procedure SetResType(const Value: String);
    procedure SetResTypeId(const Value: Word);
    procedure SetData(const Value: TMemoryStream);

  public

    property Name: String read FName write FName;
    property ResType: String read FType write SetResType;
    property ResTypeId: Word read FTypeId write SetResTypeId;

    property Gff: TGffFile read FGff write FGff;
    property Loaded: Boolean read GetLoaded;
    property Error: TLetoError read FError;

    property Data: TMemoryStream read FData write SetData;

    property Next: TErfStruct read FNext write FNext;
    property Prev: TErfStruct read FPrev write FPrev;

    constructor Create(
      const AName	: String;
      const AType	: String = 'BIC'
    );

    class function CreateAs(
      const Stream	: TMemoryStream;
      const AName	: String;
      const AType	: String = 'BIC'
    ) : TErfStruct; overload;

    class function CreateAs(
      const FileName	: String;
      const AName	: String;
      const AType	: String = 'BIC'
    ) : TErfStruct; overload;

    class function CreateAs(
      const AGff	: TGffFile;
      const AName	: String;
      const AType	: String = 'BIC'
    ) : TErfStruct; overload;

    destructor Destroy; override;

    function Parse: TLetoError;

  end;


implementation


{ TErfStruct }


(*------------------------------------------------------------------------------
constructor

The generic constructor for a new ERF resource. The Name and Type (ResType)
of the resource can be passed in, Type will be BIC if not specified.

------------------------------------------------------------------------------*)
constructor TErfStruct.Create(
  const AName		: String;
  const AType		: String
);
begin

  FName			:= AName;
  ResType		:= AType;

  FGff			:= TGffFile.Create;
  FError		:= Err_Missing_File;

  FData			:= TMemoryStream.Create;

end;

(*------------------------------------------------------------------------------
CreateAs

These are effectively CreateFrom constructors, but implemented as class
functions so that the passed-in prototypes can be sanity-checked before
an instance is actually returned; this means nil is the return value
when the sanity check fails (Stream is nil, FileName doesn't exist,
Gff is nil or not Loaded), but otherwise an instance of TErfStruct is
returned.

------------------------------------------------------------------------------*)
class function TErfStruct.CreateAs(
  const Stream		: TMemoryStream;
  const AName		: String;
  const AType		: String
): TErfStruct;
begin
  if Assigned(Stream) then
    Result		:= TErfStruct.CreateFrom(Stream, AName, AType)
  else
    Result		:= nil;
end;

class function TErfStruct.CreateAs(
  const FileName	: String;
  const AName		: String;
  const AType		: String
): TErfStruct;
begin
  if FileExists(FileName) then
    Result		:= TErfStruct.CreateFrom(FileName, AName, AType)
  else
    Result		:= nil;
end;

class function TErfStruct.CreateAs(
  const AGff		: TGffFile;
  const AName		: String;
  const AType		: String
): TErfStruct;
begin
  if Assigned(AGff) and AGff.Loaded then
    Result		:= TErfStruct.CreateFrom(AGff, AName, AType)
  else
    Result		:= nil;
end;

(*------------------------------------------------------------------------------
CreateFrom (Stream)

When a resource should contain some arbitrary payload, it can be given as a
TMemoryStream. The stream is not examined, it is merely stored in the
ErfStruct. It can then be Parsed at a later time (but only if it is a GFF),
or if left unparsed, ErfFile can save the arbitrary data back to file - this
supports non-GFF types in an ERF.

Primarily, this is intended as an efficiency feature: when dealing with a
large ERF, you may want to operate only on a single GFF resource inside of it,
so parsing every single resource would be costly and fruitless. Instead,
CreateFrom every resource using Stream, then Parse those Structs that require
operation.

------------------------------------------------------------------------------*)
constructor TErfStruct.CreateFrom(
  const Stream		: TMemoryStream;
  const AName		: String;
  const AType		: String
);
begin

  FName			:= AName;
  ResType		:= AType;

  FError		:= Err_Missing_File;

  FData			:= TMemoryStream.Create;
  Data			:= Stream;

end;

(*------------------------------------------------------------------------------
CreateFrom (FileName)

Create a new ErfStruct populated with the GFF indicated by FileName.

------------------------------------------------------------------------------*)
constructor TErfStruct.CreateFrom(
  const FileName	: String;
  const AName		: String;
  const AType		: String
);
begin
  Create(AName, AType);

  LoadFromFile(FileName);

end;

(*------------------------------------------------------------------------------
CreateFrom (Gff)

Create a new ErfStruct populated with the GFF indicated.

------------------------------------------------------------------------------*)
constructor TErfStruct.CreateFrom(
  const AGff		: TGffFile;
  const AName		: String;
  const AType		: String
);
begin

  FName			:= AName;
  ResType		:= AType;

  FGff			:= TGffFile.Create;
  FGff.Signature	:= AGff.Signature;
  FGff.Version		:= AGff.Version;
  FGff.Root.CopyFrom(AGff.Root, True);
  FGff.Modified		:= False;
  FGff.Loaded		:= True;

  FError		:= Success;

  FData			:= TMemoryStream.Create;

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TErfStruct.Destroy;
begin

  if Assigned(FGff) then
    FreeAndNil(FGff);
  FreeAndNil(FData);

  inherited;
end;

(*------------------------------------------------------------------------------
LoadFromFile

------------------------------------------------------------------------------*)
function TErfStruct.LoadFromFile(const FileName: String): TLetoError;
var
  Stream		: TMemoryStream;
begin

  if not FileExists(FileName) then begin
    FError		:= Err_Missing_File;
    Result		:= FError;
    Exit;
  end;

  Stream		:= TMemoryStream.Create;
  Stream.LoadFromFile(FileName);

  Result		:= LoadFromStream(Stream);

  FreeAndNil(Stream);

end;

(*------------------------------------------------------------------------------
LoadFromStream

------------------------------------------------------------------------------*)
function TErfStruct.LoadFromStream(Stream: TMemoryStream): TLetoError;
begin
  FError		:= Gff.LoadFromStream(Stream);
  Result		:= FError;
end;

(*------------------------------------------------------------------------------
Parse

Whatever is in Data is treated as a GFF structure, and given to GffFile
as one, for LoadFromStream. The Gff property is then assigned this newly
constructed Gff.

The Result is the result of GffFile.LoadFromStream.

------------------------------------------------------------------------------*)
function TErfStruct.Parse: TLetoError;
begin

  // If the Gff has already been parsed, just quit.
  if Assigned(Gff) then begin
    Result		:= FError;
    Exit;
  end else
    FGff		:= TGffFile.Create;

  FError		:= LoadFromStream(Data);
  Result		:= FError;

end;

(*------------------------------------------------------------------------------
property Loaded

------------------------------------------------------------------------------*)
function TErfStruct.GetLoaded: Boolean;
begin
  Result		:= Assigned(Gff) and Gff.Loaded;
end;

(*------------------------------------------------------------------------------
property ResType

------------------------------------------------------------------------------*)
procedure TErfStruct.SetResType(const Value: String);
begin
  if Value <> '' then
    FType		:= Trim(Value)
  else
    FType		:= 'BIC';
  FTypeId		:= GetResTypeId(FType);
end;

(*------------------------------------------------------------------------------
property ResTypeId

------------------------------------------------------------------------------*)
procedure TErfStruct.SetResTypeId(const Value: Word);
begin
  FTypeId		:= Value;
  FType			:= GetResTypeName(Value);
end;

(*------------------------------------------------------------------------------
property Data

------------------------------------------------------------------------------*)
procedure TErfStruct.SetData(const Value: TMemoryStream);
var
  P			: Int64;
begin
  P			:= Value.Position;
  FData.Clear;
  FData.Size		:= Value.Size;
  FData.CopyFrom(Value, 0);
  Value.Seek(P, soFromBeginning);
end;

(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
end.
