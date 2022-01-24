(*------------------------------------------------------------------------------
Class_TlkFile

Leto's second-generation TLK engine, built now for editing as well as reading
of StringRefs. Some of the implementation may seem crude (arrays/buckets,
records), but the premise is speed. Compare the GFF engine, which has a much
more complex implementation, for the sake of flexibility.

Note that this engine is intended only for TLK V3.0 files. Pre-3.0 the files
had a different format, and this engine will outright reject them.

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

unit Class_TlkFile;

interface

uses
  SysUtils, Classes,
  Header_Leto;

type

  TTlkField		= record
    Text		: String;
    Flags		: Cardinal;
    SoundResRef		: array[0..15] of Char;
    Offset		: Cardinal;
    Size		: Cardinal;
    SoundLen		: Single;
  end;
  PTlkField		= ^TTlkField;


  TTlkHeader		= packed record
    Sig			: array[0..3] of Char;
    Ver			: array[0..3] of Char;
    LangId		: Cardinal;
    Count		: Cardinal;
    Offset		: Cardinal;
  end;


  TTlkFile		= class

  private

    Buckets		: array of array of array of PTlkField;

    FLoaded		: Boolean;
    FCount		: Cardinal;
    FCapacity		: Cardinal;
    FTotalSize		: Int64;

    FGrowthStage	: Byte;

    procedure Reset;
    procedure Grow;

    function GetStrRef(const Index: Cardinal): String;
    procedure SetStrRef(const Index: Cardinal; const Value: String);

    function GetStrRefFlags(const Index: Cardinal): Cardinal;
    procedure SetStrRefFlags(const Index, Value: Cardinal);

    function GetField(Index: Cardinal): PTlkField;

  public

    FileName		: String;

    Header		: TTlkHeader;

    Female		: TTlkFile;
    Alternate		: TTlkFile;

    Modified		: Boolean;

    property Loaded: Boolean read FLoaded;
    property Count: Cardinal read FCount;

    constructor Create(const AFileName: String = '');
    destructor Destroy; override;

    function LoadFromFile(const AFileName: String): TLetoError;
    function LoadFromStream(const Stream: TStream): TLetoError;

    function SaveToFile(const AFileName: String = ''): TLetoError;
    function SaveToStream(const Stream: TStream): TLetoError;

    function HasStrRef(const Index: Cardinal): Boolean;

    property StrRef[const Index: Cardinal]: String
      read GetStrRef write SetStrRef; default;

    property StrRefFlags[const Index: Cardinal]: Cardinal
      read GetStrRefFlags write SetStrRefFlags;

    function GetString(
      const Index	: Cardinal;
      const AsFemale	: Boolean = False
    ): String;

    function GetStringFlags(
      const Index	: Cardinal;
      const AsFemale	: Boolean = False
    ): Cardinal;

    procedure SetString(
      const Index	: Cardinal;
      const Value	: String;
      const AsFemale	: Boolean = False
    );

    procedure SetStringFlags(
      const Index	: Cardinal;
      const Flags	: Cardinal;
      const AsFemale	: Boolean = False
    );

    function FindString(
      const StringMask	: String;
      const IgnoreCase	: Boolean
    ): TStringList;

    property Field[Index: Cardinal]: PTlkField read GetField;

    function Add(const Text: String): Cardinal;

    procedure AddField(const Field: PTlkField);

    procedure Delete(Index: Cardinal);

    procedure Clear;

    procedure Close;

  end;


implementation

const
  HEADER_SIZE		= 20;

  BUCKET_SIZE_1		= 100;
  BUCKET_SIZE_2		= 100;

  GrowthChart		: array[0..5] of Word = ( 1, 16, 100, 200, 1000, 2000 );


{ TTlkFile }


(*------------------------------------------------------------------------------

	Private

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
Reset

SetLength Buckets back to its original capacity, 10,000.

------------------------------------------------------------------------------*)
procedure TTlkFile.Reset;
var
  C			: Cardinal;
begin
  FGrowthStage		:= 0;
  SetLength(Buckets, GrowthChart[0]);
  SetLength(Buckets[0], BUCKET_SIZE_1);
  for C := 0 to BUCKET_SIZE_2-1 do
    SetLength(Buckets[0][C], BUCKET_SIZE_2);

  FCount		:= 0;
  FCapacity		:= BUCKET_SIZE_1 * BUCKET_SIZE_2;
  FTotalSize		:= 0;

  Modified		:= True;

end;

(*------------------------------------------------------------------------------
Grow

------------------------------------------------------------------------------*)
procedure TTlkFile.Grow;
var
  Size			: Cardinal;
  Start			: Cardinal;
  C1, C2		: Cardinal;
begin
  Inc(FGrowthStage);
  Size			:= GrowthChart[FGrowthStage];
  Start			:= Length(Buckets);
  SetLength(Buckets, Size);
  for C1 := Start to Size-1 do begin
    SetLength(Buckets[C1], BUCKET_SIZE_1);
    for C2 := 0 to BUCKET_SIZE_1-1 do
      SetLength(Buckets[C1][C2], BUCKET_SIZE_2);
  end;

  FCapacity		:= Size * BUCKET_SIZE_1 * BUCKET_SIZE_2;
end;

(*------------------------------------------------------------------------------
property StrRef

------------------------------------------------------------------------------*)
function TTlkFile.GetStrRef(const Index: Cardinal): String;
var
  P			: PTlkField;
begin
  P			:= Field[Index];
  if Assigned(P) then
    Result		:= P.Text
  else
    Result		:= '';
end;

procedure TTlkFile.SetStrRef(const Index: Cardinal; const Value: String);
var
  P			: PTlkField;
  OldLen, NewLen	: Cardinal;
begin
  P			:= Field[Index];
  if not Assigned(P) then Exit;
  OldLen		:= Length(P.Text);
  NewLen		:= Length(Value);
  P.Text		:= Value;
  P.Size		:= NewLen;
  // DONE 1: CHANGE: [4.0.4] TEXT_PRESENT set to 1.
  P.Flags		:= $0001 or P.Flags;
  // DONE 1: BUG: [4.0.4] Bounds overrun on shrinking TLK
  FTotalSize		:= FTotalSize + Integer(NewLen - OldLen);
  Modified		:= True;
end;

(*------------------------------------------------------------------------------
property StrRefFlags

------------------------------------------------------------------------------*)
function TTlkFile.GetStrRefFlags(const Index: Cardinal): Cardinal;
var
  P			: PTlkField;
begin
  P			:= Field[Index];
  if Assigned(P) then
    Result		:= P.Flags
  else
    Result		:= 0;
end;

procedure TTlkFile.SetStrRefFlags(const Index, Value: Cardinal);
var
  P			: PTlkField;
begin
  P			:= Field[Index];
  if not Assigned(P) then Exit;
  P.Flags		:= Value;
  Modified		:= True;
end;

(*------------------------------------------------------------------------------
property Field

------------------------------------------------------------------------------*)
function TTlkFile.GetField(Index: Cardinal): PTlkField;
var
  Custom		: Boolean;
  B0, B1, B2		: Cardinal;
begin
  Result		:= nil;

  if Index = NO_STRREF then Exit;

  { Bit 0x01000000 indicates alternate TLK file.
    However, if an Alternate isn't specified, or the StrRef doesn't exist
    in the alternate, use this Tlk file.
  }
  Custom		:= Index and TLK_FLAG_CUSTOM = TLK_FLAG_CUSTOM;
  Index			:= Index and TLK_VALID_RANGE;
  if
    Custom and
    Assigned(Alternate) and
    Alternate.HasStrRef(Index)
  then begin
    Result		:= Alternate.GetField(Index);
    Exit;
  end;

  if not(Index < FCount) then Exit;

  B0 := Index div (BUCKET_SIZE_1 * BUCKET_SIZE_2);
  B1 := (Index - (B0 * BUCKET_SIZE_1 * BUCKET_SIZE_2)) div BUCKET_SIZE_1;
  B2 := Index mod BUCKET_SIZE_2;

  Result		:= Buckets[B0][B1][B2];

end;


(*------------------------------------------------------------------------------

	Public

------------------------------------------------------------------------------*)


(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TTlkFile.Create(const AFileName: String);
begin
  Reset;

  if AFileName <> '' then
    LoadFromFile(AFileName);

  Modified		:= False;

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TTlkFile.Destroy;
begin
  Clear;

  inherited;
end;

(*------------------------------------------------------------------------------
LoadFromFile

------------------------------------------------------------------------------*)
function TTlkFile.LoadFromFile(const AFileName: String): TLetoError;
var
  Stream		: TFileStream;
begin
  Clear;

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
function TTlkFile.LoadFromStream(const Stream: TStream): TLetoError;
var
  C			: Cardinal;
  P			: PTlkField;
  O, S			: Cardinal;
  NotUsed		: Cardinal;
begin
  FLoaded		:= False;
  Result		:= Err_Not_Tlk;

  if Stream.Size < HEADER_SIZE then Exit;

  Stream.Seek(0, 0);

  Stream.Read(Header, HEADER_SIZE);

  if Lowercase(Header.Sig + Header.Ver) <> 'tlk v3.0' then Exit;

  { Sanity tests }

  if
    (Header.Offset > Stream.Size) or
    (Int64(Header.Offset) <> HEADER_SIZE + (Int64(Header.Count) * 40))
  then
    Exit;

  Stream.Seek(HEADER_SIZE + ((Header.Count-1) * 40) + 28, 0);
  Stream.Read(O, 4);
  Stream.Read(S, 4);
  if Stream.Size <> Int64(Header.Offset) + Int64(O) + Int64(S) then begin
    Result		:= Err_Bad_ResOffset;
    Exit;
  end;

  Result		:= Success;
  FTotalSize		:= Stream.Size - Header.Offset;

  for C := 0 to Header.Count-1 do begin
    P			:= New(PTlkField);
    Stream.Seek(HEADER_SIZE + (C * 40), 0);
    Stream.Read(P.Flags, 4);
    Stream.Read(P.SoundResRef, 16);
    Stream.Read(NotUsed, 4);
    Stream.Read(NotUsed, 4);
    Stream.Read(P.Offset, 4);
    Stream.Read(P.Size, 4);
    Stream.Read(P.SoundLen, 4);
    // DONE 1: CHANGE: [4.0.4] Read text even if flag is 0.
    //if P.Flags and TLK_FLAG_TEXT = TLK_FLAG_TEXT then begin
    if P.Size > 0 then begin
      Stream.Seek(Header.Offset + P.Offset, 0);
      P.Text		:= ReadString(Stream, P.Size);
    end;
    AddField(P);
  end;

  FLoaded		:= True;
  Modified		:= False;

end;

(*------------------------------------------------------------------------------
SaveToFile

------------------------------------------------------------------------------*)
function TTlkFile.SaveToFile(const AFileName: String): TLetoError;
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
function TTlkFile.SaveToStream(const Stream: TStream): TLetoError;
var
  C			: Cardinal;
  P			: PTlkField;
  NotUsed		: Cardinal;
  Offset		: Cardinal;
begin

  Result		:= Success;

  Header.Count		:= FCount;
  Header.Offset		:= HEADER_SIZE + (FCount * 40);

  Stream.Size		:= Header.Offset + FTotalSize;

  Stream.Seek(0, 0);
  Stream.Write(Header, HEADER_SIZE);

  NotUsed		:= 0;
  Offset		:= 0;

  for C := 0 to FCount-1 do begin
    P			:= Field[C];
    if P.Size > 0 then
      P.Offset		:= Offset
    else
      P.Offset		:= 0;
    Stream.Seek(HEADER_SIZE + (C * 40), 0);
    Stream.Write(P.Flags, 4);
    Stream.Write(P.SoundResRef, 16);
    Stream.Write(NotUsed, 4);
    Stream.Write(NotUsed, 4);
    Stream.Write(P.Offset, 4);
    Stream.Write(P.Size, 4);
    Stream.Write(P.SoundLen, 4);
    if P.Text <> '' then begin
      Stream.Seek(Header.Offset + P.Offset, 0);
      Stream.Write(PChar(P.Text)^, P.Size);
      Inc(Offset, P.Size);
    end
    else
      Inc(Offset, P.Size);
  end;

  Modified		:= False;

end;

(*------------------------------------------------------------------------------
HasStrRef

------------------------------------------------------------------------------*)
function TTlkFile.HasStrRef(const Index: Cardinal): Boolean;
begin
  Result		:= (Index and TLK_VALID_RANGE) < FCount;
end;

(*------------------------------------------------------------------------------
GetString

------------------------------------------------------------------------------*)
function TTlkFile.GetString(
  const Index		: Cardinal;
  const AsFemale	: Boolean
): String;
begin
  if AsFemale and Assigned(Female) then
    Result		:= Female.GetStrRef(Index)
  else
    Result		:= GetStrRef(Index);
end;

(*------------------------------------------------------------------------------
GetStringFlags

------------------------------------------------------------------------------*)
function TTlkFile.GetStringFlags(
  const Index		: Cardinal;
  const AsFemale	: Boolean
): Cardinal;
begin
  if AsFemale and Assigned(Female) then
    Result		:= Female.GetStrRefFlags(Index)
  else
    Result		:= GetStrRefFlags(Index);
end;

(*------------------------------------------------------------------------------
SetString

------------------------------------------------------------------------------*)
procedure TTlkFile.SetString(
  const Index		: Cardinal;
  const Value		: String;
  const AsFemale	: Boolean
);
begin
  if AsFemale and Assigned(Female) then
    Female.SetStrRef(Index, Value)
  else
    SetStrRef(Index, Value);
end;

(*------------------------------------------------------------------------------
SetStringFlags

------------------------------------------------------------------------------*)
procedure TTlkFile.SetStringFlags(
  const Index		: Cardinal;
  const Flags		: Cardinal;
  const AsFemale	: Boolean
);
begin
  if AsFemale and Assigned(Female) then
    Female.SetStrRefFlags(Index, Flags)
  else
    SetStrRefFlags(Index, Flags);
end;

(*------------------------------------------------------------------------------
FindString

Finds all fields that match this StringMask. This can be a fairly slow
routine, as it searches every single field without any special fancy
algorithm; such optimization should occur later.

If Ignore is true, then each field's text is matched as lowercase. StringMask
itself should already be made lowercase by the calling function.

------------------------------------------------------------------------------*)
function TTlkFile.FindString(
  const StringMask	: String;
  const IgnoreCase	: Boolean
): TStringList;
var
  B0, B1, B2		: Cardinal;
  I			: Cardinal;
  S			: String;
begin
  Result		:= TStringList.Create;
  I			:= 0;

  for B0 := 0 to High(Buckets) do
    for B1 := 0 to BUCKET_SIZE_1-1 do
      for B2 := 0 to BUCKET_SIZE_2-1 do begin
        S		:= Buckets[B0][B1][B2].Text;
        if IgnoreCase then
          S		:= Lowercase(S);
        if StringMatch(StringMask, S) then
          Result.Add(IntToStr(I));
        Inc(I);
        if I = Count then Exit;
      end;

end;

(*------------------------------------------------------------------------------
Add

------------------------------------------------------------------------------*)
function TTlkFile.Add(const Text: String): Cardinal;
var
  P			: PTlkField;
begin
  P			:= New(PTlkField);
  P.Flags		:= TLK_FLAG_TEXT;
  P.SoundResRef		:= '';
  P.SoundLen		:= 0;
  P.Text		:= Text;
  P.Size		:= Length(Text);
  Inc(FTotalSize, P.Size);
  AddField(P);
  Result		:= FCount-1;
end;

(*------------------------------------------------------------------------------
AddField

------------------------------------------------------------------------------*)
procedure TTlkFile.AddField(const Field: PTlkField);
var
  B0, B1, B2		: Cardinal;
begin
  Assert(
    FCount < TLK_VALID_RANGE,
    'Maximum string count reached in TLK.'
  );
  if FCount = FCapacity then Grow;

  B0 := FCount div (BUCKET_SIZE_1 * BUCKET_SIZE_2);
  B1 := (FCount - (B0 * BUCKET_SIZE_1 * BUCKET_SIZE_2)) div BUCKET_SIZE_1;
  B2 := FCount mod BUCKET_SIZE_2;

  Buckets[B0][B1][B2]	:= Field;

  Inc(FCount);

  Modified		:= True;
  
end;

(*------------------------------------------------------------------------------
Delete


------------------------------------------------------------------------------*)
procedure TTlkFile.Delete(Index: Cardinal);
begin
  Assert(True, 'TlkFile.Delete not yet implemented.');

  Modified		:= True;
end;

(*------------------------------------------------------------------------------
Clear

To close this file and open another (with the same TTlkFile), use Clear.
All the fields in Buckets are cleared, and Buckets is set back to its original
size.

------------------------------------------------------------------------------*)
procedure TTlkFile.Clear;
var
  C			: Cardinal;
  P			: PTlkField;
begin
  if FCount > 0 then
    for C := 0 to FCount-1 do begin
      P			:= GetField(C);
      Dispose(P);
    end;

  Reset;

end;

(*------------------------------------------------------------------------------
Close

Clear contents, reset FileName to ''. Presumably, this object will be reused
for opening a different Tlk in the future.

------------------------------------------------------------------------------*)
procedure TTlkFile.Close;
begin
  Clear;
  FileName		:= '';
  FLoaded		:= False;
  Modified		:= False;
  FillChar(Header, SizeOf(TTlkHeader), 0);
end;

(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
end.
