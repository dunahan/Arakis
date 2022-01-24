(*------------------------------------------------------------------------------
Header_Leto

Suite-wide types, constants, and generic utilities.

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

unit Header_Leto;

{$I LetoScript.inc}

interface

uses
  SysUtils, Classes, TypInfo, Math;

const

  { Version is handled by this const, instead of taking advantage of the
    data in the EXE, because many parts of Leto don't have the luxury of
    looking into an EXE for a version number (and it's entirely irrelevant
    on Linux).
  }
  { This is the ENGINE version. Eventually all applications relying on
    the engine will have their own MyVersion. Moneo 4.0.1.22 means
    Leto engine 4.0.1, Moneo (LetoScript) build 22.
  }
  LetoVersion		= '4.0.6';

  GffFileExtensions =
    '*.bic;*.are;*.ifo;*.git;*.uti;*.utc;*.dlg;*.itp;*.utt;*.uts;*.gff;' +
    '*.fac;*.ute;*.utd;*.utp;*.gic;*.gui;*.utm;*.jrl;*.utw;*.ptm;*.ptt';
  ErfFileExtensions =
    '*.erf;*.sav;*.hak;*.nwm;*.mod';
  TxtFileExtensions =
    '*.txt;*.ini;*.nss;*.set;*.2da;*.txi;*.dft';

type

  TLetoFileType		= (
    ftUnknown,
    ftNested,
    ftXML,
    ftText,
      ftTXT, ftINI, ftNSS, ftSET, ft2DA, ftTXI, ftDFT,
      ft_Text,
    ftDbf, ftFpt,
    ftGFF,
      ftARE, ftIFO, ftBIC, ftGIT, ftUTI, ftUTC, ftDLG, ftITP, ftUTT,
      ftUTS, ftFAC, ftUTE, ftUTD, ftUTP, ftGIC, ftGUI, ftUTM, ftJRL,
      ftUTW, ftPTM, ftPTT,
      ft_GFF,
    ftERF,
      ftSAV, ftHAK, ftNWM, ftMOD,
      ft_ERF,
    ftBIF
  );
  TLetoFileTypeSet	= set of TLetoFileType;

const

  GffFileTypes		: TLetoFileTypeSet = [ftGFF..ft_GFF];
  ErfFileTypes		: TLetoFileTypeSet = [ftERF..ft_ERF];
  TxtFileTypes		: TLetoFileTypeSet = [ftText..ft_Text];

type

  TResTypeRec		= record
    ResType		: String;
    ResId		: Word;
  end;

const

  { Last updated: 2004-06-14, from Bio docs dated 2004-03-04
    Items marked with a * are not documented by BioWare.
    Note: you'll need to resize the array when adding new
    ResTypes. The INVALID record MUST be last in the list. }
  ResTypes		: array[0..73] of TResTypeRec = (
    (ResType: 'RES'; ResId: $0000),	{ * }
    (ResType: 'BMP'; ResId: $0001),
    (ResType: 'MVE'; ResId: $0002),	{ * }
    (ResType: 'TGA'; ResId: $0003),
    (ResType: 'WAV'; ResId: $0004),
    (ResType: 'PLT'; ResId: $0006),
    (ResType: 'INI'; ResId: $0007),
    (ResType: 'BMU'; ResId: $0008),	{ * }
    (ResType: 'MPG'; ResId: $0009),	{ * }
    (ResType: 'TXT'; ResId: $000A),
    (ResType: 'PLH'; ResId: $07D0),	{ * }
    (ResType: 'TEX'; ResId: $07D1),	{ * }
    (ResType: 'MDL'; ResId: $07D2),
    (ResType: 'THG'; ResId: $07D3),	{ * }
    (ResType: 'FNT'; ResId: $07D5),	{ * }
    (ResType: 'LUA'; ResId: $07D7),	{ * }
    (ResType: 'SLT'; ResId: $07D8),	{ * }
    (ResType: 'NSS'; ResId: $07D9),
    (ResType: 'NCS'; ResId: $07DA),
    (ResType: 'MOD'; ResId: $07DB),	{ * }
    (ResType: 'ARE'; ResId: $07DC),
    (ResType: 'SET'; ResId: $07DD),
    (ResType: 'IFO'; ResId: $07DE),
    (ResType: 'BIC'; ResId: $07DF),
    (ResType: 'WOK'; ResId: $07E0),
    (ResType: '2DA'; ResId: $07E1),
    (ResType: 'TLK'; ResId: $07E2),	{ * }
    (ResType: 'TXI'; ResId: $07E6),
    (ResType: 'GIT'; ResId: $07E7),
    (ResType: 'BTI'; ResId: $07E8),	{ * }
    (ResType: 'UTI'; ResId: $07E9),
    (ResType: 'BTC'; ResId: $07EA),	{ * }
    (ResType: 'UTC'; ResId: $07EB),
    (ResType: 'DLG'; ResId: $07ED),
    (ResType: 'ITP'; ResId: $07EE),
    (ResType: 'BTT'; ResId: $07EF),	{ * }
    (ResType: 'UTT'; ResId: $07F0),
    (ResType: 'DDS'; ResId: $07F1),
    (ResType: 'UTS'; ResId: $07F3),
    (ResType: 'LTR'; ResId: $07F4),
    (ResType: 'GFF'; ResId: $07F5),
    (ResType: 'FAC'; ResId: $07F6),
    (ResType: 'BTE'; ResId: $07F7),	{ * }
    (ResType: 'UTE'; ResId: $07F8),
    (ResType: 'BTD'; ResId: $07F9),	{ * }
    (ResType: 'UTD'; ResId: $07FA),
    (ResType: 'BTP'; ResId: $07FB),	{ * }
    (ResType: 'UTP'; ResId: $07FC),
    (ResType: 'DFT'; ResId: $07FD),
    (ResType: 'GIC'; ResId: $07FE),
    (ResType: 'GUI'; ResId: $07FF),
    (ResType: 'CSS'; ResId: $0800),	{ * }
    (ResType: 'CCS'; ResId: $0801),	{ * }
    (ResType: 'BTM'; ResId: $0802),	{ * }
    (ResType: 'UTM'; ResId: $0803),
    (ResType: 'DWK'; ResId: $0804),
    (ResType: 'PWK'; ResId: $0805),
    (ResType: 'BTG'; ResId: $0806),	{ * }
    (ResType: 'UTG'; ResId: $0807),	{ * }
    (ResType: 'JRL'; ResId: $0808),
    (ResType: 'SAV'; ResId: $0809),	{ * }
    (ResType: 'UTW'; ResId: $080A),
    (ResType: '4PC'; ResId: $080B),	{ * }
    (ResType: 'SSF'; ResId: $080C),
    (ResType: 'HAK'; ResId: $080D),	{ * }
    (ResType: 'NWM'; ResId: $080E),	{ * }
    (ResType: 'BIK'; ResId: $080F),	{ * }
    (ResType: 'NDB'; ResId: $0810),
    (ResType: 'PTM'; ResId: $0811),
    (ResType: 'PTT'; ResId: $0812),
    (ResType: 'ERF'; ResId: $270D),	{ * }
    (ResType: 'BIF'; ResId: $270E),	{ * }
    (ResType: 'KEY'; ResId: $270F),	{ * }
    (ResType: '???'; ResId: $FFFF)	{ INVALID }
  );

type

  TGffLoadLevel		= (
    LoadMinimum,
    LoadShort,
    LoadMedium,
    LoadFull
  );

  TGffVarType		= (
    gffByte,		{ 0 }
    gffChar,		{ 1 }
    gffWord,		{ 2 }
    gffShort,		{ 3 }
    gffDWord,		{ 4 }
    gffInt,		{ 5 }
    gffDWord64,		{ 6 }
    gffInt64,		{ 7 }
    gffFloat,		{ 8 }
    gffDouble,		{ 9 }
    gffString,		{ 10 }
    gffResRef,		{ 11 }
    gffLocString,	{ 12 }
    gffVoid,		{ 13 }
    gffStruct,		{ 14 }
    gffList,		{ 15 }
    gffANY,		{ (Leto internal: for functions that accept a FieldType) }
    gffUNSPECIFIED,	{ (Leto internal: when caller does *NOT* want to specify) }
    gffINVALID		{ (Leto internal: indicates a new or invalid Field) }
  );
  TGffVarSet		= set of TGffVarType;

const

  { Field data is in the Field's last 4 bytes }
  SimpleGffVars		: TGffVarSet = [
    gffByte, gffChar, gffWord, gffShort, gffDWord, gffInt,
    gffFLOAT
  ];

  { Field data is in the Field Data area of stream }
  ComplexGffVars	: TGffVarSet = [
    gffDWord64, gffInt64, gffDouble,
    gffString, gffResRef, gffLocString, gffVoid
  ];

  { Cannot have children (and must be parented by a List-Struct) }
  SterileGffVars	: TGffVarSet = [
    gffByte, gffChar, gffWord, gffShort, gffDWord, gffInt,
    gffDWord64, gffInt64, gffFloat, gffDouble,
    gffString, gffResRef, gffLocString, gffVoid
  ];

  { String types }
  StringGffVars		: TGffVarSet = [
    gffString, gffResRef, gffLocString
  ];

  { All 16 types (including Struct) }
  ValidGffVars		: TGffVarSet = [
    gffByte, gffChar, gffWord, gffShort, gffDWord, gffInt,
    gffDWord64, gffInt64, gffFloat, gffDouble,
    gffString, gffResRef, gffLocString, gffVoid,
    gffStruct, gffList
  ];

  NO_STRREF		= $FFFFFFFF;
  TLK_VALID_RANGE	= $00FFFFFF;
  TLK_FLAG_CUSTOM	= $01000000;
  TLK_FLAG_TEXT		= $0001;
  TLK_FLAG_SND		= $0002;
  TLK_FLAG_SNDLEN	= $0004;
  TLK_MASK_TEXT_OFF	= $000E;

type

  TUseTlkMode		= (
    useTlk, useTlkF, useCustom, useCustomF
  );

  TLocLang		= record
    Name		: String;
    Id			: Cardinal;
  end;

const
  LocLangs		: array[0..9] of TLocLang = (
    (Name: 'English';			Id: 0),
    (Name: 'French';			Id: 1),
    (Name: 'German';			Id: 2),
    (Name: 'Italian';			Id: 3),
    (Name: 'Spanish';			Id: 4),
    (Name: 'Polish';			Id: 5),
    (Name: 'Korean';			Id: 128),
    (Name: 'Chinese Traditional';	Id: 129),
    (Name: 'Chinese Simplified';	Id: 130),
    (Name: 'Japanese';			Id: 131)
  );


type

  TLetoString		= (
    Leto_NYI,
    Leto_Error
  );

  TLetoError		= (
    Err_Unknown, Err_Unspecified, Err_Internal, Err_NYI, Success, Cancel,
    Err_Missing_File, Err_No_File, Err_No_Filename, Err_No_Filehandle,
    Err_No_Overwrite, Err_Unsupported_File, Err_Locked_File,
    Err_Invalid_Resource, Err_Not_Erf, Err_Not_Gff, Err_Not_Tlk, Err_Not_2da,
    Err_Bad_Header, {Err_Bad_StructCount, } Err_Bad_StructIndex,
    Err_Bad_FieldIndex, Err_Bad_LabelIndex, Err_Bad_DataOffset,
    Err_Bad_ListIndex, Err_Bad_LocString, Err_Bad_ResOffset, Err_Empty_Erf
  );
  TLetoErrorSet		= set of TLetoError;

  TLetoErrToStr		= record
    Err			: TLetoError;
    S			: String;
  end;

const

  // DONE 1: CHANGE: [4.0.4] LetoScript errors in Header_LetoScript, so Engine doesn't need to be updated.
  LetoErrors		: array[0..25] of TLetoErrToStr = (
    (Err: Err_Unknown;
     S: 'Unknown error.'),
    (Err: Err_Unspecified;
     S: 'Unspecified error.'),
    (Err: Err_Internal;
     S: 'Internal error.'),
    (Err: Err_NYI;
     S: 'Not yet implemented.'),
    (Err: Success;
     S: 'No error to report.'),
    (Err: Cancel;
     S: 'Operation canceled.'),
    (Err: Err_Missing_File;
     S: 'The specified file could not be found.'),
    (Err: Err_No_File;
     S: 'A file must be open for this operation.'),
    (Err: Err_No_Filename;
     S: 'No filename supplied.'),
    (Err: Err_No_Overwrite;
     S: 'File already exists (overwrite not specified).'),
    (Err: Err_Unsupported_File;
     S: 'The specified file is an unsupported type.'),
    (Err: Err_Locked_File;
     S: 'The specified file could not be loaded, it may be in use.'),
    (Err: Err_Invalid_Resource;
     S: 'The specified resource does not exist in this file.'),
    (Err: Err_Not_Erf;
     S: 'This file is not a valid ERF.'),
    (Err: Err_Not_Gff;
     S: 'This file is not a valid GFF.'),
    (Err: Err_Not_Tlk;
     S: 'This file is not a valid TLK.'),
    (Err: Err_Not_2da;
     S: 'This file is not a valid 2DA.'),
    (Err: Err_Bad_Header;
     S: 'Failed to open file: bad header.'),
    (Err: Err_Bad_StructIndex;
     S: 'Failed to open file: bad struct index.'),
    (Err: Err_Bad_FieldIndex;
     S: 'Failed to open file: bad field index.'),
    (Err: Err_Bad_LabelIndex;
     S: 'Failed to open file: bad label index.'),
    (Err: Err_Bad_DataOffset;
     S: 'Failed to open file: bad data offset.'),
    (Err: Err_Bad_ListIndex;
     S: 'Failed to open file: bad list index.'),
    (Err: Err_Bad_LocString;
     S: 'Failed to open file: bad locstring.'),
    (Err: Err_Bad_ResOffset;
     S: 'Failed to open file: bad resource offset.'),
    (Err: Err_Empty_Erf;
     S: 'Failed to open file: empty resource file!')
  );
  (*
    (Err: Err_LS_;
     S: ''),
  *)

type

  TLetoLogLevel		= (
    Log_None,
    Log_Errors,
    Log_Partial,
    Log_Full
  );

  TProgressStage	= (
    psStarting, psRunning, psEnding
  );

  TLetoProgressEvent	= procedure(
    Sender		: TObject;
    Stage		: TProgressStage;
    ItemsDone		: Integer;
    ItemsTotal		: Integer;
    Msg			: String
  ) of object;

  { HEADERS }

  TGffHeader		= record
    Sig			: String;
    Ver			: String;
    StructOffset	: Cardinal;
    StructCount		: Cardinal;
    FieldOffset		: Cardinal;
    FieldCount		: Cardinal;
    LabelOffset		: Cardinal;
    LabelCount		: Cardinal;
    FieldDataOffset	: Cardinal;
    FieldDataCount	: Cardinal;
    FieldIndicesOffset	: Cardinal;
    FieldIndicesCount	: Cardinal;
    ListIndicesOffset	: Cardinal;
    ListIndicesCount	: Cardinal;
  end;

  { The naming here conforms in general to BioWare's documentation,
    although FileType is an enumerated type for better accountability,
    and Sig is the actual string BioWare calls FileType. }
  TErfHeader		= record
    Sig				: String;
    Ver				: String;
    LanguageCount		: Cardinal;
    LocalizedStringSize		: Cardinal;
    EntryCount			: Cardinal;
    OffsetToLocalizedString	: Cardinal;
    OffsetToKeyList		: Cardinal;
    OffsetToResourceList	: Cardinal;
    BuildYear			: Cardinal;
    BuildDay			: Cardinal;
    DescriptionStrRef		: Cardinal;
  end;

  { GENERIC }

  TCaseType		= (
    ctUppercase,	// FOO
    ctLowercase,	// foo
    ctProper		// Foo
  );

  { GENERAL UTILITIES }

  function ReadInt(
    const Stream	: TStream;
    const Len		: Byte = 4
  ): Integer;

  function ReadLong(
    const Stream	: TStream;
    const Len		: Byte = 4
  ): Cardinal;

  function ReadString(
    const Stream	: TStream;
    const Len		: Integer
  ): String;

  function Swap16(X: Word): Word; register;
  function Swap32(X: Cardinal): Cardinal; register;

  function SortListByValue(List: TStringList; Index1, Index2: Integer): Integer;

  //function StrClause(S, Open, Close: String): String;

  function StrMatchAt(
    const Substr, S	: String;
    const Index		: Integer = 1;
    const CaseSensitive	: Boolean = True
  ): Integer;

  function StringMatch(
    const Substr, S	: String;
    const Index		: Integer = 1
  ): Boolean;

  function ParamToList(Param: String): TStringList;
  function ListToParam(List: TStringList): String;
  function KeyValToList(Param: String): TStringList;

  function TryStrToOct(const S: String; out Value: Cardinal): Boolean;
  function StrToOct(const S: String): Cardinal;

  function TryStrToBin(const S: String; out Value: Cardinal): Boolean;
  function StrToBin(const S: String): Cardinal;

  function IntToBin(I, Digits: Cardinal): String;

  function StrTrimSpace(const S: String): String;

  function StringRepeat(
    const S		: String;
    const Count		: Integer
  ): String;

  function Base64Encode(const Stream: TStream): String; overload;
  function Base64Encode(const Data: String): String; overload;
  procedure Base64Decode(const S: String; const Stream: TStream); overload;
  function Base64Decode(const S: String): String; overload;

  function GetParams: TStringList;

  function VerifyParams(
    const Params	: TStringList;
    Flags		: String;
    Values		: array of String;
    const WantInfile	: Boolean = True;
    const WantOutfile	: Boolean = True;
    Omake		: Cardinal = 0
  ): Boolean;

  function GetFileList(
    const FileMask	: String;
    const Path		: String = '';
    const Recursive	: Boolean = False;
    const ReadOnly	: Boolean = False;
    const Hidden	: Boolean = False;
    const DirsOnly	: Boolean = False;
    const FullPaths	: Boolean = True
  ): TStringList;

  { LOCALIZATION }

  procedure MakeLangList(const List: TStrings);
  function LangNtoI(const Name: String): Cardinal;
  function LangItoN(const Id: Cardinal): String;
  function LangItoA(const Id: Cardinal): Cardinal;

  function StringToLetoString(const S: String): TLetoString;
  function GetLetoString(const Id: TLetoString): String;

  function GetLetoError(const Err: TLetoError): String;
  function FormatLetoError(
    const Err		: TLetoError;
    const Symptoms	: array of const
  ): String;

  procedure WriteStrings(const Strings: array of String); overload;
  procedure WriteStrings(const Strings: String); overload;
  procedure WriteStrings(const Strings: TStringList); overload;
  procedure WriteStrings(const LetoString: TLetoString); overload;
  procedure WriteStrings(const LetoError: TLetoError); overload;

  { TYPE INFO AND CONVERSION }

  function TryBFTtoFT(
    const BitField	: Word;
    var FieldType	: TGffVarType
  ): Boolean;

  function TryBFMtoFS(
    Mask		: Word;
    var FieldSet	: TGffVarSet
  ): Boolean;

  function TryMFTtoFT(
    const BitField	: Word;
    var FileType	: TLetoFileType
  ): Boolean;

  function TryMFBtoFS(
    Mask		: Word;
    var FileSet		: TLetoFileTypeSet
  ): Boolean;

  function GuessFileTypeFromStream(
    const Stream	: TStream
  ) : TLetoFileType;

  function GuessFileTypeFromFile(
    const FileName	: String
  ) : TLetoFileType;

  function GuessFileTypeFromSig(
    const Signature	: String
  ) : TLetoFileType;

  function TryStrToFileType(
    const TypeName	: String;
    var FileType	: TLetoFileType
  ): Boolean;

  function IsGffStream(
    const Stream	: TStream
  ) : Boolean;

  function IsGffFile(
    const FileName	: String
  ) : Boolean;

  function IsGffType(
    const FileType	: TLetoFileType
  ): Boolean; overload;
  function IsGffType(
    const FileName	: TFileName
  ): Boolean; overload;
  function IsGffType(
    const ResTypeId	: Word
  ): Boolean; overload;

  function IsErfStream(
    const Stream	: TStream
  ) : Boolean;

  function IsErfFile(
    const FileName	: String
  ) : Boolean;

  function IsErfType(
    const FileType	: TLetoFileType
  ) : Boolean;

  function GetResTypeName(
    const ResTypeId	: Word
  ) : String;

  function GetResTypeId(
    const ResType	: String
  ) : Word;

  function TryStrToFieldType(
    TypeName		: String;
    var VarType		: TGffVarType
  ) : Boolean;

  function FieldTypeToStr(
    FieldType		: TGffVarType;
    Truncate		: Boolean = True
  ) : String;

  function TryStrToGffLoadLevel(
    const LevelName	: String;
    var LoadLevel	: TGffLoadLevel
  ) : Boolean;

  function ConvertPath(const FileName: String): String;

implementation

const
  B64 : array[0..63] of Char =
    'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/';

(*------------------------------------------------------------------------------
ReadInt

Read a (four byte) integer from the stream, with error-checking.

------------------------------------------------------------------------------*)
function ReadInt(
  const Stream		: TStream;
  const Len		: Byte
): Integer;
begin
  if Assigned(Stream) then
    Stream.Read(Result, Len)
  else
    Result		:= -1;
end;

(*------------------------------------------------------------------------------
ReadLong

Read a longword from the stream.

------------------------------------------------------------------------------*)
function ReadLong(
  const Stream		: TStream;
  const Len		: Byte
): Cardinal;
begin
  if Assigned(Stream) then
    Stream.Read(Result, Len)
  else
    Result		:= 0;
end;

(*------------------------------------------------------------------------------
ReadString

Read an arbitrary-length string from the stream, with error-checking.

------------------------------------------------------------------------------*)
function ReadString(
  const Stream		: TStream;
  const Len		: Integer
): String;
var
  I			: Integer;
begin
  Result		:= '';

  if not Assigned(Stream) or (Len < 1) then Exit;

  SetLength(Result, Len);
  Stream.Read(Result[1], Len);

  // Strip trailing #0s
  for I := 1 to Len do
    if Result[I] = #0 then break;

  SetLength(Result, I-1);

end;

(*------------------------------------------------------------------------------
Swap16

Changes from big-endian to little-endian format, for instance after using
ReadInt from an FPT file.

------------------------------------------------------------------------------*)
function Swap16(X: Word): Word;
asm
  SHL EAX, 16
  BSWAP EAX
end;

(*------------------------------------------------------------------------------
Swap32

------------------------------------------------------------------------------*)
function Swap32(X: Cardinal): Cardinal;
asm
  BSWAP EAX
end;

(*------------------------------------------------------------------------------
SortListByValue

A CustomSort method for StringLists that contain only integers (a TIntList,
as it were), this sorts numbers properly so that you get [1 2 10 11 20]
instead of [1 10 11 2 20]. The given List should use its Object params to
store the integers, so that simple casting (instead of StrToInt) is all
that's necessary to perform the comparison.

------------------------------------------------------------------------------*)
function SortListByValue(List: TStringList; Index1, Index2: Integer): Integer;
begin
  if( Integer(List.Objects[Index1]) < Integer(List.Objects[Index2]) ) then
    Result := -1
  else if( integer (List.Objects[Index1]) > Integer(List.Objects[Index2]) ) then
    Result := 1
  else
    Result := 0;
end;

(*------------------------------------------------------------------------------
StrClause

Unlike many other StrBetween functions, this one supports nested clauses;
why don't the others, one wonders? ;p Its only caveat is that it works
"inside out", finding the last innermost clause first, and working backward
and outward from there.

Currently deprecated, unused project-wide and requires JclStrings dependency.

------------------------------------------------------------------------------
function StrClause(S, Open, Close: String): String;
var
  P			: Integer;
begin
  Result		:= '';

  // Last instance of Close
  P			:= StrLastPos(Close, S);
  if P = 0 then begin
    if StrLastPos(Open, S) > 0 then
      Result		:= StringReplace(S, Open, '', [rfReplaceAll]);
    Exit;
  end else if P = 1 then begin
    Result		:= StringReplace(S, Close, '', []);
    Exit;
  end;

  // First previous instance of Open
  S			:= Copy(S, 1, P-1);
  P			:= StrLastPos(Open, S);

  Result		:= Copy(S, P+Length(Open), Length(S));

end;
*)

(*------------------------------------------------------------------------------
StrMatchAt

Based on JclStrings StrMatches, but with an Integer Result for the location of
the find. This is effectively what StrMatch (JclStrings) should do, except
that the current version of StrMatch does not support the wildcard *.

Note that this has the added param of CaseSensitive. This convenience will
convert S to lowercase before running the search. Substr, however, is *not*
converted, since most functions will have already converted Substr.

------------------------------------------------------------------------------*)
function StrMatchAt(
  const Substr, S	: String;
  const Index		: Integer;
  const CaseSensitive	: Boolean
): Integer;
var
  Matches		: Boolean;
  LS			: String;
  StringPtr		: PChar;
  PatternPtr		: PChar;
  StringRes		: PChar;
  PatternRes		: PChar;
begin
  Result		:= 0;
  Matches		:= False;

  if SubStr = '' then Exit;

  if Substr = '*' then begin
    Result		:= 1;
    Exit;
  end;

  if CaseSensitive then
    StringPtr		:= PChar(@S[Index])
  else begin
    LS			:= Lowercase(S);
    StringPtr		:= PChar(@LS[Index]);
  end;
  PatternPtr		:= PChar(SubStr);
  StringRes		:= nil;
  PatternRes		:= nil;

  try

  repeat

    repeat
      case PatternPtr^ of
        #0:
          begin
            Matches := StringPtr^ = #0;
            if Matches or (StringRes = nil) or (PatternRes = nil) then
              Exit;

            StringPtr := StringRes;
            PatternPtr := PatternRes;
            Break;
          end;
        '*':
          begin
            Inc(PatternPtr);
            PatternRes := PatternPtr;
            Break;
          end;
        '?':
          begin
            if StringPtr^ = #0 then
              Exit;
            Inc(StringPtr);
            Inc(PatternPtr);
          end;
        else
          begin
            if StringPtr^ = #0 then
              Exit;
            if StringPtr^ <> PatternPtr^ then
            begin
              if (StringRes = nil) or (PatternRes = nil) then
                Exit;
              StringPtr := StringRes;
              PatternPtr := PatternRes;
              Break;
            end
            else
            begin
              Inc(StringPtr);
              Inc(PatternPtr);
            end;
          end;
      end;
    until False;

    repeat
      case PatternPtr^ of
        #0:
          begin
            Matches := True;
            Exit;
          end;
        '*':
          begin
            Inc(PatternPtr);
            PatternRes := PatternPtr;
          end;
        '?':
          begin
            if StringPtr^ = #0 then
              Exit;
            Inc(StringPtr);
            Inc(PatternPtr);
          end;
        else
          begin
            repeat
              if StringPtr^ = #0 then
                Exit;
              if StringPtr^ = PatternPtr^ then
                Break;
              Inc(StringPtr);
            until False;
            Inc(StringPtr);
            StringRes := StringPtr;
            Inc(PatternPtr);
            Break;
          end;
      end;
    until False;

  until False;

  finally
    if Matches then
      Result		:= Integer(StringPtr);
  end;

end;

(*------------------------------------------------------------------------------
StringMatch

------------------------------------------------------------------------------*)
function StringMatch(
  const Substr, S	: String;
  const Index		: Integer
): Boolean;
var
  StringPtr,
    PatternPtr,
    StringRes,
    PatternRes		: PChar;
begin
  Result		:= SubStr = '*';

  if Result or (S = '') then Exit;

  StringPtr		:= PChar(@S[Index]);
  PatternPtr		:= PChar(SubStr);
  StringRes		:= nil;
  PatternRes		:= nil;

  repeat

    repeat
      case PatternPtr^ of
        #0:
          begin
            Result	:= StringPtr^ = #0;
            if Result or (StringRes = nil) or (PatternRes = nil) then
              Exit;
            StringPtr	:= StringRes;
            PatternPtr	:= PatternRes;
            Break;
          end;
        '*':
          begin
            Inc(PatternPtr);
            PatternRes	:= PatternPtr;
            Break;
          end;
        '?':
          begin
            if StringPtr^ = #0 then
              Exit;
            Inc(StringPtr);
            Inc(PatternPtr);
          end;
        else
          begin
            if StringPtr^ = #0 then
              Exit;
            if StringPtr^ <> PatternPtr^ then
            begin
              if (StringRes = nil) or (PatternRes = nil) then
                Exit;
              StringPtr := StringRes;
              PatternPtr := PatternRes;
              Break;
            end
            else
            begin
              Inc(StringPtr);
              Inc(PatternPtr);
            end;
          end;
      end;
    until False;

    repeat
      case PatternPtr^ of
        #0:
          begin
            Result	:= True;
            Exit;
          end;
        '*':
          begin
            Inc(PatternPtr);
            PatternRes	:= PatternPtr;
          end;
        '?':
          begin
            if StringPtr^ = #0 then
              Exit;
            Inc(StringPtr);
            Inc(PatternPtr);
          end;
        else
          begin
            repeat
              if StringPtr^ = #0 then
                Exit;
              if StringPtr^ = PatternPtr^ then
                Break;
              Inc(StringPtr);
            until False;
            Inc(StringPtr);
            StringRes	:= StringPtr;
            Inc(PatternPtr);
            Break;
          end;
      end;
    until False;

  until False;

end;

(*------------------------------------------------------------------------------
ParamToList

Converts a string into a TStringList, using spaces and commas as delims,
single-quotes as the QuoteChar. This is a state machine, because of the
way name-value pairs are quoted, and the shortcomings of TStringList to
deal with such idiosyncrasies.

------------------------------------------------------------------------------*)
function ParamToList(Param: String): TStringList;
var
  C			: Char;
  Buff			: String;
  Quoted		: Boolean;
begin
  Result		:= TStringList.Create;
  Quoted		:= False;

  while Param <> '' do begin
    C			:= Param[1];

    if Quoted and (C <> '''') then
      Buff		:= Buff + C
    else if (C = ' ') or (C = ',') then begin
      if (C = ',') and (Length(Param) > 1) and (Param[2] = ' ') then
        Delete(Param, 1, 1);
      if (C = ',') or (Buff <> '') then
        Result.Add(Buff);
      Buff		:= '';
    end else if C = '''' then begin
      if not Quoted then
        Quoted		:= True
      // '' in a quoted string yields a quote
      else if (Length(Param) > 1) and (Param[2] = '''') then begin
        Delete(Param, 1, 1);
        Buff		:= Buff + '''';
      end else
        Quoted		:= False;
    end else
      Buff		:= Buff + C;

    Delete(Param, 1, 1);
  end;

  if Buff <> '' then
    Result.Add(Buff);

end;

(*------------------------------------------------------------------------------
ListToParam

The counterpart to the above, since StringList.CommaText uses double-quotes,
and cannot be trusted to yield a result which can be re-converted to a List.

------------------------------------------------------------------------------*)
function ListToParam(List: TStringList): String;
var
  I			: Integer;
begin
  Result		:= '';

  if not Assigned(List) then Exit;

  for I := 0 to List.Count-1 do begin
    Result :=
      Result +
      '''' +
      StringReplace(List[I], '''', '''''', [rfReplaceAll]) +
      '''';
    if I < List.Count -1 then
      Result		:= Result + ',';
  end;

end;

(*------------------------------------------------------------------------------
KeyValToList

List ParamToList, except this version is designed for strings like:

Table=Minotopia, BIC=(d:/nwserver/servervault/lil one/dy'ne)

"Table" and "BIC" will become Values in the StringList. The QuoteChar is
parantheses, but is optional. (Nesting parantheses is not supported.)
This is specifically used by NWNX-Leto for some functions that need a list
or arguments, where some arguments could have spaces and quotes, as in
the example. (But will never need to use parans in the quoted text.)

------------------------------------------------------------------------------*)
function KeyValToList(Param: String): TStringList;
var
  C			: Char;
  P			: Integer;
  KeyBuff, ValBuff	: String;
  Key, Quoted		: Boolean;
begin
  Result		:= TStringList.Create;

  P			:= 1;
  Key			:= True;
  Quoted		:= False;

  while P < Length(Param)+2 do begin
    C			:= Param[P];

    if Key then begin
      if C = '=' then
        Key		:= False
      else if not(C in [',', ' ']) then
        KeyBuff		:= KeyBuff + C;
    end

    else begin
      if (ValBuff = '') and (C = ' ') then
      else if (ValBuff = '') and (C = '(') then
        Quoted		:= True
      else if
        ( Quoted and (C = ')') ) or
        ( not Quoted and (C = ',') ) or
        ( P > Length(Param) )
      then begin
        Result.Values[KeyBuff] := ValBuff;
        KeyBuff		:= '';
        ValBuff		:= '';
        Key		:= True;
      end
      else
        ValBuff		:= ValBuff + C;
    end;

    Inc(P);

  end;

end;

(*------------------------------------------------------------------------------
TryStrToOct

Tries to convert the string S to an Integer, as if S were an octal number
(in string form).

------------------------------------------------------------------------------*)
function TryStrToOct(const S: String; out Value: Cardinal): Boolean;
const
  OctDigits		= '01234567';
var
  I, P			: Integer;
begin
  I			:= 0;
  Value			:= 0;
  Result		:= True;
  while I < Length(S) do begin
    Inc(I);
    P			:= Pos(S[I], OctDigits);
    Result		:= P > 0;
    if not Result then Break;
    Inc(
      Value,
      (P-1) * Trunc(Power(8, Length(S)-I))
    )
  end;
end;

(*------------------------------------------------------------------------------
StrToOct

------------------------------------------------------------------------------*)
function StrToOct(const S: String): Cardinal;
const
  OctDigits		= '01234567';
var
  I, P			: Integer;
begin
  I			:= 0;
  Result		:= 0;
  while I < Length(S) do begin
    Inc(I);
    P			:= Pos(S[I], OctDigits);
    if P = 0 then begin
      Result		:= 0;
      Exit;
    end;
    Inc(
      Result,
      (P-1) * Trunc(Power(8, Length(S)-I))
    )
  end;
end;

(*------------------------------------------------------------------------------
TryStrToBin

------------------------------------------------------------------------------*)
function TryStrToBin(const S: String; out Value: Cardinal): Boolean;
const
  BinDigits		= '01';
var
  I, P			: Integer;
begin
  I			:= 0;
  Value			:= 0;
  Result		:= True;
  while I < Length(S) do begin
    Inc(I);
    P			:= Pos(S[I], BinDigits);
    Result		:= P > 0;
    if not Result then Break;
    Inc(
      Value,
      (P-1) * Trunc(Power(2, Length(S)-I))
    )
  end;
end;

(*------------------------------------------------------------------------------
StrToBin

------------------------------------------------------------------------------*)
function StrToBin(const S: String): Cardinal;
const
  BinDigits		= '01';
var
  I, P			: Integer;
begin
  I			:= 0;
  Result		:= 0;
  while I < Length(S) do begin
    Inc(I);
    P			:= Pos(S[I], BinDigits);
    if P = 0 then begin
      Result		:= 0;
      Exit;
    end;
    Inc(
      Result,
      (P-1) * Trunc(Power(2, Length(S)-I))
    )
  end;
end;

(*------------------------------------------------------------------------------
IntToBin

Like IntToHex, but in binary.

------------------------------------------------------------------------------*)
function IntToBin(I, Digits: Cardinal): String;
var
  L			: Cardinal;
begin
  Result		:= '';
  while I <> 0 do begin
    Result :=
      Chr( 48 + Ord( (I and 1) <> 0 ) ) + Result;
    I := I shr 1;
  end;

  L			:= Length(Result);
  if L < Digits then
    Result :=
      StringOfChar('0', Digits - L) + Result;
end;

(*------------------------------------------------------------------------------
StrTrimSpace

------------------------------------------------------------------------------*)
function StrTrimSpace(const S: String): String;
begin
  Result		:= S;
  Result		:= StringReplace(Result, #9, '', [rfReplaceAll]);
  Result		:= Trim(Result);
end;

(*------------------------------------------------------------------------------
StringRepeat

------------------------------------------------------------------------------*)
function StringRepeat(
  const S		: String;
  const Count		: Integer
): String;
var
  I			: Integer;
begin
  Result		:= '';
  for I := 1 to Count do
    Result		:= Result + S;
end;

(*------------------------------------------------------------------------------
Base64Encode

------------------------------------------------------------------------------*)
function Base64Encode(const Stream: TStream): String;
var
  Bucket		: array[1..3] of Byte;
  N			: Integer;
begin
  Result		:= '';
  Bucket[1]		:= 0;
  Bucket[2]		:= 0;
  Bucket[3]		:= 2;
  N			:= 0;

  Stream.Seek(0, 0);

  while Stream.Position < Stream.Size do begin

    Stream.Read(Bucket[1], 1);
    Result		:= Result + B64[Bucket[2] or (Bucket[1] shr Bucket[3])];
    Bucket[2]		:= (Bucket[1] shl (6-Bucket[3])) and 63;
    Bucket[3]		:= (Bucket[3]+2) and 7;

    if Bucket[3] = 0 then begin
      Bucket[3]		:= 2;
      Result		:= Result + B64[Bucket[2]];
      Bucket[2]		:= 0;
      { Newline after 76 characters }
      Inc(N);
      if N = 19 then begin
        Result		:= Result + sLineBreak;
        N		:= 0;
      end;
    end;

  end;

  if Bucket[3] > 2 then begin
    Result		:= Result + B64[Bucket[2]] + '=';
    if Bucket[3] = 4 then
      Result		:= Result + '=';
  end;

end;

function Base64Encode(const Data: String): String; overload;
var
  Bucket		: array[1..3] of Byte;
  I, N			: Integer;
begin
  Result		:= '';
  Bucket[1]		:= 0;
  Bucket[2]		:= 0;
  Bucket[3]		:= 2;
  N			:= 0;

  for I := 1 to Length(Data) do begin

    Bucket[1]		:= Ord(Data[I]);
    Result		:= Result + B64[Bucket[2] or Bucket[1] shr Bucket[3]];
    Bucket[2]		:= (Bucket[1] shl (6-Bucket[3])) and 63;
    Bucket[3]		:= (Bucket[3]+2) and 7;

    if Bucket[3] = 0 then begin
      Bucket[3]		:= 2;
      Result		:= Result + B64[Bucket[2]];
      Bucket[2]		:= 0;
      { Newline after 76 characters }
      Inc(N);
      if N = 19 then begin
        Result		:= Result + sLineBreak;
        N		:= 0;
      end;
    end;

  end;

  if Bucket[3] > 2 then begin
    Result		:= Result + B64[Bucket[2]] + '=';
    if Bucket[3] = 4 then
      Result		:= Result + '=';
  end;

end;

(*------------------------------------------------------------------------------
Base64Decode

------------------------------------------------------------------------------*)
procedure Base64Decode(const S: String; const Stream: TStream);
var
  Bucket		: array[1..4] of Byte;
  I			: Cardinal;
  P			: Integer;
begin
  Bucket[3]		:= 0;

  Stream.Seek(0, 0);

  for I := 1 to Length(S) do begin

    { Newline, padding, invalids are skipped. }
    P			:= Pos(S[I], B64)-1;
    if P = -1 then
      Continue
    else
      Bucket[2]		:= P;

    if Bucket[3] > 0 then begin
      Bucket[4] :=
        (Bucket[1] shl Bucket[3]) or (Bucket[2] shr (6-Bucket[3]));
      Stream.Write(Bucket[4], 1);
    end;
    Bucket[3]		:= (Bucket[3]+2) and 7;
    Bucket[1]		:= Bucket[2];

  end;

  Stream.Seek(0, 0);

end;

function Base64Decode(const S: String): String;
var
  Bucket		: array[1..4] of Byte;
  I			: Cardinal;
  P			: Integer;
begin
  Result		:= '';
  Bucket[3]		:= 0;

  for I := 1 to Length(S) do begin

    { Newline, padding, invalids are skipped. }
    P			:= Pos(S[I], B64)-1;
    if P = -1 then
      Continue
    else
      Bucket[2]		:= P;

    if Bucket[3] > 0 then
      Result := Result + Chr(
        (Bucket[1] shl Bucket[3]) or (Bucket[2] shr (6-Bucket[3]))
      );

    Bucket[3]		:= (Bucket[3]+2) and 7;
    Bucket[1]		:= Bucket[2];

  end;

end;

(*------------------------------------------------------------------------------
GetParams

A simple state machine for loading the ParamStrs into a StringList, following
a basic syntax:

-x
A flag. Can be given as "-x -y -z" or "-xyz". These have no explicit value,
they are simply "on" (or naturally off, when not specified).

--foo
A whole-word flag, which can also have a value assigned, such as "--foo:bar".
These cannot be compounded, they must be listed separately, "--foo --bar".

foo
Without dashes; the first one to appear is the infile, the second is the
outfile, and everything afterward is treated as an "omake" (extra) arg,
and labeled omake1, omake2, etc. So, "foo bar baz" gives you infile=foo,
outfile=bar, and omake1=baz.

------------------------------------------------------------------------------*)
function GetParams: TStringList;
var
  I, L, P, O		: Integer;
  S			: String;
begin
  Result		:= TStringList.Create;
  O			:= 0;

  for I := 1 to ParamCount do begin
    S			:= ParamStr(I);
    if S[1] = '-' then begin
      if Length(S) = 1 then
        Continue
      else if S[2] = '-' then begin
        P := Pos(':', S);
        if P > 0 then
          Result.Values[ Copy(S, 3, P-3) ] :=
            Copy(S, P+1, Length(S))
        else
          Result.Values[ Copy(S, 3, Length(S)) ] := 'on';
      end else
        for L := 2 to Length(S) do
          Result.Values[S[L]] := 'on';
    end else if Result.Values['infile'] = '' then
      Result.Values['infile'] := S
    else if Result.Values['outfile'] = '' then
      Result.Values['outfile'] := S
    else begin
      Inc(O);
      Result.Values['omake' + IntToStr(O)] := S;
    end;
  end;

end;

(*------------------------------------------------------------------------------
VerifyParams

A simple function for verifying that a set of params (as given in the form
returned by GetParams) meets certain criteria, such as not having any
extraneous flags.

------------------------------------------------------------------------------*)
function VerifyParams(
  const Params		: TStringList;
  Flags			: String;
  Values		: array of String;
  const WantInfile	: Boolean;
  const WantOutfile	: Boolean;
  Omake			: Cardinal
): Boolean;
var
  I, L			: Integer;
  N, V			: String;
begin
  Result		:= False;

  for I := 0 to Params.Count-1 do begin
    N			:= Params.Names[I];
    V			:= Params.ValueFromIndex[I];
    if Length(N) = 1 then begin
      if Pos(N, Flags) = 0 then Exit
      else Flags := StringReplace(Flags, N, '', []);
    end else if Copy(N, 1, 5) = 'omake' then begin
      if Omake < 1 then Exit
      else Dec(Omake);
    end else if N = 'infile' then begin
      if not WantInfile then Exit;
    end else if N = 'outfile' then begin
      if not WantOutfile then Exit;
    end else begin
      for L := 0 to Length(Values)-1 do
        if Values[L] = N then begin
          Values[L] := '';
          N := '';
          Break;
        end;
      if N <> '' then Exit;
    end;
  end;

  Result		:= True;
end;

(*------------------------------------------------------------------------------
FindDirs

Used by FindFiles / GetFileList.

------------------------------------------------------------------------------*)
procedure FindDirs(
  F			: TSearchRec;
  const Path		: String;
  const DirList		: TStringList
);
begin
  if FindFirst(Path, faDirectory, F) <> 0 then Exit;
  repeat
    if
      ((F.Attr and faDirectory) = faDirectory) and
      (F.Name <> '.') and (F.Name <> '..')
    then
      DirList.Add(F.Name);
  until FindNext(F) <> 0;
  SysUtils.FindClose(F);
end;

(*------------------------------------------------------------------------------
FindFiles

Used by GetFileList.

------------------------------------------------------------------------------*)
procedure FindFiles(
  const List		: TStringList;
  const Path, Mask	: String;
  const Recursive,
    ReadOnly, Hidden,
    DirsOnly, FullPaths	: Boolean
);
var
  F			: TSearchRec;
  Found			: Boolean;
  SubDirs		: TStringList;
  I			: Integer;
  IsDir, IsRO,
    IsHidden		: Boolean;
begin
  Found			:= FindFirst(Path + Mask, faAnyFile, F) = 0;
  if Found then
  repeat
    { To prevent Platform Warnings (without disabling them entirely)
      the actual values of faReadOnly (1) and faHidden (2) are used. }
    IsDir		:= (F.Attr and faDirectory) = faDirectory;
    IsRO		:= (F.Attr and 1) = 1;
    IsHidden		:= (F.Attr and 2) = 2;
    if
      (
        DirsOnly and IsDir and
        (F.Name <> '.') and (F.Name <> '..')
      ) or (
        not DirsOnly and
        not IsDir and
        (
          ( IsRO and ReadOnly ) or
          ( IsHidden and Hidden ) or
          ( not IsRO and not IsHidden )
        )
      )
    then begin
      if FullPaths then
        List.Add(Path + F.Name)
      else
        List.Add(F.Name);
    end;
  until FindNext(F) <> 0;
  if Found then
    SysUtils.FindClose(F);
  if not Recursive then Exit;
  { Now do subdirs }
  SubDirs		:= TStringList.Create;
  try
    // TODO 4: LINUX: Is '*.*' necessary as well?
    // TODO 4: BUG: Hidden directories never found
    if DirsOnly then
      FindDirs(F, Path + Mask, SubDirs)
    else
      FindDirs(F, Path + '*', SubDirs);
    for I := 0 to SubDirs.Count-1 do
      FindFiles(
        List, Path + SubDirs[I] + PathDelim, Mask,
        Recursive, ReadOnly, Hidden, DirsOnly, FullPaths
      );
  finally
    FreeAndNil(SubDirs);
  end;
end;

(*------------------------------------------------------------------------------
GetFileList

A generic FindFirst routine for use whenever a FileMask must yield a list of
files in some directory, with flexible parameters.

This routine comprises almost the entire logic of LetoScript's Bic.Vault.

------------------------------------------------------------------------------*)
function GetFileList(
  const FileMask	: String;
  const Path		: String = '';
  const Recursive	: Boolean = False;
  const ReadOnly	: Boolean = False;
  const Hidden		: Boolean = False;
  const DirsOnly	: Boolean = False;
  const FullPaths	: Boolean = True
): TStringList;
var
  P			: String;
begin
  Result		:= TStringList.Create;

  if FileMask = '' then Exit;

  { Support for lazy slashing }
  P			:= ConvertPath(Path);
  if (Length(P) > 1) and (P[Length(P)] <> PathDelim) then
    P			:= P + PathDelim;

  FindFiles(
    Result, P, FileMask,
    Recursive, ReadOnly, Hidden, DirsOnly, FullPaths
  );

end;

(*------------------------------------------------------------------------------
MakeLangList

Fill List with all of the names of languages in LocLangs. Useful for
ComboBoxes.

------------------------------------------------------------------------------*)
procedure MakeLangList(const List: TStrings);
var
  I			: Integer;
begin
  if not Assigned(List) then Exit;

  for I := 0 to High(LocLangs) do
    List.Add(LocLangs[I].Name);

end;

(*------------------------------------------------------------------------------
LangNtoI

Given a language's Name, look up its Id, using the LocLang array.

------------------------------------------------------------------------------*)
function LangNtoI(const Name: String): Cardinal;
var
  I			: Integer;
begin
  Result		:= 0;
  for I := 0 to High(LocLangs) do
    if LocLangs[I].Name = Name then begin
      Result		:= LocLangs[I].Id;
      Break;
    end;
end;

(*------------------------------------------------------------------------------
LangItoN

The reverse of LangNtoI. Id to Name.

------------------------------------------------------------------------------*)
function LangItoN(const Id: Cardinal): String;
var
  I			: Integer;
begin
  Result		:= '';
  for I := 0 to High(LocLangs) do
    if LocLangs[I].Id = Id then begin
      Result		:= LocLangs[I].Name;
      Break;
    end;
end;

(*------------------------------------------------------------------------------
LangItoA

Takes an Id, and gets the absolute index (in LocLangs) of that Id.

------------------------------------------------------------------------------*)
function LangItoA(const Id: Cardinal): Cardinal;
var
  I			: Integer;
begin
  Result		:= 0;
  for I := 0 to High(LocLangs) do
    if LocLangs[I].Id = Id then begin
      Result		:= I;
      Break;
    end;
end;

(*------------------------------------------------------------------------------
StringToLetoString

------------------------------------------------------------------------------*)
function StringToLetoString(const S: String): TLetoString;
begin
  Result := TLetoString(
    GetEnumValue(TypeInfo(TLetoString), S)
  );
end;

(*------------------------------------------------------------------------------
GetLetoString

------------------------------------------------------------------------------*)
function GetLetoString(const Id: TLetoString): String;
begin

end;

(*------------------------------------------------------------------------------
GetLetoError

------------------------------------------------------------------------------*)
function GetLetoError(const Err: TLetoError): String;
var
  I			: Cardinal;
begin

  for I := 0 to High(LetoErrors) do
    if LetoErrors[I].Err = Err then begin
      Result		:= LetoErrors[I].S;
      Exit;
    end;

  Result		:= 'An unknown error occurred.';

end;

(*------------------------------------------------------------------------------
FormatLetoError

------------------------------------------------------------------------------*)
function FormatLetoError(
  const Err		: TLetoError;
  const Symptoms	: array of const
): String;
var
  I			: Integer;

  procedure _Format_(const Id, Str: String);
  begin
    Result		:= StringReplace(Result, Id, Str, []);
  end;

begin
  Result		:= GetLetoError(Err);

  for I := 0 to High(Symptoms) do
    with Symptoms[I] do
      case VType of
        vtInteger:	_Format_('%d', IntToStr(VInteger));
        vtAnsiString:	_Format_('%s', String(VAnsiString));
        vtChar:		_Format_('%c', VChar);
        vtVariant:	_Format_('%s', String(VAnsiString)); // watch out
        {
        vtBoolean:	S := S + BoolToStr(VBoolean);
        vtExtended:	S := S + FloatToStr(VExtended^);
        vtString:	S := S + VString^;
        vtPChar:	S := S + VPChar;
        vtObject:	S := S + VObject.ClassName;
        vtClass:	S := S + VClass.ClassName;
        vtCurrency:	S := S + CurrToStr(VCurrency^);
        vtVariant:	S := S + String(VVariant^);
        vtInt64:	S := S + IntToStr(VInt64^);
        }
      end;

end;

(*------------------------------------------------------------------------------
WriteStrings

Used only by Moneo, as a front-end to WriteLn, performing such things as
localization of strings, and writing several lines of text to the screen
at once; when given as a single string, it breaks the line up into lines
using '\n' as a delimiter. It does not perform wrapping.

------------------------------------------------------------------------------*)
procedure WriteString(S: String);
begin
  S			:= StringReplace(S, '\t', #9, [rfReplaceAll]);

  WriteLn(S);
end;
procedure WriteStrings(const Strings: TStringList);
var
  I			: Integer;
begin
  for I := 0 to Strings.Count-1 do
    WriteString(Strings[I]);
end;

procedure WriteStrings(const Strings: array of String);
var
  I			: Integer;
begin
  for I := Low(Strings) to High(Strings) do
    WriteString(Strings[I]);
end;

procedure WriteStrings(const Strings: String);
var
  S			: String;
  I			: Integer;
  Lines			: array of String;

  procedure AddToLines(const A: String);
  begin
    SetLength(Lines, Length(Lines)+1);
    Lines[High(Lines)]	:= A;
  end;

begin
  S			:= Strings;

  I			:= Pos('\n', S);
  while I > 0 do begin
    AddToLines(Copy(S, 1, I-1));
    S			:= Copy(S, I+2, Length(S));
    I			:= Pos('\n', S);
  end;
  AddToLines(S);

  WriteStrings(Lines);
end;

procedure WriteStrings(const LetoString: TLetoString);
begin
  WriteStrings(GetLetoString(LetoString));
end;

procedure WriteStrings(const LetoError: TLetoError);
begin
  WriteStrings(GetLetoError(LetoError));
end;

(*------------------------------------------------------------------------------
TryBFTtoFT

Try BitFieldType to FieldType. Since FieldType in LetoScript is a bitmask,
and in Delphi is an enum, this function converts from the former to the
latter - or if it can't, returns False.

This function is designed to only return a single FieldType (not a set), so
it also returns False if BitField is anything other than a single, valid
TGffVarType.

------------------------------------------------------------------------------*)
function TryBFTtoFT(
  const BitField	: Word;
  var FieldType		: TGffVarType
): Boolean;
var
  L			: Extended;
begin
  Result		:= BitField = 0;
  if Result then begin
    FieldType		:= gffInvalid;
    Exit;
  end;

  L			:= Log2(BitField);
  Result		:= Frac(L) = 0;
  if Result then
    FieldType		:= TGffVarType( Floor(L) );
end;

(*------------------------------------------------------------------------------
TryBFMtoFS

Try Bit Field Mask to Field Set.

------------------------------------------------------------------------------*)
function TryBFMtoFS(
  Mask			: Word;
  var FieldSet		: TGffVarSet
): Boolean;
var
  I			: Byte;
  B			: Word;
begin
  for I := 0 to 15 do begin
    B			:= 1 shl I;
    if Mask and B = B then begin
      Include(FieldSet, TGffVarType(I));
      Mask		:= Mask - B;
    end;
  end;

  Result		:= Mask = 0;

end;

(*------------------------------------------------------------------------------
TryMFTtoFT

Try MaskFileType to FileType. Same thing as above, with the ft* consts.

------------------------------------------------------------------------------*)
function TryMFTtoFT(
  const BitField	: Word;
  var FileType		: TLetoFileType
): Boolean;
const
  FileTypes		: array[0..6] of TLetoFileType = (
    ftUnknown, ftGFF, ftERF, ftBIF, ft2DA, ftXML, ftFpt
  );
var
  L			: Extended;
begin
  Result		:= BitField = 0;
  if Result then begin
    FileType		:= ftUnknown;
    Exit;
  end;

  L			:= Log2(BitField);
  Result		:= Frac(L) = 0;
  if Result then
    FileType		:= FileTypes[Floor(L)];
end;

(*------------------------------------------------------------------------------
TryBFMtoFS

Try Masked Filetype Bits to File Set.

------------------------------------------------------------------------------*)
function TryMFBtoFS(
  Mask			: Word;
  var FileSet		: TLetoFileTypeSet
): Boolean;
const
  FileTypes		: array[0..6] of TLetoFileType = (
    ftUnknown, ftGFF, ftERF, ftBIF, ft2DA, ftXML, ftFpt
  );
var
  I			: Byte;
  B			: Word;
begin
  for I := 0 to 15 do begin
    B			:= 1 shl I;
    if Mask and B = B then begin
      Include(FileSet, FileTypes[I]);
      Mask		:= Mask - B;
    end;
  end;

  Result		:= Mask = 0;

end;

(*------------------------------------------------------------------------------
GuessFileTypeFromStream

------------------------------------------------------------------------------*)
function GuessFileTypeFromStream(
  const Stream		: TStream
) : TLetoFileType;
var
  Ver			: String;
  I			: Integer;
  Byte1, Byte2		: Cardinal;
const
  FILE_MINSIZE		= 56; // No GFF or ERF should ever be smaller than this.
begin

  Result		:= ftUnknown;

  if (Stream = nil) or (Stream.Size < FILE_MINSIZE) then
    Exit;

  Stream.Seek(0, soFromBeginning);

  // Try the short heuristics, first.
  //

  Result		:= GuessFileTypeFromSig(ReadString(Stream, 4));
  if Result <> ftUnknown then
    Exit;

  // Advanced heuristics.
  //

  // If the first char of Ver isn't 'V', this is probably not an
  // NWN file at all.
  Ver			:= ReadString(Stream, 4);
  if (Ver = '') or (Ver[1] <> 'V') then Exit;

  // Probably a GFF
  Stream.Seek(48, soFromBeginning);
  Byte1			:= ReadLong(Stream);
  Byte2			:= ReadLong(Stream);
  if Stream.Size = Int64(Byte1) + Int64(Byte2) then begin
    Result		:= ftGFF;
    Exit;
  end;

  // Probably an ERF
  Stream.Seek(44, soFromBeginning);
  for I := 1 to 29 do begin
    Byte1		:= ReadLong(Stream);
    if Byte1 <> 0 then break;
  end;
  if I = 29 then
    Result		:= ftERF;

  // No idea...

end;

(*------------------------------------------------------------------------------
GuessFileTypeFromFile

------------------------------------------------------------------------------*)
function GuessFileTypeFromFile(
  const FileName	: String
) : TLetoFileType;
var
  FileStream		: TMemoryStream;
begin
  Result		:= ftUnknown;

  if not FileExists(FileName) then Exit;

  FileStream		:= TMemoryStream.Create;

  try
    try
      FileStream.LoadFromFile(FileName);
      Result		:= GuessFileTypeFromStream(FileStream);
    except
      on EFOpenError do begin
        Exit;
      end;
    end;
  finally
    FreeAndNil(FileStream);
  end;

end;

(*------------------------------------------------------------------------------
GuessFileTypeFromSig

------------------------------------------------------------------------------*)
function GuessFileTypeFromSig(
  const Signature	: String
) : TLetoFileType;
var
  Sig			: String;
  Value			: Integer;
begin
  Sig			:= Lowercase(Trim(Signature));

  Value			:= GetEnumValue(TypeInfo(TLetoFileType), 'ft' + Sig);

  if Value > -1 then
    Result		:= TLetoFileType(Value)
  else
    Result		:= ftUnknown;

end;

(*------------------------------------------------------------------------------
TryStrToFileType

------------------------------------------------------------------------------*)
function TryStrToFileType(
  const TypeName	: String;
  var FileType		: TLetoFileType
): Boolean;
var
  I			: Integer;
begin

  I			:= GetEnumValue(TypeInfo(TLetoFileType), 'ft' + TypeName);

  Result		:= I > -1;

  if Result then
    FileType		:= TLetoFileType(I);

end;

(*------------------------------------------------------------------------------
IsGffStream

------------------------------------------------------------------------------*)
function IsGffStream(
  const Stream	: TStream
) : Boolean;
begin
  Result		:= IsGffType(GuessFileTypeFromStream(Stream));
end;

(*------------------------------------------------------------------------------
IsGffFile

------------------------------------------------------------------------------*)
function IsGffFile(
  const FileName	: String
) : Boolean;
begin
  Result		:= IsGffType(GuessFileTypeFromFile(FileName));
end;

(*------------------------------------------------------------------------------
IsGffType

------------------------------------------------------------------------------*)
function IsGffType(
  const FileType	: TLetoFileType
): Boolean;
begin
  Result		:= FileType in GffFileTypes;
end;

function IsGffType(
  const FileName	: TFileName
): Boolean;
var
  S			: String;
  FileType		: TLetoFileType;
begin
  S			:= ExtractFileExt(FileName);
  S			:= Copy(S, 2, Length(S));
  Result		:= TryStrToFileType(S, FileType) and IsGffType(FileType);
end;

function IsGffType(
  const ResTypeId	: Word
): Boolean;
var
  S			: String;
  FileType		: TLetoFileType;
begin
  S			:= GetResTypeName(ResTypeId);
  Result		:= TryStrToFileType(S, FileType) and IsGffType(FileType);
end;

(*------------------------------------------------------------------------------
IsErfStream

------------------------------------------------------------------------------*)
function IsErfStream(
  const Stream	: TStream
) : Boolean;
begin
  Result		:= IsErfType(GuessFileTypeFromStream(Stream));
end;

(*------------------------------------------------------------------------------
IsErfFile

------------------------------------------------------------------------------*)
function IsErfFile(
  const FileName	: String
) : Boolean;
begin
  Result		:= IsErfType(GuessFileTypeFromFile(FileName));
end;

(*------------------------------------------------------------------------------
IsErfType

------------------------------------------------------------------------------*)
function IsErfType(
  const FileType	: TLetoFileType
) : Boolean;
begin
  Result		:= FileType in ErfFileTypes;
end;

(*------------------------------------------------------------------------------
GetResTypeName

Given an Integer corresponding to one of the ResType constants, returns the
string (three-letter-acronym) that describes that resource type. If not match
is found, the INVALID ResType is returned, which is represented as '???'.

Current notes:

------------------------------------------------------------------------------*)
function GetResTypeName(
  const ResTypeId	: Word
) : String;
var
  I			: Integer;
begin
  // Default Result is INVALID
  Result		:= ResTypes[High(ResTypes)].ResType;

  for I := Low(ResTypes) to High(ResTypes) do
    if ResTypeId = ResTypes[I].ResId then
      Result		:= ResTypes[I].ResType;

end;

(*------------------------------------------------------------------------------
GetResTypeId

Performs the complement of GetResTypeName - takes a String (presumed to be
the sig in the first four bytes of a resource Struct in an ERF - though it
could also be a file extension), and returns the ResTypeId (WORD) const
matching that resource type. If no match is found, the INVALID ResType
is returned, which is 0xFFFF.

Current notes:

------------------------------------------------------------------------------*)
function GetResTypeId(
  const ResType		: String
) : Word;
var
  I			: Integer;
  Name			: String;
begin
  // Default Result is INVALID
  Result		:= ResTypes[High(ResTypes)].ResId;

  Name			:= Uppercase(ResType);

  for I := Low(ResTypes) to High(ResTypes) do
    if Name = ResTypes[I].ResType then
      Result		:= ResTypes[I].ResId;

end;

(*------------------------------------------------------------------------------
TryStrToFieldType

------------------------------------------------------------------------------*)
function TryStrToFieldType(
  TypeName		: String;
  var VarType		: TGffVarType
) : Boolean;
var
  Value			: Integer;
begin
  TypeName		:= 'gff' + Lowercase( Trim(TypeName) );
  Value			:= GetEnumValue(TypeInfo(TGffVarType), TypeName);
  Result		:= Value > -1;
  if Result then
    VarType		:= TGffVarType(Value);
end;

(*------------------------------------------------------------------------------
FieldTypeToStr

Get the Stringified name of a TGffVarType

------------------------------------------------------------------------------*)
function FieldTypeToStr(
  FieldType		: TGffVarType;
  Truncate		: Boolean
) : String;
begin

  Result		:= GetEnumName(TypeInfo(TGffVarType), Integer(FieldType));

  if Truncate then
    Result		:= StringReplace(Result, 'gff', '', [rfReplaceAll]);

end;

(*------------------------------------------------------------------------------
TryStrToGffLoadLevel

Convert a string to a TGffLoadLevel. Returns False if the conversion failed
(but assigns LoadFull to LoadLevel, as a fallback).

------------------------------------------------------------------------------*)
function TryStrToGffLoadLevel(
  const LevelName	: String;
  var LoadLevel		: TGffLoadLevel
) : Boolean;
var
  I			: Integer;
begin
  I			:= GetEnumValue(TypeInfo(TGffLoadLevel), 'load' + LevelName);

  Result		:= I > -1;
  if Result then
    LoadLevel		:= TGffLoadLevel(I)
  else
    LoadLevel		:= LoadFull;

end;

(*------------------------------------------------------------------------------
ConvertPath

Leto tries to support Windows users using slashes in a file's path, e.g.,

c:/nwn/servervault

This is an ease-of-use function mostly, since \ signals an escape character
in any interpreted string. So you'd have to write:

c:\\nwn\\servervault

This function converts / to \, so that Delphi / Windows functions which are
picky about this will get what they want.

------------------------------------------------------------------------------*)
function ConvertPath(const FileName: String): String;
begin
{$IFDEF MSWINDOWS}
  Result		:= StringReplace(FileName, '/', '\', [rfReplaceAll]);
{$ELSE}
  Result		:= FileName;
{$ENDIF}
end;

(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
(*------------------------------------------------------------------------------
------------------------------------------------------------------------------*)
end.
