(*------------------------------------------------------------------------------
Class_LetoXml

Various parts of Leto require some kind of "native" XML access, this is it.
It was originally based on a visual component by Sébastien Buysse
[sbuysse@buypin.com], which was part of the JEDI JVCL. I made the code non-
visual, streamlined, and added support for whitespace preservation.

This program is free software, under the terms of the BSD license. See the
included License.txt for details, or contact dragon@weathersong.net if you
need more information.

------------------------------------------------------------------------------*)

unit Class_LetoXml;

interface

uses
  // TODO 3: CHANGE: Remove IniFiles dependency (THashedStringList)
  Classes, SysUtils, IniFiles,
  Header_Leto;

type

  // Forward
  TLetoXmlElem		= class;
  TLetoXmlElemHeader	= class;
  TLetoXmlElemDocType	= class;
  TLetoXmlElemSheet	= class;
  TLetoXmlElemComment	= class;
  TLetoXmlElemCData	= class;
  TLetoXmlElemText	= class;
  TLetoXmlElems		= class;
  TLetoXmlElemsProlog	= class;
  TLetoXmlProp		= class;
  TLetoXmlProps		= class;
  TLetoXml		= class;

  TSortMethod		= (
    SortByValue, SortByProp
  );


  TLetoXmlElem		= class

  private

    FName		: String;
    FParent		: TLetoXmlElem;
    FRoot		: TLetoXmlElem;
    FItems		: TLetoXmlElems;
    FProps		: TLetoXmlProps;
    FValue		: String;
    FPointer		: String;
    FData		: Pointer;

    FLine		: Cardinal;

  protected

    procedure SetName(const Value: string);

    function GetItems: TLetoXmlElems;
    function GetItem(const Index: Integer): TLetoXmlElem;

    function GetProps: TLetoXmlProps;

    function GetIntValue: Int64;
    procedure SetIntValue(const Value: Int64);

    function GetBoolValue: Boolean;
    procedure SetBoolValue(const Value: Boolean);

  public

    property Name: String read FName write SetName;
    property Parent: TLetoXmlElem read FParent write FParent;
    property Root: TLetoXmlElem read FRoot write FRoot;
    property Pointer: String read FPointer write FPointer;
    property Items: TLetoXmlElems read GetItems;
    property Item[const Index: Integer]: TLetoXmlElem read GetItem; default;
    property Props: TLetoXmlProps read GetProps;

    property AsInt: Int64 read GetIntValue write SetIntValue;
    property AsBool: Boolean read GetBoolValue write SetBoolValue;
    property Value: String read FValue write FValue;
    property Data: Pointer read FData write FData;

    property LineNum: Cardinal read FLine;

    constructor Create(const AOwner, ARoot: TLetoXmlElem);
    destructor Destroy; override;

    procedure Clear; virtual;

    function Find(Name, Prop, Path: String): TLetoXmlElem;

    function LoadFromStream(
      const Stream	: TStream;
      const TrimSpace	: Boolean
    ): Boolean; virtual;

    procedure SaveToStream(
      const Stream	: TStream;
      const IndentStr	: String = ' ';
      const Level	: Integer = 0
    ); virtual;

    function SaveToString: String;

    function Establish(
      const ItemNamed	: String;
      const Default	: String = ''
    ): TLetoXmlElem; overload;

    function Establish(
      const ItemNamed	: String;
      const PropValue	: String;
      const Default	: String;
      const PropNamed	: String = 'name';
      const EstProp	: String = 'value'
    ): String; overload;

    function HasProp(const PropName: String): Boolean;

  end;

  TLetoXmlElemHeader	= class(TLetoXmlElem)

  private

    FStandalone		: Boolean;
    FEncoding		: String;
    FVersion		: String;

  public

    property Version: string read FVersion write FVersion;
    property Standalone: Boolean read FStandalone write FStandalone;
    property Encoding: string read FEncoding write FEncoding;

    constructor Create;

    function LoadFromStream(
      const Stream	: TStream;
      const TrimSpace	: Boolean
    ):Boolean; override;

    procedure SaveToStream(
      const Stream	: TStream;
      const IndentStr	: String = ' ';
      const Level	: Integer = 0
    ); override;

  end;

  TLetoXmlElemDocType	= class(TLetoXmlElem)
  public
    function LoadFromStream(
      const Stream	: TStream;
      const TrimSpace	: Boolean
    ):Boolean; override;
    procedure SaveToStream(
      const Stream	: TStream;
      const IndentStr	: String = ' ';
      const Level	: Integer = 0
    ); override;
  end;

  TLetoXmlElemSheet = class(TLetoXmlElem)
  public
    function LoadFromStream(
      const Stream	: TStream;
      const TrimSpace	: Boolean
    ):Boolean; override;
    procedure SaveToStream(
      const Stream	: TStream;
      const IndentStr	: String = ' ';
      const Level	: Integer = 0
    ); override;
  end;

  TLetoXmlElemComment	= class(TLetoXmlElem)
  public
    function LoadFromStream(
      const Stream	: TStream;
      const TrimSpace	: Boolean
    ):Boolean; override;
    procedure SaveToStream(
      const Stream	: TStream;
      const IndentStr	: String = ' ';
      const Level	: Integer = 0
    ); override;
  end;

  {
  TLetoXmlElemClassic = class(TLetoXmlElem)
  public
    function LoadFromStream(
      const Stream	: TStream
    ):Boolean; override;
    procedure SaveToStream(
      const Stream	: TStream;
      const IndentStr	: String = ' ';
      const Level	: Integer = 0
    ); override;
  end;
  }

  TLetoXmlElemCData	= class(TLetoXmlElem)
  public
    function LoadFromStream(
      const Stream	: TStream;
      const TrimSpace	: Boolean
    ):Boolean; override;
    procedure SaveToStream(
      const Stream	: TStream;
      const IndentStr	: String = ' ';
      const Level	: Integer = 0
    ); override;
  end;

  TLetoXmlElemText	= class(TLetoXmlElem)
  public
    function LoadFromStream(
      const Stream	: TStream;
      const TrimSpace	: Boolean
    ):Boolean; override;
    procedure SaveToStream(
      const Stream	: TStream;
      const IndentStr	: String = ' ';
      const Level	: Integer = 0
    ); override;
  end;

  TLetoXmlElems		= class

  private

    FElems		: THashedStringList;
    FParent		: TLetoXmlElem;
    FRoot		: TLetoXmlElem;

    function GetCount: Integer;

    function GetItemNamed(const Name: string): TLetoXmlElem;

  protected

    function GetItem(const Index: Integer): TLetoXmlElem;

    procedure AddChild(const Value: TLetoXmlElem);

    procedure AddChildFirst(const Value: TLetoXmlElem);

    procedure DoItemRename(var Value: TLetoXmlElem; const Name: string);

    procedure CreateElems;

  public

    property Parent: TLetoXmlElem read FParent write FParent;
    property Root: TLetoXmlElem read FRoot write FRoot;
    property Item[const Index: Integer]: TLetoXmlElem read GetItem; default;
    property ItemNamed[const Name: String]: TLetoXmlElem read GetItemNamed;
    property Count: Integer read GetCount;

    constructor Create(const AOwner, ARoot: TLetoXmlElem);
    destructor Destroy; override;

    function Add(
      const Name	: String
    ): TLetoXmlElem; overload;
    function Add(
      const Name, Value	: String
    ): TLetoXmlElem; overload;
    function Add(
      const Name	: String;
      const Value	: Int64
    ): TLetoXmlElem; overload;
    function Add(
      const Name	: String;
      const Value	: Boolean
    ): TLetoXmlElem; overload;
    function Add(
      const Name	: String;
      const Value	: TStream
    ): TLetoXmlElem; overload;
    function Add(
      Value		: TLetoXmlElem
    ): TLetoXmlElem; overload;

    function AddFirst(Value: TLetoXmlElem): TLetoXmlElem; overload;
    function AddFirst(const Name: string): TLetoXmlElem; overload;

    function AddComment(
      const Name	: String;
      const Value	: String
    ): TLetoXmlElemComment;

    function AddCData(
      const Name	: String;
      const Value	: String
    ): TLetoXmlElemCData;

    function AddText(
      const Name	: String;
      const Value	: String
    ): TLetoXmlElemText;

    procedure Clear; virtual;

    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: string); overload;

    function Value(const Name: String; Default: String = ''): String;
    function AsInt(const Name: String; Default: Int64 = -1): Int64;
    function AsBool(const Name: String; Default: Boolean = True): Boolean;

    function Find(const Named, Prop, Value: String): TLetoXmlElem;

    function LoadFromStream(
      const Stream	: TStream;
      const TrimSpace	: Boolean
    ): String;

    procedure SaveToStream(
      const Stream	: TStream;
      const IndentStr	: String = ' ';
      const Level	: Integer = 0
    );

    procedure Sort(
      const Method	: TSortMethod;
      const Prop	: String = '';
      const Alternate	: String = ''
    );

  end;

  TLetoXmlElemsProlog	= class

  private

    FRoot		: TLetoXmlElem;
    FElems		: THashedStringList;

    function GetCount: Integer;

    function GetItem(const Index: Integer): TLetoXmlElem;

  public

    property Root: TLetoXmlElem read FRoot write FRoot;
    property Item[const Index: Integer]: TLetoXmlElem read GetItem; default;
    property Count: Integer read GetCount;

    constructor Create(const ARoot: TLetoXmlElem);
    destructor Destroy; override;

    procedure Clear;

    function LoadFromStream(
      const Stream	: TStream;
      const TrimSpace	: Boolean
    ): Boolean;
    procedure SaveToStream(const Stream: TStream);

  end;

  TLetoXmlProp		= class

  private

    FName		: String;
    FValue		: String;
    FParent		: TLetoXmlProps;
    FPointer		: String;
    FData		: Pointer;

    procedure SetName(const Value: string);

    function GetBoolValue: Boolean;
    procedure SetBoolValue(const Value: Boolean);
    function GetCharValue: Char;
    procedure SetCharValue(const Value: Char);

  protected

    function GetIntValue: Int64;
    procedure SetIntValue(const Value: Int64);

  public

    property Parent: TLetoXmlProps read FParent write FParent;
    property Name: string read FName write SetName;
    property Value: string read FValue write FValue;
    property AsInt: Int64 read GetIntValue write SetIntValue;
    property AsBool: Boolean read GetBoolValue write SetBoolValue;
    property AsChar: Char read GetCharValue write SetCharValue;
    property Pointer: string read FPointer write FPointer;
    property Data: Pointer read FData write FData;

    function SaveToString: string;

  end;

  TLetoXmlProps		= class

  private

    FProps		: THashedStringList;
    FAnon		: TLetoXmlProp;

    function GetCount: Integer;
    function GetItemNamed(const Name: string): TLetoXmlProp;

  protected

    function GetItem(const Index: Integer): TLetoXmlProp;
    procedure DoItemRename(var Value: TLetoXmlProp; const Name: string);

  public

    property Item[const Index: Integer]: TLetoXmlProp read GetItem;
    property ItemNamed[const Name: string]: TLetoXmlProp read GetItemNamed; default;
    property Count: Integer read GetCount;

    constructor Create;
    destructor Destroy; override;

    function Add(const Name, Value: string): TLetoXmlProp; overload;
    function Add(const Name: string; const Value: Int64): TLetoXmlProp; overload;
    function Add(const Name: string; const Value: Boolean): TLetoXmlProp; overload;

    procedure Clear; virtual;

    procedure Delete(const Index: Integer); overload;
    procedure Delete(const Name: string); overload;

    function Value(const Name: string; Default: string = ''): string;
    function AsInt(const Name: string; Default: Int64 = -1): Int64;
    function AsBool(const Name: string; Default: Boolean = True): Boolean;

    function LoadFromStream(const Stream: TStream): Boolean;
    procedure SaveToStream(const Stream: TStream);

    function Establish(const PropName, PropValue: String): TLetoXmlProp;

  end;

  TLetoXml		= class

  private

    FFileName		: TFileName;
    FRoot		: TLetoXmlElem;
    FProlog		: TLetoXmlElemsProlog;
    FIndStr		: String;

    FTrim		: Boolean;

  protected

    procedure SetFileName(Value: TFileName);

  public

    property FileName: TFileName read FFileName write SetFileName;
    property Root: TLetoXmlElem read FRoot write FRoot;
    property Prolog: TLetoXmlElemsProlog read FProlog write FProlog;

    property IndentStr: String read FIndStr write FIndStr;
    property TrimSpace: Boolean read FTrim write FTrim;

    constructor Create(
      const AFileName	: TFileName = '';
      const ATrimSpace	: Boolean = True
    );
    destructor Destroy; override;

    procedure LoadFromString(
      const Value	: String;
      const ATrimSpace	: Boolean = True
    );

    function LoadFromFile(
      const AFileName	: TFileName;
      const ATrimSpace	: Boolean = True
    ): Boolean;

    function LoadFromStream(
      const Stream	: TStream;
      const ATrimSpace	: Boolean = True
    ): Boolean;

    procedure SaveToFile(FileName: TFileName);

    procedure SaveToStream(const Stream: TStream);

    function SaveToString: String;

    procedure FakeFileName(const AFileName: TFileName);

  end;


implementation

const
  cBufferSize		= 8192;

function SimpleXmlEncode(const Value: string): String;
var
  I: Integer;
  lDiff: Boolean;
begin
  //http://www.cs.tut.fi/~jkorpela/latin1/3.html#60
  Result := Value;
  lDiff := False;
  for I := 1 to Length(Value) do
    if Value[I] in ['<', '>', '&', '"', ''''] then begin
      if not lDiff then begin
        lDiff := True;
        Result := Copy(Value, 1, I - 1);
      end;
      Result := Result + '&#' + IntToStr(Ord(Value[I])) + ';';
    end else if lDiff then
      Result := Result + Value[I];
end;

procedure SimpleXmlDecode(var Value: string; TrimMultiple: Boolean = True);
var
  I, J, K, L		: Integer;
  St			: String;
begin

  St			:= '';
  J			:= -1;
  K			:= 1;

  for I := 1 to Length(Value) do
    case Value[I] of
      ' ', #10, #13:
        if
          (not TrimMultiple) or
          ((K = 1) or not (Value[K - 1] in [' ', #10, #13]))
        then begin
          if J > 0 then
            St := St + Value[I]
          else begin
            Value[K]	:= Value[I];
            Inc(K);
          end;
        end;

      '&':
        begin
          if J <> -1 then begin
            Value[K] := '&';
            Inc(K);
            for L := 1 to Length(St) do begin
              Value[K] := St[L];
              Inc(K);
            end;
          end;
          J := 0;
          St := '';
        end;

      '#':
        if J = 0 then
          J := 1
        else if J <> -1 then begin
          Value[K] := '&';
          Inc(K);
          for L := 1 to Length(St) do begin
            Value[K] := St[L];
            Inc(K);
          end;
          Value[K] := Value[I];
          Inc(K);
          St := '';
        end else begin
          for L := 1 to Length(St) do begin
            Value[K] := St[L];
            Inc(K);
          end;
          Value[K] := Value[I];
          Inc(K);
          St := '';
        end;

      '0'..'9':
        if J >= 1 then begin
          St := St + Value[I];
          J := 2;
        end else begin
          Value[K] := Value[I];
          Inc(K);
        end;

      'a'..'z', 'A'..'Z':
        if J >= 0 then
        begin
          St := St + Value[I];
          Inc(J);
        end else begin
          Value[K] := Value[I];
          Inc(K);
        end;

      ';':
        if J <> 0 then begin
          J := StrToIntDef(St, -1);
          case J of
            -1:
              begin
                St := LowerCase(St);
                if St = 'lt' then begin
                  Value[K] := '<';
                  Inc(K);
                end else
                if St = 'gt' then begin
                  Value[K] := '>';
                  Inc(K);
                end else if J >= 0 then begin
                  Value[K] := '&';
                  Inc(K);
                  for L := 1 to Length(St) do begin
                    Value[K] := St[L];
                    Inc(K);
                  end;
                  Value[K] := ';';
                  Inc(K);
                end else begin
                  Value[K] := ';';
                  Inc(K);
                end
              end;
            0..100:
              begin
                Value[K] := Char(J);
                Inc(K);
              end;
            233:
              begin
                Value[K] := 'é';
                Inc(K);
              end;
            232:
              begin
                Value[K] := 'è';
                Inc(K);
              end;
          end;
          St := '';
          J := -1;
        end else begin
          Value[K] := Value[I];
          Inc(K);
        end;

    else
      begin
        if J > 0 then begin
          Value[K] := '&';
          Inc(K);
          for L := 1 to Length(St) do begin
            Value[K] := St[L];
            Inc(K);
          end;
        end else if J = 0 then begin
          Value[K] := '&';
          Inc(K);
        end;
        Value[K] := Value[I];
        Inc(K);
        J := -1;
      end;

    end;

  if J <> -1 then begin
    Value[K] := '&';
    Inc(K);
    for L := 1 to Length(St) do begin
      Value[K] := St[L];
      Inc(K);
    end;
  end;

  SetLength(Value, K - 1);

end;


{ TLetoXmlElem }


(*------------------------------------------------------------------------------
property Name

------------------------------------------------------------------------------*)
procedure TLetoXmlElem.SetName(const Value: string);
begin
  if (Value <> FName) and (Value <> '') then begin
    if (Parent <> nil) and (FName <> '') then
      Parent.Items.DoItemRename(Self, Value);
    FName		:= Value;
  end;
end;

(*------------------------------------------------------------------------------
property Items

------------------------------------------------------------------------------*)
function TLetoXmlElem.GetItems: TLetoXmlElems;
begin
  if not Assigned(FItems) then
    FItems		:= TLetoXmlElems.Create(Self, Root);

  Result		:= FItems;
end;

(*------------------------------------------------------------------------------
property Item

------------------------------------------------------------------------------*)
function TLetoXmlElem.GetItem(const Index: Integer): TLetoXmlElem;
begin
  Result		:= FItems[Index];
end;

(*------------------------------------------------------------------------------
property Props

------------------------------------------------------------------------------*)
function TLetoXmlElem.GetProps: TLetoXmlProps;
begin
  if not Assigned(FProps) then
    FProps		:= TLetoXmlProps.Create();
  Result		:= FProps;
end;

(*------------------------------------------------------------------------------
property AsInt

------------------------------------------------------------------------------*)
function TLetoXmlElem.GetIntValue: Int64;
begin
  Result		:= StrToInt64Def(Value, -1);
end;

procedure TLetoXmlElem.SetIntValue(const Value: Int64);
begin
  FValue		:= IntToStr(Value);
end;

(*------------------------------------------------------------------------------
property AsBool

------------------------------------------------------------------------------*)
function TLetoXmlElem.GetBoolValue: Boolean;
begin
  Result		:= StrToBoolDef(Value, False);
end;

procedure TLetoXmlElem.SetBoolValue(const Value: Boolean);
begin
  FValue		:= BoolToStr(Value);
end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoXmlElem.Create(const AOwner, ARoot: TLetoXmlElem);
begin
  inherited Create;
  FName			:= '';
//  FParent		:= TLetoXmlElem(AOwner);
  FParent		:= AOwner;
  FRoot			:= ARoot;
end;

(*------------------------------------------------------------------------------
destroy

------------------------------------------------------------------------------*)
destructor TLetoXmlElem.Destroy;
begin
  Clear;

  if Assigned(FItems) then
    FreeAndNil(FItems);
  if Assigned(FProps) then
    FreeAndNil(FProps);

  inherited Destroy;
end;

(*------------------------------------------------------------------------------
Clear

------------------------------------------------------------------------------*)
procedure TLetoXmlElem.Clear;
begin
  if Assigned(FItems) then
    FItems.Clear;
  if Assigned(FProps) then
    FProps.Clear;
end;

(*------------------------------------------------------------------------------
Find

Like Elems.Find, but this performs a recursive search with support for a
pathing syntax.

In Path, / indicates a child element named Name (or '*' for any), with a
property Prop, with a value described between the slashes.

E.g., Find('section', 'name', '/Help/Info/About') will start at the root
node, look for the node <section name="Help"> and then pass to that node:
Find('section', 'name', 'Info/About'), so that node looks for a child node
<section name="Info">, etc. If Name were '*', the first, intermediate, and
last nodes could have different names - useful when the last node is a stub
such as <article name="About">

------------------------------------------------------------------------------*)
function TLetoXmlElem.Find(Name, Prop, Path: String): TLetoXmlElem;
var
  P			: Integer;
begin
  Result		:= nil;

  if Length(Path) < 1 then
    Exit;

  if Path[1] = '/' then begin
    System.Delete(Path, 1, 1);
    if Root <> self then begin
      Result		:= Root.Find(Name, Prop, Path);
      Exit;
    end;
  end;

  P			:= Pos('/', Path);
  if P = 0 then
    Result		:= Items.Find(Name, Prop, Path)
  else begin
    Result		:= Items.Find(Name, Prop, Copy(Path, 1, P-1));
    if Assigned(Result) then
      Result		:= Result.Find(Name, Prop, Copy(Path, P+1, Length(Path)));
  end;

end;

(*------------------------------------------------------------------------------
LoadFromStream

------------------------------------------------------------------------------*)
function TLetoXmlElem.LoadFromStream(
  const Stream		: TStream;
  const TrimSpace	: Boolean
): Boolean;
var
  I, lStreamPos, Count,
    lPos		: Integer;
  lBuf			: array [0..cBufferSize-1] of Char;
  St, lName, lValue,
    lPointer		: String;
begin

  lStreamPos		:= Stream.Position;
  St			:= '';
  lValue		:= '';
  lPointer		:= '';
  lPos			:= 1;

  Result		:= False;

  repeat

    Count := Stream.Read(lBuf, SizeOf(lBuf));

    for I := 0 to Count-1 do begin

      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of

        1:
          if lBuf[I] = '<' then
            lPos := 2
          else
            Exit;

        -1:
          if lBuf[I] = '>' then begin
            Count := 0;
            Break;
          end else
            Exit;

      else
        if lBuf[I] in [' ', #9, #10, #13] then begin
          if lPos = 2 then
            Exit;
          Stream.Seek(lStreamPos, soFromBeginning);
          if not Props.LoadFromStream(Stream) then
            Exit;
          lStreamPos := Stream.Position;
          Break; //Re read buffer
        end else
        case lBuf[I] of
          '>':
            begin
              lName := St;
              //Load elements
              Stream.Seek(lStreamPos, soFromBeginning);
              St := Items.LoadFromStream(Stream, TrimSpace);
              if lName <> St then
                Exit;
              lStreamPos := Stream.Position;

              //Set value if only one sub element
              //This might reduce speed, but this is for compatibility issues
              if (Items.Count = 1) and (Items[0] is TLetoXmlElemText) then begin
                lValue := Items[0].Value;
                Items.Clear;
              end;

              Count := 0;
              Break;
            end;
          '/':
            begin
              lName := St;
              lPos := -1;
            end;
          ':':
            begin
              lPointer := St;
              St := '';
            end;
        else
          begin
            St := St + lBuf[I];
            Inc(lPos);
          end;
        end;  // case lBuf

      end; // case lPos

    end; // for I

  until Count = 0;

  Name			:= lName;

  SimpleXmlDecode(lValue, TrimSpace);

  Value			:= lValue;
  Pointer		:= lPointer;

  Result		:= True;

  Stream.Seek(lStreamPos, soFromBeginning);

end;

(*------------------------------------------------------------------------------
SaveToStream

------------------------------------------------------------------------------*)
procedure TLetoXmlElem.SaveToStream(
  const Stream		: TStream;
  const IndentStr	: String;
  const Level		: Integer
);
var
  St			: String;
begin
  St			:= StringRepeat(IndentStr, Level) + '<' + Name;

  Stream.Write(St[1], Length(St));

  Props.SaveToStream(Stream);

  if Items.Count = 0 then begin
    if Value = '' then
      St := '/>' + sLineBreak
    else
      St := '>' + SimpleXmlEncode(Value) + '</' + Name + '>' + sLineBreak;
    Stream.Write(St[1], Length(St));
  end else begin
    St			:= '>' + sLineBreak;
    Stream.Write(St[1], Length(St));
    Items.SaveToStream(Stream, IndentStr, Level + 1);
    St := StringRepeat(IndentStr, Level) + '</' + Name + '>' + sLineBreak;
    Stream.Write(St[1], Length(St));
  end;

end;

(*------------------------------------------------------------------------------
SaveToString

------------------------------------------------------------------------------*)
function TLetoXmlElem.SaveToString: string;
var
  Stream		: TStringStream;
begin
  Stream		:= TStringStream.Create('');
  try
    SaveToStream(Stream);
    Result		:= Stream.DataString;
  finally
    Stream.Free;
  end;
end;

(*------------------------------------------------------------------------------
Establish

A convention for Items.ItemNamed that guarantees a Result. If the Item named
exists, that Item is returned. If it does not exist, it is created, and the
new Child is returned.

This is intended to be similar to IniFile functionality.

------------------------------------------------------------------------------*)
function TLetoXmlElem.Establish(
  const ItemNamed	: String;
  const Default		: String
): TLetoXmlElem;
begin
  Result		:= Items.ItemNamed[ItemNamed];
  if not Assigned(Result) then
    Result		:= Items.Add(ItemNamed, Default);
end;

function TLetoXmlElem.Establish(
  const ItemNamed, PropValue, Default, PropNamed,
    EstProp		: String
): String;
var
  Node			: TLetoXmlElem;
begin
  Node			:= Items.Find(ItemNamed, PropNamed, PropValue);
  if not Assigned(Node) then begin
    Node		:= Items.Add(ItemNamed);
    Node.Props.Add(PropNamed, PropValue);
  end;
  Result		:= Node.Props.Establish(EstProp, Default).Value;
end;

(*------------------------------------------------------------------------------
HasProp

Does this element have a prop named PropName?

------------------------------------------------------------------------------*)
function TLetoXmlElem.HasProp(const PropName: String): Boolean;
begin
  Result :=
    Assigned(FProps) and Assigned(FProps.FProps) and 
    ( FProps.FProps.IndexOf(PropName) > -1 );
end;


{ TLetoXmlElemHeader }


(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoXmlElemHeader.Create;
begin
  FVersion		:= '1.0';
  FEncoding		:= 'iso-8859-1';
  FStandalone		:= False;
end;

(*------------------------------------------------------------------------------
LoadFromStream

------------------------------------------------------------------------------*)
function TLetoXmlElemHeader.LoadFromStream(
  const Stream		: TStream;
  const TrimSpace	: Boolean
): Boolean;
const
  CS_START_HEADER = '<?xml';
  CS_STOP_HEADER = '     ?>';
var
  I, lStreamPos, Count,
    lPos		: Integer;
  lBuf			: array [0..cBufferSize-1] of Char;
  lOk			: Boolean;
begin

  lStreamPos		:= Stream.Position;
  lPos			:= 1;
  lOk			:= False;

  Result		:= False;

  repeat

    Count := Stream.Read(lBuf, SizeOf(lBuf));

    for I := 0 to Count - 1 do begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..4: //<?xml
          if lBuf[I] = CS_START_HEADER[lPos] then
            Inc(lPos)
          else
            Exit;
        5: //L
          if lBuf[I] = CS_START_HEADER[lPos] then begin
            Stream.Seek(lStreamPos, soFromBeginning);
            if not Props.LoadFromStream(Stream) then
              Exit;
            lStreamPos := Stream.Position;
            Inc(lPos);

            FVersion := Props.Value('version');
            FEncoding := Props.Value('encoding');
            FStandalone := Props.Value('standalone') = 'yes';

            Props.Clear;

            Break; //Re read buffer
          end else
            Exit;
        6: //?
          if lBuf[I] = CS_STOP_HEADER[lPos] then
            Inc(lPos)
          else
            Exit;
        7: //>
          if lBuf[I] = CS_STOP_HEADER[lPos] then
          begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end
          else
            Exit;
      end;
    end;
  until Count = 0;

  if not lOk then
    Exit;

  Result		:= True;
  Name			:= '';

  Stream.Seek(lStreamPos, soFromBeginning);

end;

(*------------------------------------------------------------------------------
SaveToStream

------------------------------------------------------------------------------*)
procedure TLetoXmlElemHeader.SaveToStream(
  const Stream		: TStream;
  const IndentStr	: String;
  const Level		: Integer
);
var
  St			: String;
begin
  St := StringRepeat(IndentStr, Level) + '<?xml version="' + FVersion + '"';
  if Standalone then
    St			:= St + ' standalone="yes"';
  if Encoding <> '' then
    St			:= St + ' encoding="' + Encoding + '"';
  St			:= St + '?>' + sLineBreak;
  Stream.Write(St[1], Length(St));
end;


{ TLetoXmlElemDocType }


(*------------------------------------------------------------------------------
LoadFromStream

------------------------------------------------------------------------------*)
function TLetoXmlElemDocType.LoadFromStream(
  const Stream		: TStream;
  const TrimSpace	: Boolean
): Boolean;
const
  CS_START_DOCTYPE = '<!DOCTYPE';
var
  I, lStreamPos, Count,
    lPos		: Integer;
  lBuf			: array [0..cBufferSize-1] of Char;
  lOk			: Boolean;
  lChar			: Char;
  St			: String;
begin
  lStreamPos		:= Stream.Position;
  lPos			:= 1;
  lOk			:= False;
  lChar			:= '>';
  St			:= '';

  Result		:= False;

  repeat

    Count := Stream.Read(lBuf, SizeOf(lBuf));

    for I := 0 to Count - 1 do begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..9: //<!DOCTYPE
          if lBuf[I] = CS_START_DOCTYPE[lPos] then
            Inc(lPos)
          else
            Exit;
        10: //]> or >
          if lChar = lBuf[I] then begin
            if lChar = '>' then begin
              lOk := True;
              Count := 0;
              Break; //This is the end
            end else begin
              St := St + lBuf[I];
              lChar := '>';
            end;
          end else begin
            St := St + lBuf[I];
            if lBuf[I] = '[' then
              lChar := ']';
          end;
      end;
    end;
  until Count = 0;

  if not lOk then
    Exit;

  Name			:= '';
  Value			:= Trim(St);
  Result		:= True;

  Stream.Seek(lStreamPos, soFromBeginning);
end;

(*------------------------------------------------------------------------------
SaveToStream

------------------------------------------------------------------------------*)
procedure TLetoXmlElemDocType.SaveToStream(
  const Stream		: TStream;
  const IndentStr	: String;
  const Level		: Integer
);
var
  St			: String;
begin
  St := StringRepeat(IndentStr, Level) + '<!DOCTYPE ' + Value + '>' + sLineBreak;
  Stream.Write(St[1], Length(St));
end;


{ TLetoXmlElemSheet }


(*------------------------------------------------------------------------------
LoadFromStream

------------------------------------------------------------------------------*)
function TLetoXmlElemSheet.LoadFromStream(
  const Stream		: TStream;
  const TrimSpace	: Boolean
): Boolean;
const
  CS_START_PI = '<?xml-stylesheet';
  CS_STOP_PI = '                ?>';
var
  I, lStreamPos, Count,
    lPos		: Integer;
  lBuf			: array [0..cBufferSize-1] of Char;
  lOk			: Boolean;
begin
  lStreamPos		:= Stream.Position;
  lPos			:= 1;
  lOk			:= False;

  Result		:= False;

  repeat

    Count := Stream.Read(lBuf, SizeOf(lBuf));

    for I := 0 to Count - 1 do begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..15: //<?xml-stylesheet
          if lBuf[I] = CS_START_PI[lPos] then
            Inc(lPos)
          else
            Exit;
        16: //L
          if lBuf[I] = CS_START_PI[lPos] then begin
            Stream.Seek(lStreamPos, soFromBeginning);
            if not Props.LoadFromStream(Stream) then
              Exit;
            lStreamPos := Stream.Position;
            Inc(lPos);
            Break; //Re read buffer
          end else
            Exit;
        17: //?
          if lBuf[I] = CS_STOP_PI[lPos] then
            Inc(lPos)
          else
            Exit;
        18: //>
          if lBuf[I] = CS_STOP_PI[lPos] then begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end else
            Exit;
      end;
    end;
  until Count = 0;

  if not lOk then
    Exit;

  Name			:= '';
  Result		:= True;

  Stream.Seek(lStreamPos, soFromBeginning);
end;

(*------------------------------------------------------------------------------
SaveToStream

------------------------------------------------------------------------------*)
procedure TLetoXmlElemSheet.SaveToStream(
  const Stream		: TStream;
  const IndentStr	: String;
  const Level		: Integer
);
var
  I			: Integer;
  St			: String;
begin
  St			:= StringRepeat(IndentStr, Level) + '<?xml-stylesheet';
  for I := 0 to Props.GetCount - 1 do
    St			:= St + Props.Item[I].SaveToString;
  St			:= St + '?>' + sLineBreak;

  Stream.Write(St[1], Length(St));
end;


{ TLetoXmlElemComment }


(*------------------------------------------------------------------------------
LoadFromStream

------------------------------------------------------------------------------*)
function TLetoXmlElemComment.LoadFromStream(
  const Stream		: TStream;
  const TrimSpace	: Boolean
): Boolean;
const
  CS_START_COMMENT = '<!--';
  CS_STOP_COMMENT = '    -->';
var
  I, lStreamPos, Count,
    lPos		: Integer;
  lBuf			: array [0..cBufferSize-1] of Char;
  St			: String;
  lOk			: Boolean;
begin
  lStreamPos		:= Stream.Position;
  St			:= '';
  lPos			:= 1;
  lOk			:= False;

  Result		:= False;

  repeat

    Count := Stream.Read(lBuf, SizeOf(lBuf));

    for I := 0 to Count - 1 do begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..4: //<!--
          if lBuf[I] = CS_START_COMMENT[lPos] then
            Inc(lPos)
          else
            Exit;
        5:
          if lBuf[I] = CS_STOP_COMMENT[lPos] then
            Inc(lPos)
          else
            St := St + lBuf[I];
        6: //-
          if lBuf[I] = CS_STOP_COMMENT[lPos] then
            Inc(lPos)
          else begin
            St := St + '-' + lBuf[I];
            Dec(lPos);
          end;
        7: //>
          if lBuf[I] = CS_STOP_COMMENT[lPos] then begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end else begin
            St := St + '--' + lBuf[I];
            Dec(lPos, 2);
          end;
      end;
    end;
  until Count = 0;

  if not lOk then
    Exit;

  Value			:= St;
  Name			:= '';
  Result		:= True;

  Stream.Seek(lStreamPos, soFromBeginning);
end;

(*------------------------------------------------------------------------------
SaveToStream

------------------------------------------------------------------------------*)
procedure TLetoXmlElemComment.SaveToStream(
  const Stream		: TStream;
  const IndentStr	: String;
  const Level		: Integer
);
var
  St			: String;
begin
  St			:= StringRepeat(IndentStr, Level) + '<!--';
  Stream.Write(St[1], Length(St));
  if Value <> '' then
    Stream.Write(Value[1], Length(Value));
  St			:= '-->' + sLineBreak;
  Stream.Write(St[1], Length(St));
end;


{ TLetoXmlElemCData }


(*------------------------------------------------------------------------------
LoadFromStream

------------------------------------------------------------------------------*)
function TLetoXmlElemCData.LoadFromStream(
  const Stream		: TStream;
  const TrimSpace	: Boolean
): Boolean;
const
  CS_START_CDATA = '<![CDATA[';
  CS_STOP_CDATA = '         ]]>';
var
  I, lStreamPos, Count,
    lPos		: Integer;
  lBuf			: array [0..cBufferSize-1] of Char;
  St			: String;
  lOk			: Boolean;
begin
  lStreamPos		:= Stream.Position;
  St			:= '';
  lPos			:= 1;
  lOk			:= False;

  Result		:= False;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    for I := 0 to Count - 1 do begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        1..9: //<![CDATA[
          if lBuf[I] = CS_START_CDATA[lPos] then
            Inc(lPos)
          else
            Exit;
        10:
          if lBuf[I] = CS_STOP_CDATA[lPos] then
            Inc(lPos)
          else
            St := St + lBuf[I];
        11: //-
          if lBuf[I] = CS_STOP_CDATA[lPos] then
            Inc(lPos)
          else begin
            St := St + ']' + lBuf[I];
            Dec(lPos);
          end;
        12: //>
          if lBuf[I] = CS_STOP_CDATA[lPos] then begin
            Count := 0; //End repeat
            lOk := True;
            Break; //End if
          end else begin
            St := St + ']]' + lBuf[I];
            Dec(lPos, 2);
          end;
      end;
    end;
  until Count = 0;

  if not lOk then
    Exit;

  Value			:= St;
  Name			:= '';
  Result		:= True;

  Stream.Seek(lStreamPos, soFromBeginning);
end;

(*------------------------------------------------------------------------------
SaveToStream

------------------------------------------------------------------------------*)
procedure TLetoXmlElemCData.SaveToStream(
  const Stream		: TStream;
  const IndentStr	: String;
  const Level		: Integer
);
var
  St			: String;
begin
  St			:= StringRepeat(IndentStr, Level) + '<![CDATA[';
  Stream.Write(St[1], Length(St));
  if Value <> '' then
    Stream.Write(Value[1], Length(Value));
  St			:= ']]>' + sLineBreak;
  Stream.Write(St[1], Length(St));
end;


{ TLetoXmlElemText }


(*------------------------------------------------------------------------------
LoadFromStream

------------------------------------------------------------------------------*)
function TLetoXmlElemText.LoadFromStream(
  const Stream		: TStream;
  const TrimSpace	: Boolean
): Boolean;
var
  I, lStreamPos, Count,
    lPos		: Integer;
  lBuf			: array [0..cBufferSize-1] of Char;
  St			: String;
begin
  lStreamPos		:= Stream.Position;
  St			:= '';
  lPos			:= 0;

  //Result		:= False;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    for I := 0 to Count - 1 do begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lBuf[I] of
        '<':
          begin
            //Quit text
            Dec(lStreamPos);
            Count := 0;
            Break;
          end;
        ' ':
          if lPos = 0 then begin
            if TrimSpace then
              Inc(lPos);
            St := St + ' ';
          end;
      else
        begin
          lPos := 0;
          St := St + lBuf[I];
        end;
      end;
    end;
  until Count = 0;

  SimpleXmlDecode(St, TrimSpace);

  Value			:= St;
  Name			:= '';
  Result		:= True;

  Stream.Seek(lStreamPos, soFromBeginning);
end;

(*------------------------------------------------------------------------------
SaveToStream

------------------------------------------------------------------------------*)
procedure TLetoXmlElemText.SaveToStream(
  const Stream		: TStream;
  const IndentStr	: String;
  const Level		: Integer
);
var
  St			: String;
begin
  if Value <> '' then begin
    St := StringRepeat(IndentStr, Level) + SimpleXmlEncode(Value) + sLineBreak;
    Stream.Write(St[1], Length(St));
  end;
end;


{ TLetoXmlElems }


(*------------------------------------------------------------------------------
property Count

------------------------------------------------------------------------------*)
function TLetoXmlElems.GetCount: Integer;
begin
  if Assigned(FElems) then
    Result		:= FElems.Count
  else
    Result		:= 0;
end;

(*------------------------------------------------------------------------------
property ItemNamed

------------------------------------------------------------------------------*)
function TLetoXmlElems.GetItemNamed(const Name: string): TLetoXmlElem;
var
  I			: Integer;
begin
  Result		:= nil;
  if not Assigned(FElems) then Exit;

  I			:= FElems.IndexOf(Name);
  if I > -1 then
    Result		:= TLetoXmlElem(FElems.Objects[I]);

end;

(*------------------------------------------------------------------------------
property Item

------------------------------------------------------------------------------*)
function TLetoXmlElems.GetItem(const Index: Integer): TLetoXmlElem;
begin
  if not Assigned(FElems) or (Index < 0) or (Index > FElems.Count-1) then
    Result		:= nil
  else
    Result		:= TLetoXmlElem(FElems.Objects[Index]);
end;

(*------------------------------------------------------------------------------
AddChild

------------------------------------------------------------------------------*)
procedure TLetoXmlElems.AddChild(const Value: TLetoXmlElem);
begin
  CreateElems;
  FElems.AddObject(Value.Name, Value);
end;

(*------------------------------------------------------------------------------
AddChildFirst

------------------------------------------------------------------------------*)
procedure TLetoXmlElems.AddChildFirst(const Value: TLetoXmlElem);
begin
  CreateElems;
  FElems.InsertObject(0, Value.Name, Value);
end;

(*------------------------------------------------------------------------------
DoItemRename

------------------------------------------------------------------------------*)
procedure TLetoXmlElems.DoItemRename(
  var Value		: TLetoXmlElem;
  const Name		: String
);
var
  I			: Integer;
begin
  I			:= FElems.IndexOfObject(Value);
  if I > -1 then
    FElems[I]		:= Name;
end;

(*------------------------------------------------------------------------------
CreateElems

------------------------------------------------------------------------------*)
procedure TLetoXmlElems.CreateElems;
begin
  if not Assigned(FElems) then
    FElems		:= THashedStringList.Create;
end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoXmlElems.Create(const AOwner, ARoot: TLetoXmlElem);
begin
  FParent		:= AOwner;
  FRoot			:= ARoot;
end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoXmlElems.Destroy;
begin
  Clear;
  if Assigned(FElems) then
    FreeAndNil(FElems);

  inherited Destroy;
end;

(*------------------------------------------------------------------------------
Add

------------------------------------------------------------------------------*)
function TLetoXmlElems.Add(const Name: String): TLetoXmlElem;
begin
  Result		:= TLetoXmlElem.Create(Parent, Root);
  Result.FName		:= Name;
  AddChild(Result);
end;

function TLetoXmlElems.Add(const Name, Value: String): TLetoXmlElem;
begin
  Result		:= TLetoXmlElem.Create(Parent, Root);
  Result.Name		:= Name;
  Result.Value		:= Value;
  AddChild(Result);
end;

function TLetoXmlElems.Add(
  const Name		: String;
  const Value		: Int64
): TLetoXmlElem;
begin
  Result		:= Add(Name, IntToStr(Value));
end;

function TLetoXmlElems.Add(
  const Name		: String;
  const Value		: Boolean
): TLetoXmlElem;
begin
  Result		:= Add(Name, BoolToStr(Value));
end;

function TLetoXmlElems.Add(
  const Name		: String;
  const Value		: TStream
): TLetoXmlElem;
var
  Stream		: TStringStream;
  Buf			: array [0..cBufferSize-1] of Byte;
  St			: String;
  I, Count		: Integer;
begin
  Stream		:= TStringStream.Create('');
  repeat
    St			:= '';
    Count		:= Value.Read(Buf, SizeOf(Buf));
    for I := 0 to Count - 1 do
      St := St + IntToHex(Buf[I], 2);
    Stream.WriteString(St);
  until Count = 0;

  Result		:= Add(Name, Stream.DataString);

  Stream.Free;
end;

function TLetoXmlElems.Add(Value: TLetoXmlElem): TLetoXmlElem;
begin
  CreateElems;
  FElems.AddObject(Value.Name, Value);

  Result		:= Value;
end;

(*------------------------------------------------------------------------------
AddFirst

------------------------------------------------------------------------------*)
function TLetoXmlElems.AddFirst(Value: TLetoXmlElem): TLetoXmlElem;
begin
  CreateElems;
  FElems.InsertObject(0, Value.Name, Value);

  Result		:= Value;
end;

function TLetoXmlElems.AddFirst(const Name: string): TLetoXmlElem;
begin
  Result		:= TLetoXmlElem.Create(Parent, Root);
  Result.FName		:= Name;
  AddChildFirst(Result);
end;

(*------------------------------------------------------------------------------
AddComment

------------------------------------------------------------------------------*)
function TLetoXmlElems.AddComment(
  const Name, Value	: String
): TLetoXmlElemComment;
begin
  Result		:= TLetoXmlElemComment.Create(Parent, Root);
  Result.FName		:= Name;
  Result.Value		:= Value;
  AddChild(Result);
end;

(*------------------------------------------------------------------------------
AddCData

------------------------------------------------------------------------------*)
function TLetoXmlElems.AddCData(
  const Name, Value	: String
): TLetoXmlElemCData;
begin
  Result		:= TLetoXmlElemCData.Create(Parent, Root);
  Result.FName		:= Name;
  Result.Value		:= Value;
  AddChild(Result);
end;

(*------------------------------------------------------------------------------
AddText

------------------------------------------------------------------------------*)
function TLetoXmlElems.AddText(
  const Name, Value	: String
): TLetoXmlElemText;
begin
  Result		:= TLetoXmlElemText.Create(Parent, Root);
  Result.FName		:= Name;
  Result.Value		:= Value;
  AddChild(Result);
end;

(*------------------------------------------------------------------------------
Clear

------------------------------------------------------------------------------*)
procedure TLetoXmlElems.Clear;
var
  I			: Integer;
begin
  if not Assigned(FElems) then Exit;
  for I := 0 to FElems.Count - 1 do
    TLetoXmlElem(FElems.Objects[I]).Free;
  FElems.Clear;
end;

(*------------------------------------------------------------------------------
Delete

------------------------------------------------------------------------------*)
procedure TLetoXmlElems.Delete(const Index: Integer);
begin
  if Assigned(FElems) and (Index >= 0) and (Index < FElems.Count) then
    FElems.Delete(Index);
end;

procedure TLetoXmlElems.Delete(const Name: string);
begin
  if Assigned(FElems) then
    Delete(FElems.IndexOf(Name));
end;

(*------------------------------------------------------------------------------
Value

------------------------------------------------------------------------------*)
function TLetoXmlElems.Value(const Name: string; Default: string): string;
var
  Elem			: TLetoXmlElem;
begin
  Result		:= '';
  Elem			:= GetItemNamed(Name);
  if Assigned(Elem) then
    Result		:= Elem.Value
  else
    Result		:= Default;
end;

(*------------------------------------------------------------------------------
AsInt

------------------------------------------------------------------------------*)
function TLetoXmlElems.AsInt(const Name: string; Default: Int64): Int64;
var
  Elem			: TLetoXmlElem;
begin
  Elem			:= GetItemNamed(Name);
  if Assigned(Elem) then
    Result		:= Elem.AsInt
  else
    Result		:= Default;
end;

(*------------------------------------------------------------------------------
AsBool

------------------------------------------------------------------------------*)
function TLetoXmlElems.AsBool(const Name: string; Default: Boolean): Boolean;
var
  Elem			: TLetoXmlElem;
begin
  Elem			:= GetItemNamed(Name);
  if Assigned(Elem) then
    Result		:= Elem.AsBool
  else
    Result		:= Default;
end;

(*------------------------------------------------------------------------------
Find

Try and find a child element with the Prop and Value indicated.

If Named is '*', the element can have any name. (Otherwise, pattern matching
is not currently supported.)

------------------------------------------------------------------------------*)
function TLetoXmlElems.Find(const Named, Prop, Value: String): TLetoXmlElem;
var
  I			: Integer;
  Child			: TLetoXmlElem;
begin
  Result		:= nil;

  for I := 0 to Count-1 do begin
    Child		:= Item[I];
    if
      ( (Named = '*') or (Child.Name = Named) ) and
      Child.HasProp(Prop) and
      (Child.Props[Prop].Value = Value)
    then begin
      Result		:= Child;
      Exit;
    end;
  end;
  
end;

(*------------------------------------------------------------------------------
LoadFromStream

------------------------------------------------------------------------------*)
function TLetoXmlElems.LoadFromStream(
  const Stream		: TStream;
  const TrimSpace	: Boolean
): String;
const
  LOAD_STATE_WAIT_TAG	= 0;
  LOAD_STATE_TAG_TYPE	= 1;
  LOAD_STATE_END_TAG	= 2;
var
  I, lStreamPos, Count,
    lPos		: Integer;
  lBuf			: array [0..cBufferSize-1] of Char;
  St			: string;
  lElem			: TLetoXmlElem;
begin

  lStreamPos		:= Stream.Position;
  Result		:= '';
  St			:= '';
  lPos			:= 0;

  Result		:= '';

  repeat

    Count		:= Stream.Read(lBuf, SizeOf(lBuf));

    for I := 0 to Count-1 do begin

      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        LOAD_STATE_WAIT_TAG:
          case lBuf[I] of
            ' ', #9, #13, #10: ;
            '<':
              begin
                lPos := LOAD_STATE_TAG_TYPE;
                St := lBuf[I];
              end;
          else
            //This is text
//            if not TrimSpace or not(lBuf[I] in [' ', #9, #13, #10]) then
            begin
              lElem := TLetoXmlElemText.Create(Parent, Root);
              Stream.Seek(lStreamPos - 1, soFromBeginning);
              if not lElem.LoadFromStream(Stream, TrimSpace) then
                Exit;
              lStreamPos := Stream.Position;
              CreateElems;
              FElems.AddObject(lElem.Name, lElem);
              Break;
            end;
          end;

        LOAD_STATE_TAG_TYPE:
        begin
          lElem := nil;
          case lBuf[I] of
            '/':
              if St = '<' then begin
                lPos := LOAD_STATE_END_TAG;
                St := '';
              end else begin
                lElem := TLetoXmlElem.Create(Parent, Root);
                St := St + lBuf[I];
              end;

            ' ', '>', ':': //This should be a  tag
              begin
                lElem := TLetoXmlElem.Create(Parent, Root);
                St := St + lBuf[I];
              end;
          else
            begin
              St := St + lBuf[I];
              if St = '<![CDATA[' then
                lElem := TLetoXmlElemCData.Create(Parent, Root)
              else if St = '<!--' then
                lElem := TLetoXmlElemComment.Create(Parent, Root);
              //<?
            end;
          end;

          if lElem <> nil then
          begin
            CreateElems;
            Stream.Seek(lStreamPos - (Length(St)), soFromBeginning);
            if not lElem.LoadFromStream(Stream, TrimSpace) then
              Exit;
            lStreamPos := Stream.Position;
            FElems.AddObject(lElem.Name, lElem);
            St := '';
            lPos := LOAD_STATE_WAIT_TAG;
            Break;
          end;
        end;

        LOAD_STATE_END_TAG:
          if lBuf[I] = '>' then begin
            Result := St;
            Count := 0;
            Break;
          end else
            St := St + lBuf[I];

      end; // case

    end; // for I

  until Count = 0;

  Stream.Seek(lStreamPos, soFromBeginning);

end;

(*------------------------------------------------------------------------------
SaveToStream

------------------------------------------------------------------------------*)
procedure TLetoXmlElems.SaveToStream(
  const Stream		: TStream;
  const IndentStr	: String;
  const Level		: Integer
);
var
  I			: Integer;
begin
  for I := 0 to Count-1 do
    Item[I].SaveToStream(Stream, IndentStr, Level);
end;

(*------------------------------------------------------------------------------
Sort

------------------------------------------------------------------------------*)
procedure TLetoXmlElems.Sort(
  const Method		: TSortMethod;
  const Prop		: String;
  const Alternate	: String
);
var
  SortList		: TStringList;
  I			: Integer;
  S			: String;
begin
  SortList		:= TStringList.Create;
  SortList.Duplicates	:= dupIgnore;

  for I := 0 to FElems.Count-1 do begin
    if Method = SortByValue then
      S			:= Item[I].Value
    else
      S			:= Item[I].Props[Prop].Value;
    if (S = '') then begin
      if Alternate <> '' then
        S		:= Item[I].Props[Alternate].Value;
      if S = '' then
        S		:= '*';
    end;
    SortList.AddObject(S, FElems.Objects[I]);
  end;

  SortList.Sort;

  for I := 0 to SortList.Count-1 do
    FElems.Move(
      FElems.IndexOfObject( SortList.Objects[I] ),
      I
    );

  FreeAndNil(SortList);
end;


{ TLetoXmlElemsProlog }


(*------------------------------------------------------------------------------
property Count

------------------------------------------------------------------------------*)
function TLetoXmlElemsProlog.GetCount: Integer;
begin
  Result		:= FElems.Count;
end;

(*------------------------------------------------------------------------------
property Item

------------------------------------------------------------------------------*)
function TLetoXmlElemsProlog.GetItem(const Index: Integer): TLetoXmlElem;
begin
  Result		:= TLetoXmlElem(FElems.Objects[Index]);
end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoXmlElemsProlog.Create(const ARoot: TLetoXmlElem);
begin
  inherited Create;

  FRoot			:= ARoot;
  FElems		:= THashedStringList.Create;

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoXmlElemsProlog.Destroy;
begin
  Clear;
  FreeAndNil(FElems);

  inherited Destroy;
end;

(*------------------------------------------------------------------------------
Clear

------------------------------------------------------------------------------*)
procedure TLetoXmlElemsProlog.Clear;
begin
  while FElems.Count > 0 do begin
    TLetoXmlElem(FElems[0]).Free;
    FElems.Delete(0);
  end;
end;

(*------------------------------------------------------------------------------
LoadFromStream

------------------------------------------------------------------------------*)
function TLetoXmlElemsProlog.LoadFromStream(
  const Stream		: TStream;
  const TrimSpace	: Boolean
): Boolean;
var
  I, lStreamPos, Count,
    lPos		: Integer;
  lBuf			: array [0..cBufferSize-1] of Char;
  St			: String;
  lEnd			: Boolean;
  lElem			: TLetoXmlElem;
begin

  lStreamPos		:= Stream.Position;
  St			:= '';
  lPos			:= 0;

  Result		:= False;

  repeat

    Count := Stream.Read(lBuf, SizeOf(lBuf));

    for I := 0 to Count-1 do begin

      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        0: //We are waiting for a tag and thus avoiding spaces
          case lBuf[I] of
            ' ', #9, #13, #10: ;
            '<':
              begin
                lPos := 1;
                St := lBuf[I];
              end;
          else
            Exit;
          end;

        1: //We are trying to determine the kind of the tag
          begin
            lElem := nil;
            lEnd := False;

            St := St + lBuf[I];
            if St = '<?xml ' then
              lElem := TLetoXmlElemHeader.Create
            else if St = '<!DOCTYPE' then
              lElem := TLetoXmlElemDocType.Create(nil, Root)
            else if St = '<?xml-stylesheet' then
              lElem := TLetoXmlElemSheet.Create(nil, Root)
            else if St = '<!--' then
              lElem := TLetoXmlElemComment.Create(nil, Root)
            else if St = '<![CDATA[' then
              lEnd := True
            else if (Length(St) > 1) and not (St[2] in ['!', '?']) then
              lEnd := True;

            if lEnd then begin
              lStreamPos := lStreamPos - Length(St);
              Count := 0;
              Break;
            end else if lElem <> nil then begin
              Stream.Seek(lStreamPos - (Length(St)), soFromBeginning);
              if not lElem.LoadFromStream(Stream, TrimSpace) then
                Exit;
              lStreamPos := Stream.Position;
              FElems.AddObject(lElem.Name, lElem);
              St := '';
              lPos := 0;
              Break;
            end;

          end;

      end; // case lPos

    end; // for I

  until Count = 0;

  Stream.Seek(lStreamPos, soFromBeginning);

  Result		:= True;

end;

(*------------------------------------------------------------------------------
SaveToStream

------------------------------------------------------------------------------*)
procedure TLetoXmlElemsProlog.SaveToStream(
  const Stream		: TStream
);
var
  I			: Integer;
begin
  if Count = 0 then
    FElems.AddObject('', TLetoXmlElemHeader.Create);
  for I := 0 to Count - 1 do
    Item[I].SaveToStream(Stream, '');
end;


{ TLetoXmlProp }


(*------------------------------------------------------------------------------
property Name

------------------------------------------------------------------------------*)
procedure TLetoXmlProp.SetName(const Value: string);
begin
  if (Value <> FName) and (Value <> '') then begin
    if (Parent <> nil) and (FName <> '') then
      Parent.DoItemRename(Self, Value);
    FName		:= Value;
  end;
end;

(*------------------------------------------------------------------------------
property AsBool

------------------------------------------------------------------------------*)
function TLetoXmlProp.GetBoolValue: Boolean;
begin
  Result		:= StrToBoolDef(Value, False);
end;

procedure TLetoXmlProp.SetBoolValue(const Value: Boolean);
begin
  FValue		:= BoolToStr(Value);
end;

(*------------------------------------------------------------------------------
property AsChar

------------------------------------------------------------------------------*)
function TLetoXmlProp.GetCharValue: Char;
var
  S			: String;
begin
  S			:= FValue;
  if S = '' then
    Result		:= #0
  else
    Result		:= S[1];
end;

procedure TLetoXmlProp.SetCharValue(const Value: Char);
begin
  FValue		:= Value;
end;

(*------------------------------------------------------------------------------
property AsInt

------------------------------------------------------------------------------*)
function TLetoXmlProp.GetIntValue: Int64;
begin
  Result		:= StrToInt64Def(Value, -1);
end;

procedure TLetoXmlProp.SetIntValue(const Value: Int64);
begin
  FValue		:= IntToStr(Value);
end;

(*------------------------------------------------------------------------------
SaveToString

------------------------------------------------------------------------------*)
function TLetoXmlProp.SaveToString: string;
begin
  if Pointer <> '' then
    Result := Format(' %s:%s="%s"', [Pointer, Name, SimpleXmlEncode(Value)])
  else
    Result := Format(' %s="%s"', [Name, SimpleXmlEncode(Value)]);
end;


{ TLetoXmlProps }


(*------------------------------------------------------------------------------
property Count


------------------------------------------------------------------------------*)
function TLetoXmlProps.GetCount: Integer;
begin
  if Assigned(FProps) then
    Result		:= FProps.Count
  else
    Result		:= 0;
end;

(*------------------------------------------------------------------------------
property ItemNamed

------------------------------------------------------------------------------*)
function TLetoXmlProps.GetItemNamed(const Name: string): TLetoXmlProp;
begin
  Result		:= nil;

  if Assigned(FProps) then
    Result		:= GetItem(FProps.IndexOf(Name));

  if not Assigned(Result) then
    Result		:= FAnon;
end;

(*------------------------------------------------------------------------------
property Item

------------------------------------------------------------------------------*)
function TLetoXmlProps.GetItem(const Index: Integer): TLetoXmlProp;
begin
  if Assigned(FProps) and (Index > -1) and (Index < Count) then
    Result		:= TLetoXmlProp(FProps.Objects[Index])
  else
    Result		:= nil;
end;

(*------------------------------------------------------------------------------
DoItemRename

------------------------------------------------------------------------------*)
procedure TLetoXmlProps.DoItemRename(
  var Value		: TLetoXmlProp;
  const Name		: String
);
var
  I			: Integer;
begin
  I := FProps.IndexOfObject(Value);
  if I > -1 then
    FProps[I]		:= Name;
end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoXmlProps.Create;
begin
  FAnon			:= TLetoXmlProp.Create;
end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoXmlProps.Destroy;
begin
  Clear;

  if Assigned(FProps) then
    FreeAndNil(FProps);

  FreeAndNil(FAnon);

  inherited Destroy;
end;

(*------------------------------------------------------------------------------
Add

------------------------------------------------------------------------------*)
function TLetoXmlProps.Add(const Name, Value: String): TLetoXmlProp;
var
  Elem			: TLetoXmlProp;
begin
  if not Assigned(FProps) then
    FProps		:= THashedStringList.Create;

  Elem			:= TLetoXmlProp.Create;

  FProps.AddObject(Name, Elem);

  Elem.FName		:= Name;
  Elem.Value		:= Value;
  Elem.Parent		:= self;

  Result		:= Elem;
end;

function TLetoXmlProps.Add(
  const Name		: String;
  const Value		: Int64
): TLetoXmlProp;
begin
  Result		:= Add(Name, IntToStr(Value));
end;

function TLetoXmlProps.Add(
  const Name		: String;
  const Value		: Boolean
): TLetoXmlProp;
begin
  Result		:= Add(Name, BoolToStr(Value));
end;

(*------------------------------------------------------------------------------
Clear

------------------------------------------------------------------------------*)
procedure TLetoXmlProps.Clear;
var
  I			: Integer;
begin
  if not Assigned(FProps) then Exit;

  for I := 0 to FProps.Count-1 do
    TLetoXmlProp(FProps.Objects[I]).Free;

  FProps.Clear;
end;

(*------------------------------------------------------------------------------
Delete

------------------------------------------------------------------------------*)
procedure TLetoXmlProps.Delete(const Index: Integer);
begin
  if Assigned(FProps) and (Index >= 0) and (Index < Count) then
    FProps.Delete(Index);
end;

procedure TLetoXmlProps.Delete(const Name: string);
begin
  if Assigned(FProps) then
    Delete(FProps.IndexOf(Name));
end;

(*------------------------------------------------------------------------------
Value

------------------------------------------------------------------------------*)
function TLetoXmlProps.Value(const Name: string; Default: string): string;
var
  Prop			: TLetoXmlProp;
begin
  Result		:= '';
  Prop			:= GetItemNamed(Name);
  if Assigned(Prop) then
    Result		:= Prop.Value
  else
    Result		:= Default;
end;

(*------------------------------------------------------------------------------
AsInt

------------------------------------------------------------------------------*)
function TLetoXmlProps.AsInt(const Name: string; Default: Int64): Int64;
var
  Prop			: TLetoXmlProp;
begin
  Prop			:= GetItemNamed(Name);
  if Assigned(Prop) then
    Result		:= Prop.AsInt
  else
    Result		:= Default;
end;

(*------------------------------------------------------------------------------
AsBool

------------------------------------------------------------------------------*)
function TLetoXmlProps.AsBool(const Name: string; Default: Boolean): Boolean;
var
  Prop			: TLetoXmlProp;
begin
  Prop			:= GetItemNamed(Name);
  if Assigned(Prop) then
    Result		:= Prop.AsBool
  else
    Result		:= Default;
end;

(*------------------------------------------------------------------------------
LoadFromStream

------------------------------------------------------------------------------*)
function TLetoXmlProps.LoadFromStream(const Stream: TStream): Boolean;
var
  I, lStreamPos, Count,
    lPos		: Integer;
  lBuf			: array [0..cBufferSize-1] of Char;
  lName, lValue,
    lPointer		: String;
  lPropStart		: Char;
begin

  lStreamPos		:= Stream.Position;
  lValue		:= '';
  lPointer		:= '';
  lName			:= '';
  lPropStart		:= ' ';
  lPos			:= 0;

  Result		:= False;

  repeat
    Count := Stream.Read(lBuf, SizeOf(lBuf));
    for I := 0 to Count - 1 do begin
      //Increment Stream pos for after comment
      Inc(lStreamPos);

      case lPos of
        0: //We are waiting for a property
          begin
            case lBuf[I] of
              ' ', #9, #10, #13:
                begin
                end;
              'a'..'z', 'A'..'Z', '0'..'9', '-', '_':
                begin
                  lName := lBuf[I];
                  lPos := 1;
                end;
              '/', '>', '?':
                begin
                  Dec(lStreamPos);
                  Count := 0;
                  Break;
                end;
            else
              Exit;
            end;
          end;

        1: //We are reading a property name
          case lBuf[I] of
            'a'..'z', 'A'..'Z', '0'..'9', '-', '_':
              lName := lName + lBuf[I];
            ':':
              begin
                lPointer := lName;
                lName := '';
              end;
            '=':
              lPos := 2;
          else
            Exit;
          end;

        2: //We are going to start a property content
          if lBuf[I] in ['''', '"'] then
          begin
            lPropStart := lBuf[I];
            lValue := '';
            lPos := 3;
          end else
            Exit;

        3: //We are reading a property
          if lBuf[I] = lPropStart then begin
            SimpleXmlDecode(lValue, False);
            with Add(lName, lValue) do
              Pointer := lPointer;
            lPos := 0;
          end else
            lValue := lValue + lBuf[I];
      end;
    end;
  until Count = 0;

  Result		:= True;

  Stream.Seek(lStreamPos, soFromBeginning);

end;

(*------------------------------------------------------------------------------
SaveToStream

------------------------------------------------------------------------------*)
procedure TLetoXmlProps.SaveToStream(const Stream: TStream);
var
  St			: String;
  I			: Integer;
begin
  St			:= '';
  for I := 0 to Count-1 do
    St			:= St + Item[I].SaveToString;
  if St <> '' then
    Stream.Write(St[1], Length(St));
end;

(*------------------------------------------------------------------------------
Establish

------------------------------------------------------------------------------*)
function TLetoXmlProps.Establish(
  const PropName,
    PropValue		: String
): TLetoXmlProp;
var
  I			: Integer;
begin
  if not Assigned(FProps) then
    FProps		:= THashedStringList.Create;
    
  I			:= FProps.IndexOf(PropName);
  if I > -1 then begin
    Result		:= GetItem(I);
    Result.Value	:= PropValue;
  end else
    Result		:= Add(PropName, PropValue);
end;


{ TLetoXml }


(*------------------------------------------------------------------------------
property FileName

------------------------------------------------------------------------------*)
procedure TLetoXml.SetFileName(Value: TFileName);
begin
  FFileName		:= Value;
  LoadFromFile(Value);
end;

(*------------------------------------------------------------------------------
constructor

------------------------------------------------------------------------------*)
constructor TLetoXml.Create(
  const AFileName	: TFileName;
  const ATrimSpace	: Boolean
);
begin
  inherited Create;

  FRoot			:= TLetoXmlElem.Create(nil, nil);
  FRoot.Root		:= FRoot;
  FProlog		:= TLetoXmlElemsProlog.Create(FRoot);

  IndentStr		:= #9;
  TrimSpace		:= ATrimSpace;

  if AFileName <> '' then
    LoadFromFile(AFileName, ATrimSpace);

end;

(*------------------------------------------------------------------------------
destructor

------------------------------------------------------------------------------*)
destructor TLetoXml.Destroy;
begin
  FreeAndNil(FRoot);
  FreeAndNil(FProlog);
  inherited Destroy;
end;

(*------------------------------------------------------------------------------
LoadFromString

------------------------------------------------------------------------------*)
procedure TLetoXml.LoadFromString(
  const Value		: String;
  const ATrimSpace	: Boolean
);
var
  Stream		: TStringStream;
begin
  Stream		:= TStringStream.Create(Value);
  try
    LoadFromStream(Stream, ATrimSpace);
  finally
    Stream.Free;
  end;
end;

(*------------------------------------------------------------------------------
LoadFromFile

------------------------------------------------------------------------------*)
function TLetoXml.LoadFromFile(
  const AFileName	: TFileName;
  const ATrimSpace	: Boolean
): Boolean;
var
  Stream		: TFileStream;
begin
  Result		:= False;
  FFileName		:= AFileName;

  if not FileExists(FileName) then Exit;

  try

  try
    Stream		:= TFileStream.Create(FileName, fmOpenRead or fmShareDenyNone);
  except
    Exit;
  end;

  Result		:= LoadFromStream(Stream, ATrimSpace);

  finally
    FreeAndNil(Stream);
  end;

end;

(*------------------------------------------------------------------------------
LoadFromStream

------------------------------------------------------------------------------*)
function TLetoXml.LoadFromStream(
  const Stream		: TStream;
  const ATrimSpace	: Boolean
): Boolean;
begin
  FTrim			:= ATrimSpace;
  FRoot.Clear;
  FProlog.Clear;
  Result :=
    FProlog.LoadFromStream(Stream, ATrimSpace) and
    FRoot.LoadFromStream(Stream, ATrimSpace);
end;

(*------------------------------------------------------------------------------
SaveToString

------------------------------------------------------------------------------*)
function TLetoXml.SaveToString: String;
var
  Stream		: TStringStream;
begin
  try
    Stream		:= TStringStream.Create('');
    try
      SaveToStream(Stream);
      Result		:= Stream.DataString;
    finally
      Stream.Free;
    end;
  except
  end;
end;

(*------------------------------------------------------------------------------
SaveToFile

------------------------------------------------------------------------------*)
procedure TLetoXml.SaveToFile(FileName: TFileName);
var
  Stream		: TFileStream;
begin
  try
    if FileExists(FileName) then begin
      Stream		:= TFileStream.Create(FileName, fmOpenWrite);
      Stream.Size		:= 0;
    end else
      Stream		:= TFileStream.Create(FileName, fmCreate);
    try
      SaveToStream(Stream);
    finally
      Stream.Free;
    end;
  except
  end;
end;

(*------------------------------------------------------------------------------
SaveToStream

------------------------------------------------------------------------------*)
procedure TLetoXml.SaveToStream(const Stream: TStream);
begin
  Prolog.SaveToStream(Stream);
  Root.SaveToStream(Stream, IndentStr);
end;

(*------------------------------------------------------------------------------
FakeFileName

Sets FFileName, without the usual SetFileName obstruction.

------------------------------------------------------------------------------*)
procedure TLetoXml.FakeFileName(const AFileName: TFileName);
begin
  FFileName		:= AFileName;
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
