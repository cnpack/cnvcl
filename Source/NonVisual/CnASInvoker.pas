{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2020 CnPack 开发组                       }
{                   ------------------------------------                       }
{                                                                              }
{            本开发包是开源的自由软件，您可以遵照 CnPack 的发布协议来修        }
{        改和重新发布这一程序。                                                }
{                                                                              }
{            发布这一开发包的目的是希望它有用，但没有任何担保。甚至没有        }
{        适合特定目的而隐含的担保。更详细的情况请参阅 CnPack 发布协议。        }
{                                                                              }
{            您应该已经和开发包一起收到一份 CnPack 发布协议的副本。如果        }
{        还没有，可访问我们的网站：                                            }
{                                                                              }
{            网站地址：http://www.cnpack.org                                   }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnASInvoker;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：根据接口动态调用方法单元
* 单元作者：由 周劲羽 移植自 Delphi 7 Source
* 备    注：该单元通过修改和移植自 Delphi 7 中的 Source\Soap 下的
*           IntfInfo, Invoker, InvokeRegistry, InvRules, TypeTrans 等单元。
*           注：该单元不支持 Delphi/BCB 5，仅支持 Delphi/BCB 6 以上版本。
* 开发平台：PWin2K SP3 + Delphi 7
* 兼容测试：PWin9X/2000/XP + Delphi 6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2003.07.08
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFNDEF COMPILER6_UP}
  'Error: This unit can used only for Delphi / C++Builder 6 or up.'
{$ENDIF COMPILER6_UP}

uses
  Sysutils, Classes, TypInfo, Variants;

type

//==============================================================================
// 接口 RTTI 相关定义，移植自 IntfInfo
//==============================================================================

  PIntfParamEntry = ^TIntfParamEntry;
  TIntfParamEntry = record
    Flags: TParamFlags;
    Name: string;
    Info: PTypeInfo;
  end;

  TIntfParamEntryArray = array of TIntfParamEntry;

  TCallConv = (ccReg, ccCdecl, ccPascal, ccStdCall, ccSafeCall);

  PIntfMethEntry = ^TIntfMethEntry;
  TIntfMethEntry = record
    Name: string;
    CC: TCallConv; { Calling convention }
    Pos: Integer; { Index (relative to whole interface VMT) }
    ParamCount: Integer;
    ResultInfo: PTypeInfo;
    SelfInfo: PTypeInfo;
    Params: TIntfParamEntryArray;
    HasRTTI: Boolean;
  end;
  TIntfMethEntryArray = array of TIntfMethEntry;
  TPIntfMethEntryArray = array of PIntfMethEntry;

  { Governs show the MDA array is filled }
  TFillMethodArrayOpt = (fmoAllBaseMethods, fmoRTTIBaseMethods, fmoNoBaseMethods);

  PIntfMetaData = ^TIntfMetaData;
  TIntfMetaData = record
    Name: string;
    UnitName: string;
    MDA: TIntfMethEntryArray;
    IID: TGUID;
    Info: PTypeInfo;
    AncInfo: PTypeInfo;
    NumAnc: Integer; { #Methods in base interfaces }
  end;

  EInterfaceRTTIException = class(Exception);

  TDynToClear = record
    P: Pointer;
    Info: PTypeInfo;
  end;

//==============================================================================
// 用于动态方法调用的临时数据类，移植修改自 InvokeRegistry
//==============================================================================

{ TDataContext }

  TDataContext = class
  protected
    DataOffset: Integer;
    Data: array of Byte;
    DataP: array of Pointer;
    VarToClear: array of Pointer;
    DynArrayToClear: array of TDynToClear;
    StrToClear: array of Pointer;
    WStrToClear: array of Pointer;
  public
    constructor Create;
    destructor Destroy; override;
    function  AllocData(Size: Integer): Pointer;
    procedure SetDataPointer(Index: Integer; P: Pointer);
    function  GetDataPointer(Index: Integer): Pointer;
    procedure AddDynArrayToClear(P: Pointer; Info: PTypeInfo);
    procedure AddVariantToClear(P: PVarData);
    procedure AddStrToClear(P: Pointer);
    procedure AddWStrToClear(P: Pointer);
  end;

{ TInvContext }

  TInvContext = class(TDataContext)
  private
    ResultP: Pointer;
  public
    procedure SetMethodInfo(const MD: TIntfMethEntry);
    procedure SetParamPointer(Param: Integer; P: Pointer);
    function  GetParamPointer(Param: Integer): Pointer;
    function  GetResultPointer: Pointer;
    procedure SetResultPointer(P: Pointer);
    procedure AllocServerData(const MD: TIntfMethEntry);
  end;

//==============================================================================
// 动态接口方法调用器类，移植修改自 Invoke
//==============================================================================

{ TInterfaceInvoker }

  TInterfaceInvoker = class
  public
    procedure Invoke(const Obj: TObject; IntfMD: TIntfMetaData;
      const MethNum: Integer; const Context: TInvContext);
    constructor Create;
  end;

//==============================================================================
// 类型转换器类，移植修改自 TypeTrans
//==============================================================================

  ETypeTransException = class(Exception);

{ TTypeTranslator }

  TTypeTranslator = class
  public
    constructor Create;
    destructor Destroy; override;

    procedure CastVariantToNative(Info: PTypeInfo; const Value: OleVariant;
      NatData: Pointer);
    procedure CastNativeToVariant(Info: PTypeInfo; var Value: OleVariant;
      NatData: Pointer);
  end;

procedure GetIntfMetaData(Info: PTypeInfo; var IntfMD: TIntfMetaData;
  MethodArrayOpt: TFillMethodArrayOpt); overload;
procedure GetIntfMetaData(Info: PTypeInfo; var IntfMD: TIntfMetaData;
  IncludeAllAncMethods: Boolean = False); overload;
function GetMethNum(const IntfMD: TIntfMetaData; const MethName: string;
  ParamCount: Integer = -1): Integer;

function TypeNamesMatch(const RegName: string; const OtherName: string): Boolean;
function OtherTypeName(const TypeName: string): string;
function SameTypeInfo(const RegInfo: PTypeInfo; const OtherInfo: PTypeInfo):
  Boolean;

function InterfaceInvoker: TInterfaceInvoker;
function TypeTranslator: TTypeTranslator;

implementation

const
  KindNameArray: array[tkUnknown..tkDynArray] of string =
  ('Unknown', 'Integer', 'Char', 'Enumeration', 'Float',
    'String', 'Set', 'Class', 'Method', 'WChar', 'LString', 'WString',
    'Variant', 'Array', 'Record', 'Interface', 'Int64', 'DynArray');

  CallingConventionName: array[ccReg..ccSafeCall] of string =
  ('REGISTER', 'CDECL', 'PASCAL', 'STDCALL', 'SAFECALL');

  TypeInfoNames: array[0..33] of string = ('Boolean', 'bool',
    'Char', 'char',
    'Char', 'signed char',
    'Byte', 'unsigned char',
    'SmallInt', 'short',
    'Word', 'unsigned short',
    'Integer', 'int',
    'Cardinal', 'unsigned int',
    'Integer', 'long',
    'Cardinal', 'unsigned long',
    'Int64', '__int64',
    'Int64', 'unsigned __int64',
    'Single', 'float',
    'Double', 'double',
    'Extended', 'long double',
    'String', 'AnsiString',
    'WideString', 'WideString');

  CCMap: array[0..4] of TCallConv = (ccReg, ccCdecl, ccPascal, ccStdCall,
    ccSafeCall);

resourcestring
  SNoInterfaceGUID = 'Class %s does not implement interface GUID %s';
  SUnsupportedCC = 'Unsupported calling convention: %s';
  SVariantCastNotSupported = 'Type cannot be cast as Variant';
  SUnexpectedDataType = 'Internal error: data type kind %s not expected in this context';
  SNoRTTI = 'Interface %s has no RTTI';
  SNoRTTIParam = 'Parameter %s on Method %s of Interface %s has no RTTI';

var
  FInterfaceInvoker: TInterfaceInvoker;
  FTypeTranslator: TTypeTranslator;

function InterfaceInvoker: TInterfaceInvoker;
begin
  if not Assigned(FInterfaceInvoker) then
    FInterfaceInvoker := TInterfaceInvoker.Create;
  Result := FInterfaceInvoker;
end;

function TypeTranslator: TTypeTranslator;
begin
  if not Assigned(FTypeTranslator) then
    FTypeTranslator := TTypeTranslator.Create;
  Result := FTypeTranslator;
end;

//==============================================================================
// 接口 RTTI 相关定义，移植自 IntfInfo
//==============================================================================

function ReadString(var P: Pointer): string;
var
  B: Byte;
begin
  B := Byte(P^);
  SetLength(Result, B);
  P := Pointer(Integer(P) + 1);
  Move(P^, Result[1], Integer(B));
  P := Pointer(Integer(P) + B);
end;

function ReadByte(var P: Pointer): Byte;
begin
  Result := Byte(P^);
  P := Pointer(Integer(P) + 1);
end;

function ReadWord(var P: Pointer): Word;
begin
  Result := Word(P^);
  P := Pointer(Integer(P) + 2);
end;

function ReadLong(var P: Pointer): Integer;
begin
  Result := Integer(P^);
  P := Pointer(Integer(P) + 4);
end;

procedure FillMethodArray(P: Pointer; IntfMD: PIntfMetaData; Offset, Methods:
  Integer);
var
  S: string;
  I, J, K, L: Integer;
  ParamCount: Integer;
  Kind, Flags: Byte;
  ParamInfo: PTypeInfo;
  ParamName: string;
  IntfMethod: PIntfMethEntry;
  IntfParam: PIntfParamEntry;
begin
  for I := 0 to Methods - 1 do
  begin
    IntfMethod := @IntfMD.MDA[Offset];
    IntfMethod.Name := ReadString(P);
    Kind := ReadByte(P); { tkKind }
    IntfMethod.CC := CCMap[ReadByte(P)];
    ParamCount := ReadByte(P); { Param count including self }
    IntfMethod.ParamCount := ParamCount - 1;
    IntfMethod.Pos := Offset;
    IntfMethod.HasRTTI := True;

    SetLength(IntfMethod.Params, ParamCount);
    K := 0;
    for J := 0 to ParamCount - 1 do
    begin
      Flags := ReadByte(P); { Flags }
      ParamName := ReadString(P); { Param name }
      S := ReadString(P); { Param type name }
      L := ReadLong(P); { Param Type Info }
      if L <> 0 then
        ParamInfo := PPTypeInfo(L)^
      else
        raise EInterfaceRTTIException.CreateFmt(SNoRTTIParam, [ParamName,
          IntfMethod.Name, IntfMD.UnitName + '.' + IntfMd.Name]);
      if J = 0 then
        IntfMethod.SelfInfo := ParamInfo
      else
      begin
        IntfParam := @IntfMethod.Params[K];
        IntfParam.Flags := TParamFlags(Flags);
        IntfParam.Name := ParamName;
        IntfParam.Info := ParamInfo;
        Inc(K);
      end;
    end;
    if Kind = Byte(mkFunction) then
    begin
      S := ReadString(P);
      IntfMethod.ResultInfo := PPTypeInfo(ReadLong(P))^;
    end;
    Inc(Offset);
  end;
end;

function WalkAncestors(PP: PPTypeInfo; AddMeths: Boolean; IntfMD: PIntfMetaData;
  WithRTTIOnly: Boolean): Integer;
var
  S: string;
  AncTP: Pointer;
  P: Pointer;
  B: Byte;
  NumMethods, NumAncMeths, I: Integer;
  HasRTTI: Boolean;
begin
  P := Pointer(PP^);
  ReadByte(P); // Kind
  S := ReadString(P); // Symbol name
  AncTP := Pointer(ReadLong(P)); // Ancestor TypeInfo
  P := Pointer(Integer(P) + 17); // Intf.flags and GUID
  B := Byte(P^); // Length
  P := Pointer(Integer(P) + B + 1); // Unit name  and count
  NumMethods := ReadWord(P); // # methods
  I := ReadWord(P); // $FFFF if no RTTI, # methods again if has RTTI
  HasRTTI := (I <> $FFFF);

  { Compute the number of methods }
  if (AncTP <> nil) and (HasRTTI or (WithRTTIOnly = False)) then
  begin
    NumAncMeths := WalkAncestors(AncTP, False, nil, WithRTTIOnly);
  end else
    NumAncMeths := 0;
  { Ancestor count }
  Result := NumAncMeths;
  { Plus our own }
  if (HasRTTI or (WithRTTIOnly = False)) then
    Result := Result + NumMethods;
  { Do we need to fill in method information too? }
  if AddMeths then
  begin
    if HasRTTI then
    begin
      FillMethodArray(P, IntfMD, NumAncMeths, NumMethods);
      if NumAncMeths > 0 then
        WalkAncestors(AncTP, AddMeths, IntfMD, WithRTTIOnly);
    end;
  end;
end;

function GetNumAncMeths(P: Pointer; WithRTTIOnly: Boolean = False): Integer;
var
  B: Byte;
  Anc: Pointer;
begin
  Result := 0;
  ReadByte(P); // tkKind
  B := Byte(P^); // Symbol length
  P := Pointer(Integer(P) + B + 1); // Skip sym name  and count
  Anc := Pointer(ReadLong(P)); // Ancestor pointer
  if Anc <> nil then
    Result := WalkAncestors(Anc, False, nil, WithRTTIOnly);
end;

procedure GetIntfMetaData(Info: PTypeInfo; var IntfMD: TIntfMetaData;
  MethodArrayOpt: TFillMethodArrayOpt);
var
  I, Offset: Integer;
  Methods: Integer;
  BaseRTTIMethods: Integer;
  HasRTTI: Integer;
  PP: PPTypeInfo;
  P: Pointer;
  SelfMethCount: Integer;
begin
  P := Pointer(Info);
  { Get total number of ancestor methods }
  IntfMD.NumAnc := GetNumAncMeths(P);
  { Get base methods we could expose }
  BaseRTTIMethods := GetNumAncMeths(P, True);
  IntfMD.Info := Info;
  { tkKind }
  ReadByte(P);
  IntfMD.Name := ReadString(P);
  PP := PPTypeInfo(ReadLong(P));
  { Ancestor typeinfo }
  if PP <> nil then
    IntfMD.AncInfo := PP^
  else
    IntfMD.AncInfo := nil;
  { Interface flags }
  ReadByte(P);
  IntfMD.IID.D1 := Longword(ReadLong(P));
  IntfMD.IID.D2 := ReadWord(P);
  IntfMD.IID.D3 := ReadWord(P);
  for I := 0 to 7 do
    IntfMD.IID.D4[I] := ReadByte(P);
  IntfMD.UnitName := ReadString(P);
  Methods := ReadWord(P); { # methods }
  HasRTTI := ReadWord(P); { $FFFF if no RTTI, # methods again if has RTTI }
  if HasRTTI = $FFFF then
    raise EInterfaceRTTIException.CreateFmt(SNoRTTI, [IntfMD.UnitName + '.' +
      IntfMd.Name]);
  { Save my method count }
  SelfMethCount := Methods;
  { Update count of methods }
  if (MethodArrayOpt = fmoAllBaseMethods) then
  begin
    Methods := Methods + IntfMD.NumAnc;
    Offset := IntfMD.NumAnc;
  end else
    if (MethodArrayOpt = fmoRTTIBaseMethods) then
    begin
      Methods := Methods + BaseRTTIMethods;
      Offset := BaseRTTIMethods;
    end else
      Offset := 0;
  { Size array and fill in information }
  SetLength(IntfMD.MDA, Methods);
  FillMethodArray(P, @IntfMD, Offset, SelfMethCount);
  { Include method info. of base methods too?? }
  if (MethodArrayOpt = fmoAllBaseMethods) or
    (MethodArrayOpt = fmoRTTIBaseMethods) then
  begin
    if PP <> nil then
      WalkAncestors(PP, True, @IntfMD, (MethodArrayOpt = fmoRTTIBaseMethods));
  end;
end;

procedure GetIntfMetaData(Info: PTypeInfo; var IntfMD: TIntfMetaData;
  IncludeAllAncMethods: Boolean);
var
  FillMethodArrayOpt: TFillMethodArrayOpt;
begin
  if (IncludeAllAncMethods) then
    FillMethodArrayOpt := fmoAllBaseMethods
  else
    FillMethodArrayOpt := fmoRTTIBaseMethods;
  GetIntfMetaData(Info, IntfMD, FillMethodArrayOpt);
end;

function GetMethNum(const IntfMD: TIntfMetaData; const MethName: string;
  ParamCount: Integer = -1): Integer;

  function CalcParamCount(const Start: Integer; const entry: TIntfMethEntry):
      Integer;
  var
    I: Integer;
  begin
    Result := Start;
    { Not needed for C++Builder }
    { TODO -oBB : The range of this loop looks suspicious - investigate ParamCount & confirm accuracy!! }
    for I := 0 to entry.ParamCount do
      if pfOut in entry.Params[I].Flags then
        Inc(Result);
  end;

var
  I, NumNames, ExpCount: Integer;
begin
  NumNames := 0;
  Result := -1;
  for I := 0 to Length(IntfMD.MDA) - 1 do
  begin
    { TODO OWNER:BB How will this fare under C++ where symbols are case-sensitive ??
           ??????? }
    if SameText(IntfMD.MDA[I].Name, MethName) then
    begin
      if ParamCount <> -1 then
      begin
        ExpCount := CalcParamCount(ParamCount, IntfMD.MDA[I]);
        if ExpCount <> IntfMD.MDA[I].ParamCount then
          Continue;
      end;
      Result := I;
      Inc(NumNames);
    end;
  end;
  if (NumNames = 0) and (ParamCount <> -1) then
    Result := GetMethNum(IntfMD, MethName, -1);
  if NumNames > 1 then
    Result := -1;
end;

function SameTypeInfo(const RegInfo: PTypeInfo; const OtherInfo: PTypeInfo):
  Boolean;
begin
  Result := (RegInfo = OtherInfo) or
    ((RegInfo.Kind = OtherInfo.Kind) and TypeNamesMatch(RegInfo^.Name,
    OtherInfo^.Name));
end;

function TypeNamesMatch(const RegName: string; const OtherName: string): Boolean;
var
  I: Integer;
begin
  Result := (RegName = OtherName);
  if (not Result) then
  begin
    I := 1; { Start at one since we check OtherName first }
    while (I < Length(TypeInfoNames)) do
    begin
      if (OtherName = TypeInfoNames[I]) then
      begin
        Result := (RegName = TypeInfoNames[I - 1]);
        Exit;
      end;
      I := I + 2;
    end;
  end;
end;

function OtherTypeName(const TypeName: string): string;
var
  I: Integer;
begin
  I := 0;
  while (I < (Length(TypeInfoNames) - 1)) do
  begin
    if (TypeName = TypeInfoNames[I]) then
    begin
      Result := TypeInfoNames[I + 1];
      Exit;
    end;
    I := I + 2;
  end;
end;

//==============================================================================
// 参数处理规则相关过程，移植自 InvRules
//==============================================================================

function IsParamByRef(Flags: TParamFlags; ParamInfo: PTypeInfo; CC: TCallConv):
  Boolean;
begin
  Result := (pfVar in Flags) or (pfOut in Flags);

  if (not Result) and (ParamInfo.Kind = tkVariant) then
    Result := (pfConst in Flags) or (CC = ccPascal);

  if ParamInfo.Kind = tkString then
    Result := True;
end;

function GetTypeSize(P: PTypeInfo): Integer;
var
  TypeData: PTypeData;
begin
  Result := 4;
  TypeData := GetTypeData(P);
  case P.Kind of
    tkInteger:
      case TypeData^.OrdType of
        otSByte, otUByte:
          Result := SizeOf(Byte);
        otSWord, otUWord:
          Result := SizeOf(Word);
        otSLong, otULong:
          ;
      end;
    tkFloat:
      case TypeData.FloatType of
        ftSingle:
          Result := SizeOf(Single);
        ftDouble:
          Result := SizeOf(Double);
        ftComp:
          Result := SizeOf(Comp);
        ftCurr:
          Result := SizeOf(Currency);
        ftExtended:
          Result := SizeOf(Extended);
      end;
    tkChar:
      Result := 1;
    tkWChar:
      Result := 2;
    tkInt64:
      Result := SizeOf(Int64);
    tkVariant:
      Result := SizeOf(TVarData);
    tkEnumeration:
      Result := 1;
  end;
end;

function IsRetInAXDX(Info: PTypeInfo): Boolean;
begin
  Result := False;
  if Info <> nil then
    case Info.Kind of
      tkInt64:
        Result := True;
    end;
end;

function RetOnStack(Info: PTypeInfo): Boolean;
begin
  Result := False;
  if Info <> nil then
    case Info.Kind of
      tkLString,
      tkString,
      tkWString
      {$IFDEF UNICODE_STRING}, tkUString{$ENDIF}:
        Result := True;
      tkVariant:
        Result := True;
      tkDynArray:
        Result := True;
    end;
end;

function RetInFPU(Info: PTypeInfo): Boolean;
begin
  Result := False;
  if Info <> nil then
    case Info.Kind of
      tkFloat: Result := True;
    end;
end;

{
  GetStackTypeSize

  Returns the size that is actually allocated on the stack for a given
  type.  This is frequently different than the heap allocation for
  an object, because all stack pointers are allocated on 4 byte boundaries.
  So for example, the Extended type might occupy 10 bytes, but we will
  always allocate 12 bytes on the stack for it.
}
function GetStackTypeSize(P: PTypeInfo; CC: TCallConv): Integer;
var
  TypeData: PTypeData;
begin
  Result := 4;
  TypeData := GetTypeData(P);
  case P.Kind of
    tkFloat:
      case TypeData.FloatType of
        ftSingle:
          ;
        ftDouble, ftComp, ftCurr:
          Result := 8;
        ftExtended:
          Result := 10;
      end;
    tkInt64:
      Result := 8;
    tkVariant:
      if CC in [ccCdecl, ccStdCall, ccSafeCall] then
        Result := SizeOf(TVarData);
  end;

  // Make sure we're aligned on a 4 byte boundary
  Result := (Result + 3) and $FFFFFFFC;
end;

//==============================================================================
// 用于动态方法调用的临时数据类，移植修改自 InvokeRegistry
//==============================================================================

{ TDataContext }

procedure TDataContext.SetDataPointer(Index: Integer; P: Pointer);
begin
  DataP[Index] := P;
end;

function TDataContext.GetDataPointer(Index: Integer): Pointer;
begin
  Result := DataP[Index];
end;

procedure TDataContext.AddVariantToClear(P: PVarData);
var
  I: Integer;
begin
  for I := 0 to Length(VarToClear) -1 do
    if VarToClear[I] = P then
      Exit;
  I := Length(VarToClear);
  SetLength(VarToClear, I + 1);
  VarToClear[I] := P;
end;

procedure TDataContext.AddStrToClear(P: Pointer);
var
  I: Integer;
begin
  { If this string is in the list already, we're set }
  for I := 0 to Length(StrToClear) -1 do
    if StrToClear[I] = P then
      Exit;
  I := Length(StrToClear);
  SetLength(StrToClear, I + 1);
  StrToClear[I] := P;
end;

procedure TDataContext.AddWStrToClear(P: Pointer);
var
  I: Integer;
begin
  { If this WideString is in the list already, we're set }
  for I := 0 to Length(WStrToClear) -1 do
    if WStrToClear[I] = P then
      Exit;
  I := Length(WStrToClear);
  SetLength(WStrToClear, I + 1);
  WStrToClear[I] := P;
end;

constructor TDataContext.Create;
begin
  inherited;
end;

destructor TDataContext.Destroy;
var
  I: Integer;
  P: Pointer;
begin
  { Clean Variants we allocated }
  for I := 0 to Length(VarToClear) - 1 do
  begin
    if Assigned(VarToClear[I]) then
      Variant( PVarData(VarToClear[I])^) := NULL;
  end;
  SetLength(VarToClear, 0);

  { Clean up dynamic arrays we allocated }
  for I := 0 to Length(DynArrayToClear) - 1 do
  begin
    if Assigned(DynArrayToClear[I].P) then
    begin
      P := Pointer( PInteger(DynArrayToClear[I].P)^);
      DynArrayClear(P, DynArrayToClear[I].Info)
    end;
  end;
  SetLength(DynArrayToClear, 0);

  { Clean up strings we allocated }
  for I := 0 to Length(StrToClear) - 1 do
  begin
    if Assigned(StrToClear[I]) then
      PString(StrToClear[I])^ := '';
  end;
  SetLength(StrToClear, 0);

  { Clean up WideStrings we allocated }
  for I := 0 to Length(WStrToClear) - 1 do
  begin
    if Assigned(WStrToClear[I]) then
      PWideString(WStrToClear[I])^ := '';
  end;
  SetLength(WStrToClear, 0);

  inherited;
end;

procedure TDataContext.AddDynArrayToClear(P: Pointer; Info: PTypeInfo);
var
  I: Integer;
begin
  for I := 0 to Length(DynArrayToClear) -1 do
    if DynArrayToClear[I].P = P then
      Exit;
  I := Length(DynArrayToClear);
  SetLength(DynArrayToClear, I + 1);
  DynArrayToClear[I].P := P;
  DynArrayToClear[I].Info := Info;
end;

function TDataContext.AllocData(Size: Integer): Pointer;
begin
  Result := @Data[DataOffset];
  Inc(DataOffset, Size);
end;

{ TInvContext }

const
  MAXINLINESIZE = sizeof(TVarData) + 4;

procedure TInvContext.SetMethodInfo(const MD: TIntfMethEntry);
begin
  SetLength(DataP, MD.ParamCount + 1);
  SetLength(Data, (MD.ParamCount + 1) * MAXINLINESIZE);
end;

procedure TInvContext.SetParamPointer(Param: Integer; P: Pointer);
begin
   SetDataPointer(Param,  P);
end;

function TInvContext.GetParamPointer(Param: Integer): Pointer;
begin
  Result := GetDataPointer(Param);
end;

function TInvContext.GetResultPointer: Pointer;
begin
  Result := ResultP;
end;

procedure TInvContext.SetResultPointer(P: Pointer);
begin
  ResultP := P;
end;

procedure TInvContext.AllocServerData(const MD: TIntfMethEntry);
var
  I: Integer;
  Info: PTypeInfo;
  P: Pointer;
begin
  for I := 0 to MD.ParamCount - 1 do
  begin
    P := AllocData(GetTypeSize(MD.Params[I].Info));
    SetParamPointer(I, P);
    if MD.Params[I].Info.Kind = tkVariant then
    begin
      Variant(PVarData(P)^) := NULL;
      AddVariantToClear(PVarData(P));
    end else if MD.Params[I].Info.Kind = tkDynArray then
    begin
      AddDynArrayToClear(P, MD.Params[I].Info);
    end else if MD.Params[I].Info.Kind = tkLString then
    begin
      PString(P)^ := '';
      AddStrToClear(P);
    end else if (MD.Params[I].Info.kind = tkWString) {$IFDEF UNICODE_STRING} or (MD.Params[I].Info.kind = tkUString) {$ENDIF} then
    begin
      PWideString(P)^ := '';
      AddWStrToClear(P);
    end;
  end;
  if MD.ResultInfo <> nil then
  begin
    Info := MD.ResultInfo;
    case Info^.Kind of
      tkLString:
        begin
          P := AllocData(sizeof(PString));
          PString(P)^ := '';
          AddStrToClear(P);
        end;
      tkWString {$IFDEF UNICODE_STRING}, tkUString{$ENDIF}:
        begin
          P := AllocData(sizeof(PWideString));
          PWideString(P)^ := '';
          AddWStrToClear(P);
        end;
      tkInt64:
        P := AllocData(sizeof(Int64));
      tkVariant:
        begin
          P := AllocData(sizeof(TVarData));
          Variant( PVarData(P)^ ) := NULL;
          AddVariantToClear(PVarData(P));
        end;
      tkDynArray:
        begin
          P := AllocData(GetTypeSize(Info));
          AddDynArrayToClear(P, MD.ResultInfo);
        end;
      else
        P := AllocData(GetTypeSize(Info));
    end;
    SetResultPointer(P);
  end;
end;

//==============================================================================
// 动态接口方法调用器类，移植修改自 Invoke
//==============================================================================

constructor TInterfaceInvoker.Create;
begin
  inherited Create;
end;
{
  PushStackParm

  Copies an aligned number of bytes specified by ByteCount from the Parm
  to the current stack.  N.B.  We leave the bytes on the stack when we
  exit!

  Stack parameters come in many different sizes, ranging from 4 bytes to
  16 bytes.  This function copies a parameter of arbitrary
  size from a prior stack location (assumed the stack), onto the current
  stack.  On exit, we leave the additional bytes on the stack.  We use this
  to build the parameter list to the server side functions.

  We don't have to worry about copying bytes at the end of a page, because
  we assume that Parm is pointing to something higher up on the stack, and
  aligned on a proper stack boundary.
}
procedure PushStackParm(Parm: Pointer; ByteCount: Integer);
asm
        {
          EAX -> Parm (the parameter to be copied)
          EDX -> ByteCount (the number of bytes of data in Parm)
        }
        { We just use a jump table to copy the bits }
        LEA     ECX, @JT
{$IFDEF PIC}
        ADD     ECX, EBX
        ADD     ECX, EDX        // Assume that ByteCount is a DWORD multiple
        POP     EDX             // Remove and save the return address
        MOV     ECX, [ECX]
        ADD     ECX, EBX
        JMP     ECX
{$ELSE}
        ADD     ECX, EDX        // Assume that ByteCount is a DWORD multiple
        POP     EDX             // Remove and save the return address
        JMP     [ECX]
{$ENDIF}
@L4:
        PUSH    [EAX+12]
@L3:
        PUSH    [EAX+8]
@L2:
        PUSH    [EAX+4]
@L1:
        PUSH    [EAX]
        PUSH    EDX             // Push back the saved ret addr
        RET                     // All done
@JT:
        DD  0                   // 0 bytes (never happens)
        DD  @L1                 // 4 bytes
        DD  @L2                 // 8 bytes
        DD  @L3                 // 12 bytes
        DD  @L4                 // 16 bytes
end;

{
  GetFloatReturn

  Handles the nuances of retrieving the various different sized floating
  point values from the FPU and storing them in a buffer.
}
procedure GetFloatReturn(RetP: Pointer; FloatType: TFloatType);
asm
        CMP     EDX, ftSingle
        JE      @@Single
        CMP     EDX, ftDouble
        JE      @@Double
        CMP     EDX, ftExtended
        JE      @@Extended
        CMP     EDX, ftCurr
        JE      @@Curr
        CMP     EDX, ftComp
        JE      @@Curr      { Same as Curr  }
        { Should never get here :) }
@@Single:
        FSTP      DWORD PTR [EAX]
        WAIT
        RET
@@Double:
        FSTP      QWORD PTR [EAX]
        WAIT
        RET
@@Extended:
        FSTP      TBYTE PTR [EAX]
        WAIT
        RET
@@Curr:
        FISTP     QWORD PTR [EAX]
        WAIT
end;

procedure TInterfaceInvoker.Invoke(const Obj: TObject;
  IntfMD: TIntfMetaData; const MethNum: Integer;
  const Context: TInvContext);
var
  MethPos: Integer;
  Unk: IUnknown;
  IntfEntry: PInterfaceEntry;
  IntfVTable: Pointer;
  RetIsOnStack, RetIsInFPU, RetInAXDX: Boolean;
  I: Integer;
  RetP: Pointer;
  MD: TIntfMethEntry;
  DataP: Pointer;
  Temp, Temp1: Integer;
  RetEAX: Integer;
  RetEDX: Integer;
  TotalParamBytes: Integer;
  ParamBytes: Integer;
begin
{$IFDEF LINUX}
  try
{$ENDIF}
    TotalParamBytes := 0;
    MD := IntfMD.MDA[MethNUm];
    if not Obj.GetInterface(IntfMD.IID, Unk) then
      raise Exception.CreateFmt(SNoInterfaceGUID,
        [Obj.ClassName, GuidToString(IntfMD.IID)]);
    IntfEntry := Obj.GetInterfaceEntry(IntfMD.IID);
    IntfVTable := IntfEntry.VTable;
    MethPos := MD.Pos * 4; { Pos is absolute to whole VMT }
    if MD.ResultInfo <> nil then
    begin
      RetIsInFPU := RetInFPU(MD.ResultInfo);
      RetIsOnStack := RetOnStack(MD.ResultInfo);
      RetInAXDX := IsRetInAXDX(MD.ResultInfo);
      RetP := Context.GetResultPointer;
    end else
    begin
      RetIsOnStack := False;
      RetIsInFPU := False;
      RetInAXDX := False;
    end;

    if MD.CC in [ccCDecl, ccStdCall, ccSafeCall] then
    begin
      if (MD.ResultInfo <> nil) and (MD.CC = ccSafeCall) then
        asm PUSH DWORD PTR [RetP] end;
      for I := MD.ParamCount - 1 downto 0 do
      begin
        DataP := Context.GetParamPointer(I);
        if IsParamByRef(MD.Params[I].Flags, MD.Params[I].Info, MD.CC) then
          asm
        PUSH DWORD PTR [DataP]
          end
        else
        begin
          ParamBytes := GetStackTypeSize(MD.Params[I].Info, MD.CC);
          PushStackParm(DataP, ParamBytes);
          Inc(TotalParamBytes, ParamBytes);
        end;
      end;
      asm PUSH DWORD PTR [Unk] end;
      if RetIsOnStack and (MD.CC <> ccSafeCall) then
        asm PUSH DWORD PTR [RetP] end;
    end
    else if MD.CC = ccPascal then
    begin
      for I := 0 to MD.ParamCount - 1 do
      begin
        DataP := Context.GetParamPointer(I);
        if IsParamByRef(MD.Params[I].Flags, MD.Params[I].Info, MD.CC) then
          asm
         PUSH DWORD PTR [DataP]
          end
        else
        begin
//        PushStackParm(DataP, GetStackTypeSize(MD.Params[I].Info, MD.CC));
          ParamBytes := GetStackTypeSize(MD.Params[I].Info, MD.CC);
          PushStackParm(DataP, ParamBytes);
          Inc(TotalParamBytes, ParamBytes);
        end;
      end;
      if RetIsOnStack then
        asm PUSH DWORD PTR [RetP] end;
      asm PUSH DWORD PTR [Unk] end;
    end else
      raise Exception.CreateFmt(SUnsupportedCC, [CallingConventionName[MD.CC]]);

    if MD.CC <> ccSafeCall then
    begin
      asm
      MOV DWORD PTR [Temp], EAX
      MOV DWORD PTR [Temp1], ECX
      MOV EAX, MethPos
      MOV ECX, [IntfVtable]
      MOV ECX, [ECX + EAX]
      CALL ECX
      MOV DWORD PTR [RetEAX], EAX
      MOV DWORD PTR [RetEDX], EDX
      MOV EAX, DWORD PTR [Temp]
      MOV ECX, DWORD PTR [Temp1]
      end;
    end else
    begin
      asm
      MOV DWORD PTR [Temp], EAX
      MOV DWORD PTR [Temp1], ECX
      MOV EAX, MethPos
      MOV ECX, [IntfVtable]
      MOV ECX, [ECX + EAX]
      CALL ECX
      CALL System.@CheckAutoResult
      MOV DWORD PTR [RetEAX], EAX
      MOV DWORD PTR [RetEDX], EDX
      MOV EAX, DWORD PTR [Temp]
      MOV ECX, DWORD PTR [Temp1]
      end;
    end;

  // If we're cdecl, we're responsible for cleanup up the stack.
    if MD.CC = ccCDecl then
      asm
    MOV EAX, DWORD PTR [TotalParamBytes]
    ADD ESP, EAX
      end;

    if MD.ResultInfo <> nil then
    begin
      if MD.CC <> ccSafeCall then
      begin
        if RetIsInFPU then
        begin
          GetFloatReturn(RetP, GetTypeData(MD.ResultInfo).FloatType);
        end else if not RetIsOnStack then
        begin
          if RetInAXDX then
            asm
            PUSH EAX
            PUSH ECX
            MOV EAX, DWORD PTR [RetP]
            MOV ECX, DWORD PTR [RetEAX]
            MOV [EAX], ECX
            MOV ECX, DWORD PTR [RetEDX]
            MOV [EAX + 4], ECX
            POP ECX
            POP EAX
            end
          else
            asm
            PUSH EAX
            PUSH ECX
            MOV EAX, DWORD PTR [RetP]
            MOV ECX, DWORD PTR [RetEAX]
            MOV [EAX], ECX
            POP ECX
            POP EAX
            end;
        end;
      end;
    end;
{$IFDEF LINUX}
  except
    // This little bit of code is required to reset the stack back to a more
    // resonable state since the exception unwinder is completely unaware of
    // the stack pointer adjustments made in this function.
    asm
      MOV EAX, DWORD PTR [TotalParamBytes]
      ADD ESP, EAX
    end;
    raise;
  end;
{$ENDIF}
end;

//==============================================================================
// 类型转换器类，移植修改自 TypeTrans
//==============================================================================

{ TTypeTranslator }

function GetEnumValueEx(TypInfo: PTypeInfo; const Name: string): Integer;
var
  PName: string;
begin
  PName := Name;
  if SameTypeInfo(TypeInfo(System.Boolean), TypInfo) or
    SameTypeInfo(TypeInfo(System.ByteBool), TypInfo) or
    SameTypeInfo(TypeInfo(System.WordBool), TypInfo) or
    SameTypeInfo(TypeInfo(System.LongBool), TypInfo) then
  begin
    if SameText(Name, 'true') or SameText(Name, '1') then { Do not localize }
      PName := 'True' { Do not localize }
    else if SameText(Name, 'false') or SameText(Name, '0') then { Do not localize }
      PName := 'False'; { Do not localize }
    Result := GetEnumValue(TypeInfo(System.Boolean), PName);
  end else
  begin
    Result := GetEnumValue(TypInfo, PName);
  end;
end;

// 转换一个集合值为字符串
function SetToStr(TypeInfo: PTypeInfo; Value: TIntegerSet): string;
var
  EnumValue: 0..SizeOf(Integer) * 8 - 1;
begin
  Assert(TypeInfo^.Kind in [tkEnumeration, tkSet]);
  if TypeInfo^.Kind = tkSet then
    TypeInfo := GetTypeData(TypeInfo)^.CompType^;

  Result := '';
  for EnumValue := GetTypeData(TypeInfo)^.MinValue to
    GetTypeData(TypeInfo)^.MaxValue do
    if EnumValue in Value then
      if Result = '' then
        Result := GetEnumName(TypeInfo, EnumValue)
      else
        Result := Result + ', ' + GetEnumName(TypeInfo, EnumValue);
  Result := '[' + Result + ']';
end;

// 转换一个字符串到集合
function StrToSet(TypeInfo: PTypeInfo; const Value: string): TIntegerSet;
resourcestring
  SInvalidSetStr = '''%s'' is not a valid set string';
var
  EnumValue: 0..SizeOf(Integer) * 8 - 1;
  S: string;
  Strings: TStrings;
  i: Integer;
begin
  Assert(TypeInfo^.Kind in [tkEnumeration, tkSet]);
  if TypeInfo^.Kind = tkSet then
    TypeInfo := GetTypeData(TypeInfo)^.CompType^;

  Result := [];
  S := Trim(Value);
  if (S[1] = '[') and (S[Length(S)] = ']') then
  begin
    S := Copy(S, 2, Length(S) - 2);
    Strings := TStringList.Create;
    try
      Strings.CommaText := S;
      for i := 0 to Strings.Count - 1 do
      begin
        EnumValue := GetEnumValue(TypeInfo, Trim(Strings[i]));
        if (EnumValue < GetTypeData(TypeInfo)^.MinValue) or
          (EnumValue > GetTypeData(TypeInfo)^.MaxValue) then
          raise EConvertError.Create(Format(SInvalidSetStr, [Value]));

        Include(TIntegerSet(Result), EnumValue);
      end;
    finally
      Strings.Free;
    end;
  end;
end;

// 转换一个标识符为整数（Color、CharSet等）
function IdentToInt(TypeInfo: PTypeInfo; const Value: string): Integer;
var
  IdToInt: TIdentToInt;
  IntValue: Integer;
begin
  Assert(TypeInfo^.Kind = tkInteger);

  IdToInt := FindIdentToInt(TypeInfo);
  if Assigned(IdToInt) and IdToInt(Value, IntValue) then
    Result := IntValue
  else
    Result := StrToInt(Value);
end;

constructor TTypeTranslator.Create;
begin
  inherited Create;
end;

destructor TTypeTranslator.Destroy;
begin
  inherited;
end;

type
  PWideChar = ^WideChar;

procedure TTypeTranslator.CastVariantToNative(Info: PTypeInfo; const Value:
  OleVariant; NatData: Pointer);
var
  ParamTypeData: PTypeData;
  AnsiStr: string;
  WideStr: WideString;
  Int: Integer;
begin
  if VarIsNull(Value) or VarIsEmpty(Value) then
    Exit;

  ParamTypeData := GetTypeData(Info);
  case Info^.Kind of
    tkInteger:
      begin
        // 处理 TColor、CharSet 等标识符
        Int := IdentToInt(Info, Value);
        case ParamTypeData^.OrdType of
          otSByte,
            otUByte:
            PByte(NatData)^ := Int;
          otSWord,
            otUWord:
            PSmallint(NatData)^ := Int;
          otSLong,
            otULong:
            PInteger(NatData)^ := Int;
        end;
      end;
    tkFloat:
      case ParamTypeData^.FloatType of
        ftSingle:
          PSingle(NatData)^ := Value;
        ftDouble:
          begin
            if Info = TypeInfo(TDateTime) then
              PDateTime(NatData)^ := Value
            else
              PDouble(NatData)^ := Value;
          end;
        ftComp:
          PComp(NatData)^ := Value;
        ftCurr:
          PCurrency(NatData)^ := Value;
        ftExtended:
          PExtended(NatData)^ := Value;
      end;
    tkInt64:
      PInt64(NatData)^ := Value;
    tkChar:
      begin
        AnsiStr := Value;
        if AnsiStr <> '' then
          PChar(NatData)^ := AnsiStr[1];
      end;
    tkWChar:
      begin
        WideStr := Value;
        if WideStr <> '' then
          PWideChar(NatData)^ := WideStr[1];
      end;
    tkWString:
      PWideString(NatData)^ := Value;
{$IFDEF UNICODE_STRING}
    tkUString:
      PUnicodeString(NatData)^ := Value;
{$ENDIF}
    tkString:
      PShortString(NatData)^ := Value;
    tkLString:
      PString(NatData)^ := Value;
    tkEnumeration:
      { NOTE: Here we assume enums to be byte-size; make sure (specially for C++)
              that enums have generated with the proper size }
      PByte(NatData)^ := GetEnumValueEx(Info, Value);
    tkClass:
      PInteger(NatData)^ := Value;
    tkSet, tkMethod, { TODO -oyygw : 增加对集合类型的处理 }
      tkArray, tkRecord, tkInterface,
      tkDynArray:
      raise ETypeTransException.CreateFmt(SUnexpectedDataType,
        [KindNameArray[Info.Kind]]);
    tkVariant:
      Variant(PVarData(NatData)^) := Value;
  end;
end;

procedure TTypeTranslator.CastNativeToVariant(Info: PTypeInfo;
  var Value: OleVariant; NatData: Pointer);
var
  TypeData: PTypeData;
begin
  TypeData := GetTypeData(Info);
  case Info.Kind of
    tkInteger:
      case TypeData.OrdType of
        otSByte, otUByte:
          Value := Byte(NatData^);
        otSWord, otUWord:
          Value := SmallInt(NatData^);
        otSLong, otULong:
          Value := Integer(NatData^);
      end;
    tkFloat:
      case TypeData.FloatType of
        ftSingle:
          Value := Single(NatData^);
        ftDouble:
          begin
            if Info = TypeInfo(TDateTime) then
              Value := TDateTime(NatData^)
            else
              Value := Double(NatData^);
          end;
        ftComp:
          Value := Comp(NatData^);
        ftCurr:
          Value := Currency(NatData^);
        ftExtended:
          Value := Extended(NatData^);
      end;
    tkInt64:
      Value := Int64(NatData^);
    tkChar:
      Value := Char(NatData^);
    tkWChar:
      Value := WideChar(NatData^);
    tkWString:
      Value := PWideString(NatData)^;
{$IFDEF UNICODE_STRING}
    tkUString:
      Value := PUnicodeString(NatData)^;
{$ENDIF}
    tkString:
      Value := PShortString(NatData)^;
    tkLString:
      Value := PAnsiString(NatData)^;
    tkEnumeration:
      { NOTE: Here we assume enums to be byte-size; make sure (specially for C++)
              that enums have generated with the proper size }
      Value := GetEnumName(Info, PByte(NatData)^);
    tkClass:
      Value := PInteger(NatData)^; // 对象按指针（整数）处理
    tkSet, tkMethod, { TODO -oyygw : 增加对集合类型的处理 }
      tkArray, tkRecord, tkInterface,
      tkDynArray:
      raise ETypeTransException.CreateFmt(SUnexpectedDataType,
        [KindNameArray[Info.Kind]]);
    tkVariant:
      Value := Variant(PVarData(NatData)^);
  end;
end;

initialization

finalization
  if Assigned(FInterfaceInvoker) then
    FreeAndNil(FInterfaceInvoker);
  if Assigned(FTypeTranslator) then
    FreeAndNil(FTypeTranslator);

end.

