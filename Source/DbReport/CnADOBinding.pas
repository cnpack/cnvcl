{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2023 CnPack 开发组                       }
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

unit CnADOBinding;
{* |<PRE>
================================================================================
* 软件名称：CnPack 组件包
* 单元名称：查询分析器控件仿 VC++ 的数据绑定单元
* 单元作者：penal
*           不得闲 (appleak46@yahoo.com.cn)
* 备    注：Delphi 翻译版本
* 开发平台：PWinXP + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2007.11.24 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses 
  Windows, Classes, SysUtils, ActiveX
{$IFDEF COMPILER6_UP}
  , Variants
{$ENDIF}
  ;

//VC++
//说明了允许客户提取一行数据并直接送至类数据成员的接口。客户程序需要在其类中包含绑定条目，以指定 Recordset Field 对象和类数据成员之间的关联。
const
// enum ADOFieldStatusEnum  枚举ADO Status 属性
  adFldOK = 0;                  // 一个非NULL的值返回
  adFldBadAccessor = 1;         // 绑定无效.
  adFldCantConvertValue = 2;    // 指示字段可以在不丢失任何数据的情况下被提取和储存
  adFldNull = 3;                // 返回了一个NULL值
  adFldTruncated = 4;           // 指示从数据源读取数据时缩略的可变长值数据
  adFldSignMismatch = 5;        //指示通过技术提供对象[provider]返回的数值带有正负号；但是ADO字段值的数值类型不带有正负号
  adFldDataOverFlow = 6;        // 指示从技术提供对象[provider]返回的数值溢出字段的数值类型
  adFldCantCreate = 7;      //由于技术提供对象[provider]超出限制要求，所以不能再添加字段
  adFldUnavailable = 8;     //当从数据源读取数据时，技术提供对象[provider]不能决定其值
  adFldPermissionDenied = 9; //指示字段不能被更改，因为其属性为只读
  adFldIntegrityViolation = 10; //指示字段不能被更改，因为它食一个已被计算过或已被导出的实体
  adFldSchemaViolation = 11; //指示值违反了数据源计划[data source schema]对字段的限制
  adFldBadStatus = 12;  //指示一个从ADO发送到OLEDB提供者的有效状态值
  adFldDefault = 13;  //设置默认使用的字段值 ................
   //..........................
type
  PADO_BINDING_ENTRY = ^ADO_BINDING_ENTRY;
  ADO_BINDING_ENTRY = record
    ulOrdinal: UINT;
    wDataType: Word;
    bPrecision: Byte;
    bScale: Byte;
    ulSize: UINT;
    ulBufferOffset: UINT;
    ulStatusOffset: UINT;
    ulLengthOffset: UINT;
    ulADORecordBindingOffset: UINT;
    fModify: BOOL;
  end;
  TADOBindingEntry = ADO_BINDING_ENTRY;
  PADOBindingEntry = ^TADOBindingEntry;
  //IADORecordBinding 接口具有使 Recordset 字段与 C/C++ 变量关联、添加新行和执行更新的方法。所有这三个方法都可使指针指向来自 CADORecordBinding 的类，该 CADORecordBinding 定义每个字段和变量之间的绑定。
  IADORecordBinding = interface
    ['{00000544-0000-0010-8000-00AA006D2EA4}']
    function BindToRecordset(BindInfo: Pointer): HResult; stdcall; //调用该方法可关联变量与字段
    function AddNew(BindInfo: Pointer): HResult; stdcall; //调用该方法可直接调用 ADO AddNew 方法
    function Update(BindInfo: Pointer): HResult; stdcall;//调用该方法可直接调用 ADO Update 方法。
  end;

  EADOBindingException = class(Exception);

  PColumnRawData = ^TColumnRawData;
  TColumnRawData = record
    DataLength: LongWord;
    Status: LongWord;
    RawData: array[0..0] of Byte;
  end;

  TADOBinding = class
  private
    FEntries: array of TADOBindingEntry;
    FBindingData: Pointer;
    FDataSize: Integer;
    FPtrList: array of Pointer;
    function CalcDataSize(data_type: Word; data_size: Integer): LongWord;//计算字段类型长度
    procedure CheckIndex(Index: Integer);
    procedure CheckStatus(Index: Integer);
    procedure Error(status: UINT); overload;
    procedure Error(const msg: string); overload;
    function GetAsInteger(Index: Integer): Integer;
    function GetAsString(Index: Integer): string;
    function GetLen(Index: Integer): Integer;
    function GetStatus(Index: Integer): Integer;
    function GetAsDouble(Index: Integer): Double;
    function GetIsNull(Index: Integer): Boolean;
    function GetAsRawData(Index: Integer): PColumnRawData;
    function GetAsSingle(Index: Integer): Single;
  public
    constructor Create(bindings: array of TADOBindingEntry); overload;
    constructor Create; overload;
    destructor Destroy; override;

    // 绑定定长数据
    procedure AddBinding(ordinal: UINT; data_type: Word;
                        modify: Boolean); overload;
    // 绑定变长数据adVarChar等
    procedure AddBinding(ordinal: UINT; data_type: Word;
                        data_size: UINT; modify: Boolean); overload;
    // 绑定数值数据adNumeric等
    procedure AddBinding(ordinal: UINT; data_type: Word;
                        precision, scale: Byte; modify: Boolean); overload;

    function GetADOBindingData: Pointer;
    procedure ClearBuffer;
    property AsString[Index: Integer]: string read GetAsString;
    property AsInteger[Index: Integer]: Integer read GetAsInteger;
    property AsDouble[Index: Integer]: Double read GetAsDouble;
    property AsSingle[Index: Integer]: Single read GetAsSingle;
    property AsRawData[Index: Integer]: PColumnRawData read GetAsRawData;
    property Status[Index: Integer]: Integer read GetStatus;
    property Len[Index: Integer]: Integer read GetLen;
    property IsNull[Index: Integer]: Boolean read GetIsNull;
  end;

  TDefaultBindingInfo = class
  private
    FEntries: PADOBindingEntry;
  public
    function GetADOBindingEntries: PADOBindingEntry; virtual; stdcall;
    constructor Create(bindings: PADOBindingEntry);
  end;

implementation

uses
  ADOInt, OleDB;

function TDefaultBindingInfo.GetADOBindingEntries: PADOBindingEntry;
begin
  Result := FEntries;
end;

constructor TDefaultBindingInfo.Create(bindings: PADOBindingEntry);
begin
  FEntries := bindings;
end;

{ TADOBinding }

procedure TADOBinding.AddBinding(ordinal: UINT; data_type: Word;
  modify: Boolean);
var
  I: Integer;
begin
  SetLength(FEntries, Length(FEntries) + 1);
  I := Length(FEntries) - 2;
  FEntries[I].ulOrdinal := ordinal;
  FEntries[I].wDataType := data_type;
  FEntries[I].fModify := modify;
end;

procedure TADOBinding.AddBinding(ordinal: UINT; data_type: Word;
  data_size: UINT; modify: Boolean);
var
  I: Integer;
begin
  SetLength(FEntries, Length(FEntries) + 1);
  I := Length(FEntries) - 2;
  FEntries[I].ulOrdinal := ordinal;
  FEntries[I].wDataType := data_type;
  FEntries[I].ulSize := data_size;
  FEntries[I].fModify := modify;
end;

procedure TADOBinding.AddBinding(ordinal: UINT; data_type: Word; precision,
  scale: Byte; modify: Boolean);
var
  I: Integer;
begin
  SetLength(FEntries, Length(FEntries) + 1);
  I := Length(FEntries) - 2;
  FEntries[I].ulOrdinal := ordinal;
  FEntries[I].wDataType := data_type;
  FEntries[I].bPrecision := precision;
  FEntries[I].bScale := scale;
  FEntries[I].fModify := modify;
end;

function TADOBinding.CalcDataSize(data_type: Word; data_size: Integer): LongWord;
begin
  Result := 0;
  case data_type of
    adTinyInt: Result := 1;
    adSmallInt: Result := 2;
    adInteger: Result := 4;
    adBigInt: Result := 8;
    adUnsignedTinyInt: Result := 1;
    adUnsignedSmallInt: Result := 2;
    adUnsignedInt: Result := 4;
    adUnsignedBigInt: Result := 8;
    adSingle: Result := 4;
    adDouble: Result := 8;
    adCurrency: Result := 8;
    adDecimal: Result := SizeOf(DECIMAL);
    adNumeric: Result := SizeOf(DB_NUMERIC);
    adBoolean: Result := SizeOf(BOOL);
    adError: Result := SizeOf(Integer);
    adVariant: Result := SizeOf(Variant);
    adIDispatch,
    adIUnknown: Result := SizeOf(Pointer);
    adGUID: Result := SizeOf(TGuid);
    adDate: Result := SizeOf(TDateTime);
    adDBDate: Result := SizeOf(DBDATE);
    adDBTime: Result := SizeOf(DBTIME);
    adDBTimeStamp: Result := SizeOf(DBTIMESTAMP);
    adBSTR: Result := SizeOf(Pointer);
    adChar,
    adVarChar,
    adLongVarChar: Result := data_size + 1;
    adWChar,
    adVarWChar,
    adLongVarWChar: Result := data_size + 2;

    adBinary,
    adVarBinary,
    adLongVarBinary: Result := data_size;

    adFileTime: Result := sizeof(FILETIME);
  else
    Error('Unsupported data type.');
// unsupported data type:
//  adChapter = $00000088;
//  adDBFileTime = $00000089;
//  adPropVariant = $0000008A;
//  adVarNumeric = $0000008B;
//  adUserDefined = $00000084;

  end;
end;

procedure TADOBinding.CheckIndex(Index: Integer);
begin
  if (Index < 0) or (Index >= Length(FPtrList)) then
    Error('CheckIndex: Index out of bound.');
end;

procedure TADOBinding.CheckStatus(Index: Integer);
var
  stat: UINT;
begin
  stat := Self.GetStatus(Index);
  if not (stat in [adFldOK, adFldNull, adFldTruncated, adFldDefault]) then
    Error(stat);
end;

constructor TADOBinding.Create(bindings: array of TADOBindingEntry);
var
  I: Integer;
begin
  SetLength(FEntries, Length(bindings) + 1);  //初始化
  for I := 0 to Length(bindings)-1 do
    FEntries[I] := bindings[I];
end;

procedure TADOBinding.ClearBuffer;
begin
  FillChar(FBindingData^, FDataSize, 0);
end;

constructor TADOBinding.Create;
begin
  SetLength(FEntries, 1);
end;

destructor TADOBinding.Destroy;
begin
  if FBindingData <> nil then
    FreeMem(FBindingData);
  FBindingData := nil;
  inherited;
end;

procedure TADOBinding.Error(status: UINT);
const
  stat_err_msg: array[1..13] of string = (
        'Binding invalid.',       // adFldBadAccessor = 1
        'Value Can not Be Convert', // adFldCantConvertValue = 2
        'Value Is NULL', // adFldNull = 3; not error
        'Variable-length data or numeric digits were truncated.', // adFldTruncated = 4
        'Value is signed and variable data type is unsigned.', // adFldSignMismatch = 5
        'Value is larger than could be stored in the variable data type.', // adFldDataOverFlow = 6
        'Unknown column type and field already open.', // adFldCantCreate = 7;
        'Field value could not be determined.', // adFldUnavailable = 8;
        'When updating, no permission to write data.', // adFldPermissionDenied = 9;
        'When updating, field value would violate column integrity.', // adFldIntegrityViolation = 10;
        'When updating, field value would violate column schema.', // adFldSchemaViolation = 11;
        'When updating, invalid status parameter.', // adFldBadStatus = 12;
        'When updating, a default value was used.'  // adFldDefault = 13; not error
      );
begin
  if (status > 0) and (status <= adFldDefault) then
    raise EADOBindingException.Create(stat_err_msg[status])
  else
    raise EADOBindingException.Create('');
end;

procedure TADOBinding.Error(const msg: string);
begin
  raise EADOBindingException.Create(msg);
end;

function TADOBinding.GetADOBindingData: Pointer;
type
  // TDefaultBindingInfo 的类结构.
  PBindClassRec = ^BindClassRec;
  BindClassRec = record
    VMT: Pointer;
    Entries: Pointer;
  end;

var
  I: Integer;
  data_size: LongWord;
begin
  if FBindingData <> nil then
  begin
    Result := FBindingData;
    Exit;
  end;

  SetLength(FPtrList, Length(FEntries)-1);
  data_size := SizeOf(BindClassRec);
  data_size := (data_size + 7) and (not 7);
  for I := 0 to Length(FEntries)-2 do
  begin
    FEntries[I].ulLengthOffset := data_size;
    FEntries[I].ulStatusOffset := data_size + 4;
    FEntries[I].ulBufferOffset := data_size + 8;

    Inc(data_size, 8); // data length, data status
    FPtrList[I] := Pointer(data_size);
    Inc(data_size, CalcDataSize(FEntries[I].wDataType, FEntries[I].ulSize));
    data_size := (data_size + 7) and (not 7);
  end;
  FDataSize := data_size;
  FBindingData := AllocMem(data_size);
  PBindClassRec(FBindingData)^.VMT := Pointer(TDefaultBindingInfo); //PPointer(FClass)^;
  PBindClassRec(FBindingData)^.Entries := @FEntries[0];
  for I := 0 to Length(FEntries)-2 do
  begin
    FPtrList[I] := Pointer(Cardinal(FPtrList[I]) + Cardinal(FBindingData)); 
  end;

  Result := FBindingData;
end;

function TADOBinding.GetAsDouble(Index: Integer): Double;
begin
//  CheckIndex(Index);
  CheckStatus(Index);
  Result := PDouble(FPtrList[Index])^;
end;

function TADOBinding.GetAsInteger(Index: Integer): Integer;
begin
//  CheckIndex(Index);
  CheckStatus(Index);
  Result := PInteger(FPtrList[Index])^;
end;

function TADOBinding.GetAsRawData(Index: Integer): PColumnRawData;
begin
  CheckIndex(Index);
  Result := PColumnRawData(Cardinal(FPtrList[Index]) - 8);
end;

function TADOBinding.GetAsSingle(Index: Integer): Single;
begin
  CheckStatus(Index);
  Result := PSingle(FPtrList[Index])^;
end;

function TADOBinding.GetAsString(Index: Integer): string;
var
  len: Integer;
begin
  CheckStatus(Index);
  len := GetLen(Index);
  SetString(Result, PChar(FPtrList[Index]), len);
end;

function TADOBinding.GetIsNull(Index: Integer): Boolean;
begin
  Result := GetStatus(Index) = adFldNull;
end;

function TADOBinding.GetLen(Index: Integer): Integer;
var
  len: PLongWord;
begin
  CheckIndex(Index);
  len := PLongWord(Cardinal(FPtrList[Index]) - 8);
  Result := len^;
end;

function TADOBinding.GetStatus(Index: Integer): Integer;
var
  stat: PLongWord;
begin
  CheckIndex(Index);
  stat := PLongWord(Cardinal(FPtrList[Index]) - 4);
  Result := stat^;
end;

end.
