{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2026 CnPack 开发组                       }
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
{            网站地址：https://www.cnpack.org                                  }
{            电子邮件：master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnIni;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：扩展的 INI 访问单元，支持 Win32/64 和 Posix
* 单元作者：周劲羽 (zjy@cnpack.org)
* 备    注：该单元编写时参考了 RxLib 2.75 中的 RxIni.pas
*           编译时如出现 Graphics 找不到，请按编译平台是否 Windows 在工程选项中
*           添加 Vcl 或 FMX 前缀
* 开发平台：PWin2000Pro + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6 + Lazarus 4.0
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2026.03.30 V1.3
*               增加 WideString 版本的 IniFile 实现
*           2025.06.23 V1.2
*               在 FPC3 下编译通过
*           2024.02.23 V1.1
*               加入 ReadStringsBoolean 和 WriteStringsBoolean 的功能
*           2002.10.20 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF DELPHI}
  {$DEFINE SUPPORT_ZLIB}
{$ENDIF}

uses
  {$IFDEF MSWINDOWS} Windows, {$ELSE} System.Types, System.UITypes, System.UIConsts, {$ENDIF}
  Classes, SysUtils, TypInfo, IniFiles, Graphics, // FMX 下如果 Graphics 找不到，需要工程选项里加 FMX 前缀
  CnIniStrUtils, CnStream, CnWideStrings {$IFDEF SUPPORT_ZLIB}, ZLib{$ENDIF};

type

//==============================================================================
// 扩展的 INI 访问类
//==============================================================================
   
{ TCnIniFile }

  TCnIniFile = class(TCustomIniFile)
  {* 扩展的 INI 访问类，使用 Wrap 模式对 TCustomIniFile 进行扩展。定义两个构造器
     既可当普通的文件型 INI 类操作，又可仅仅作为其它 TCustomIniFile 对象的包装外
     壳进行扩展的操作。}
  private
    FIni: TCustomIniFile;
    FOwned: Boolean;
    function GetFileName: string;
    function IsBooleanType(PInfo: PTypeInfo): Boolean;
    function IsBoolType(PInfo: PTypeInfo): Boolean;
    function IsColorType(PInfo: PTypeInfo): Boolean;
    function IsDateTimeType(PInfo: PTypeInfo): Boolean;
  protected
    property Owned: Boolean read FOwned;
    property Ini: TCustomIniFile read FIni;
  public
    constructor Create(AIni: TCustomIniFile; AOwned: Boolean = False); overload;
    {* 包装构造器，使用该构造器创建实例，对已有的 TCustomIniFile 对象进行功能扩展
       对象的所有方法都转到原 INI 对象中执行
     |<PRE>
       AIni: TCustomIniFile    - 被包装的 INI 对象
       AOwned: Boolean         - 在该对象释放时是否同时释放被包装的 INI 对象
     |</PRE>}
    constructor Create(const AFileName: string; MemIniFile: Boolean = True); overload;
    {* 普通 INI 文件构造器，使用该构造器创建实例，将实例当普通的 INI 对象使用。
     |<PRE>
       FileName: string        - INI 文件名
       MemIniFile: Boolean     - 是否使用内存缓冲方式操作 INI，即内部使用 TMemIniFile 对象。
     |</PRE>}
    destructor Destroy; override;

{$IFDEF INIFILE_READWRITE_INTEGER}
    function ReadInteger(const Section, Ident: string; Default: Integer): Integer; override;
    procedure WriteInteger(const Section, Ident: string; Value: Integer); override;
{$ELSE}
    function ReadInteger(const Section, Ident: string; Default: LongInt): LongInt; override;
    procedure WriteInteger(const Section, Ident: string; Value: LongInt); override;
{$ENDIF}
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean; override;
    procedure WriteBool(const Section, Ident: string; Value: Boolean); override;
    function ReadDate(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function ReadDateTime(const Section, Name: string; Default: TDateTime): TDateTime; override;
    function ReadFloat(const Section, Name: string; Default: Double): Double; override;
    function ReadTime(const Section, Name: string; Default: TDateTime): TDateTime; override;
    procedure WriteDate(const Section, Name: string; Value: TDateTime); override;
    procedure WriteDateTime(const Section, Name: string; Value: TDateTime); override;
    procedure WriteFloat(const Section, Name: string; Value: Double); override;
    procedure WriteTime(const Section, Name: string; Value: TDateTime); override;
    function ReadString(const Section, Ident, Default: string): string; override;
    procedure WriteString(const Section, Ident, Value: String); override;
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    procedure EraseSection(const Section: string); override;
    procedure DeleteKey(const Section, Ident: String); override;
    procedure UpdateFile; override;
    
    function ReadColor(const Section, Ident: string; Default: TColor): TColor;
    {* 读取颜色}
    procedure WriteColor(const Section, Ident: string; Value: TColor);
    {* 写入颜色}
    function ReadFont(const Section, Ident: string; Font: TFont): TFont;
    {* 读取字体，将读到的字体属性设给 Font 参数，并将其返回}
    procedure WriteFont(const Section, Ident: string; Font: TFont);
    {* 写入字体}
    function ReadRect(const Section, Ident: string; const Default: TRect): TRect;
    {* 读取 Rect}
    procedure WriteRect(const Section, Ident: string; const Value: TRect);
    {* 写入 Rect}
    function ReadPoint(const Section, Ident: string; const Default: TPoint): TPoint;
    {* 读取 Point}
    procedure WritePoint(const Section, Ident: string; const Value: TPoint);
    {* 写入 Point}
    function ReadStrings(const Section, Ident: string; Strings: TStrings): TStrings; overload;
    {* 从一行文本中读取字符串列表，换行用 \n 表示}
    function ReadStringsBoolean(const Section, Ident: string; Strings: TStringList): TStringList;
    {* 从一行文本中读取带 Boolean 标记的字符串列表，Boolean 值为 Objects[Index] <> nil
      以 | 作为 Boolean 标记（0 或 1）与具体字符串的分隔符，换行用 \n 表示}
    function ReadStrings(const Section: string; Strings: TStrings): TStrings; overload;
    {* 从单独的节中读取字符串列表}
    procedure WriteStrings(const Section, Ident: string; Strings: TStrings); overload;
    {* 写入字符串列表到一行文本中}
    procedure WriteStrings(const Section: string; Strings: TStrings); overload;
    {* 写入字符串列表到单独的节中}
    procedure WriteStringsBoolean(const Section, Ident: string; Strings: TStringList);
    {* 写入带 Boolean 标记的字符串列表到单独的节中，Boolean 值为 Objects[Index] <> nil
      以 | 作为 Boolean 标记（0 或 1）与具体字符串的分隔符，换行用 \n 表示}
    procedure ReadObject(const Section: string; AObject: TObject);
    {* 读取对象 published 属性，不包含子属性}
    procedure WriteObject(const Section: string; AObject: TObject; NoDef: Boolean = True);
    {* 写入对象到单独的节中，不处理子属性，支持 TFont 和 TStrings 类型。NoDef 指定是否不保存默认值}
    property FileName: string read GetFileName;
    {* INI 文件名}
  end;

//==============================================================================
// 支持流操作的 IniFile 类
//==============================================================================
   
{ TCnStreamIniFile }

  TCnStreamIniFile = class (TMemIniFile)
  {* 支持流操作的 IniFile 类，提供了 LoadFromStream、SaveToStream 允许从流中读取
     Ini 数据。 }
  private
    FFileName: string;
    FInitData: string;
  protected

  public
    constructor Create(const AFileName: string = '');
    {* 类构造器，参数为 INI 文件名，如果该文件存在则会自动装载文件 }
    destructor Destroy; override;
    {* 类析构器 }
    function LoadFromFile(const AFileName: string): Boolean;
    {* 从文件中装载 INI 数据 }
    function LoadFromStream(AStream: TStream): Boolean; virtual;
    {* 从流中装载 INI 数据 }
    function SaveToFile(const AFileName: string): Boolean;
    {* 保存 INI 数据到文件 } 
    function SaveToStream(AStream: TStream): Boolean; virtual;
    {* 保存 INI 数据到流 }
    procedure UpdateFile; override;
    {* 更新当前 INI 数据到文件 }

    property FileName: string read FFileName;
    {* 创建对象时传递的文件名，只读属性 }
  end;

  TCnWideIniSection = class
  public
    Name: TCnWideString;
    Lines: TCnWideStringList;
    constructor Create(const AName: TCnWideString);
    destructor Destroy; override;
  end;

  TCnWideIniFile = class(TCustomIniFile)
  private
    FFileName: string;
    FSections: TList;
    FWriteBOM: Boolean;
    function GetSectionIndex(const Section: TCnWideString): Integer;
    function GetSection(const Section: TCnWideString; CreateIfMissing: Boolean): TCnWideIniSection;
    function FindKeyIndex(Lines: TCnWideStringList; const Ident: TCnWideString): Integer;
    procedure ClearSections;
    procedure ParseLines(Lines: TCnWideStringList);
  protected
  public
    constructor Create(const AFileName: string);
    destructor Destroy; override;
    function ReadString(const Section, Ident, Default: string): string; override;
    procedure WriteString(const Section, Ident, Value: String); override;
    procedure ReadSection(const Section: string; Strings: TStrings); override;
    procedure ReadSections(Strings: TStrings); override;
    procedure ReadSectionValues(const Section: string; Strings: TStrings); override;
    procedure EraseSection(const Section: string); override;
    procedure DeleteKey(const Section, Ident: String); override;
    procedure UpdateFile; override;
    function LoadFromFile(const AFileName: string): Boolean;
    function LoadFromStream(AStream: TStream): Boolean;
    function SaveToFile(const AFileName: string): Boolean;
    function SaveToStream(AStream: TStream): Boolean;
    property FileName: string read FFileName;
    property WriteBOM: Boolean read FWriteBOM write FWriteBOM;
  end;

//==============================================================================
// 支持内容加密及流操作的 IniFile 基类
//==============================================================================

{ TCnBaseEncryptIniFile }

  TCnBaseEncryptIniFile = class (TCnStreamIniFile)
  {* 支持内容加密及流操作的 IniFile 抽象基类，允许对 INI 数据进行加密。 }
  private
  {$IFDEF SUPPORT_ZLIB}
    FUseZLib: Boolean;
  {$ENDIF}
  protected
    function CreateEncryptStream(AStream: TStream): TCnEncryptStream; virtual; abstract;
  public
    constructor Create(const AFileName: string
      {$IFDEF SUPPORT_ZLIB}; AUseZLib: Boolean = False{$ENDIF});
    function LoadFromStream(AStream: TStream): Boolean; override;
    {* 从流中装载 INI 数据，流中的数据将自动解密 }
    function SaveToStream(AStream: TStream): Boolean; override;
    {* 保存 INI 数据到流，流中的数据将自动加密 }
  {$IFDEF SUPPORT_ZLIB}
    property UseZLib: Boolean read FUseZLib;
  {$ENDIF}
    {* 是否使用 ZLib 压缩 }
  end;

//==============================================================================
// 支持内容 Xor 加密及流操作的 IniFile 类
//==============================================================================

{ TCnXorIniFile }

  TCnXorIniFile = class (TCnBaseEncryptIniFile)
  {* 支持内容 Xor 加密及流操作的 IniFile 类，允许对 INI 数据进行 Xor 加密。 }
  private
    FXorStr: string;
  protected
    function CreateEncryptStream(AStream: TStream): TCnEncryptStream; override;
  public
    constructor Create(const AFileName: string; const XorStr: string
      {$IFDEF SUPPORT_ZLIB}; AUseZLib: Boolean = False{$ENDIF});
    {* 类构造器。
     |<PRE>
       FileName: string     - INI 文件名，如果文件存在将自动加载
       XorStr: string       - 用于 Xor 操作的字符串
       UseZLib: string      - 是否使用 ZLib 压缩
     |</PRE>}
  end;

//==============================================================================
// 支持内容加密及流操作的 IniFile 类
//==============================================================================

{ TCnEncryptIniFile }

  TCnEncryptIniFile = class (TCnBaseEncryptIniFile)
  {* 支持内容加密及流操作的 IniFile 类，允许对 INI 数据进行基于字符映射表的加密。 }
  private
    FSeedStr: string;
  protected
    function CreateEncryptStream(AStream: TStream): TCnEncryptStream; override;
  public
    constructor Create(const AFileName: string; const SeedStr: string
      {$IFDEF SUPPORT_ZLIB}; AUseZLib: Boolean = False{$ENDIF});
    {* 类构造器。
     |<PRE>
       FileName: string     - INI 文件名，如果文件存在将自动加载
       SeedStr: string      - 用于加密的字符串
       UseZLib: string      - 是否使用 ZLib 压缩
     |</PRE>}
  end;

implementation

uses
  CnCommon;

function IsDefaultPropertyValue(Instance: TObject; PropInfo: PPropInfo): Boolean;
var
  PropType: PTypeInfo;

  function IsDefaultOrdProp: Boolean;
  var
    Value: Longint;
    Default: LongInt;
  begin
    Value := GetOrdProp(Instance, PropInfo);
    Default := PPropInfo(PropInfo)^.Default;
    Result :=  (Default <> LongInt($80000000)) and (Value = Default);
  end;
  
  function IsDefaultFloatProp: Boolean;
  var
    Value: Extended;
  begin
    Value := GetFloatProp(Instance, PropInfo);
    Result := Value = 0;;
  end;

  function IsDefaultInt64Prop: Boolean;
  var
    Value: Int64;
  begin
    Value := GetInt64Prop(Instance, PropInfo);
    Result := Value = 0;
  end;

  function IsDefaultStrProp: Boolean;
  var
    Value: string;
  begin
    Value := GetStrProp(Instance, PropInfo);
    Result := Value = '';
  end;

begin
  Result := False;
  if (PropInfo^.GetProc <> nil) and
     (PropInfo^.SetProc <> nil) then
  begin
{$IFDEF FPC}
    PropType := PropInfo^.PropType;
{$ELSE}
    PropType := PropInfo^.PropType^;
{$ENDIF}
    case PropType^.Kind of
      tkInteger, tkChar, tkEnumeration, tkSet:
        Result := IsDefaultOrdProp;
      tkFloat:
        Result := IsDefaultFloatProp;
      tkString, tkLString, tkWString:
        Result := IsDefaultStrProp;
      tkInt64:
        Result := IsDefaultInt64Prop;
    end;
  end;
end;

//==============================================================================
// 扩展的 INI 访问类
//==============================================================================
   
{ TCnIniFile }

constructor TCnIniFile.Create(AIni: TCustomIniFile; AOwned: Boolean);
begin
  inherited Create('');
  Assert(Assigned(AIni));
  FIni := AIni;
  FOwned := AOwned;
end;

constructor TCnIniFile.Create(const AFileName: string; MemIniFile: Boolean);
begin
  if MemIniFile then
    Create(TMemIniFile.Create(AFileName), True)
  else
    Create(TIniFile.Create(AFileName), True);
end;

destructor TCnIniFile.Destroy;
begin
  if FOwned then
    FreeAndNil(FIni);
  inherited;
end;

//------------------------------------------------------------------------------
// 扩展的 INI 访问方法
//------------------------------------------------------------------------------
   
function TCnIniFile.ReadColor(const Section, Ident: string;
  Default: TColor): TColor;
begin
  try
    Result := StringToColor(ReadString(Section, Ident,
      ColorToString(Default)));
  except
    Result := Default;
  end;
end;

procedure TCnIniFile.WriteColor(const Section, Ident: string; Value: TColor);
begin
  WriteString(Section, Ident, ColorToString(Value));
end;

function TCnIniFile.ReadRect(const Section, Ident: string; const Default: TRect): TRect;
begin
  Result := StrToRect(ReadString(Section, Ident, RectToStr(Default)), Default);
end;

procedure TCnIniFile.WriteRect(const Section, Ident: string; const Value: TRect);
begin
  WriteString(Section, Ident, RectToStr(Value));
end;

function TCnIniFile.ReadPoint(const Section, Ident: string; const Default: TPoint): TPoint;
begin
  Result := StrToPoint(ReadString(Section, Ident, PointToStr(Default)), Default);
end;

procedure TCnIniFile.WritePoint(const Section, Ident: string; const Value: TPoint);
begin
  WriteString(Section, Ident, PointToStr(Value));
end;

function TCnIniFile.ReadFont(const Section, Ident: string; Font: TFont): TFont;
begin
  Result := Font;
  try
    StringToFont(ReadString(Section, Ident, FontToString(Font)), Result);
  except
    { do nothing, ignore any exceptions }
  end;
end;

procedure TCnIniFile.WriteFont(const Section, Ident: string; Font: TFont);
begin
  WriteString(Section, Ident, FontToString(Font));
end;

function TCnIniFile.ReadStrings(const Section, Ident: string;
  Strings: TStrings): TStrings;
begin
  Result := Strings;
  Strings.Text := StrToLines(ReadString(Section, Ident, LinesToStr(Strings.Text)));
end;

function TCnIniFile.ReadStrings(const Section: string; Strings: TStrings): TStrings;
begin
  Result := Strings;
  if SectionExists(Section) then
    ReadStringsFromIni(Self, Section, Result);
end;


function TCnIniFile.ReadStringsBoolean(const Section, Ident: string;
  Strings: TStringList): TStringList;
var
  I, Idx: Integer;
  S, T: string;
begin
  Result := Strings;
  Strings.Text := StrToLines(ReadString(Section, Ident, LinesToStr(Strings.Text)));

  // 解析第一个 | 之前的一位数字看是否不为 0
  for I := 0 to Strings.Count - 1 do
  begin
    Strings.Objects[I] := nil;
    T := Strings[I];
    Idx := Pos('|', T);
    if Idx > 1 then
    begin
      S := Copy(T, 1, Idx - 1);
      Delete(T, 1, Idx);
      Strings[I] := T;

      if StrToInt(S) <> 0 then
        Strings.Objects[I] := TObject(1);
    end;
  end;
end;

procedure TCnIniFile.WriteStrings(const Section, Ident: string; Strings: TStrings);
begin
  WriteString(Section, Ident, LinesToStr(Strings.Text));
end;

procedure TCnIniFile.WriteStringsBoolean(const Section, Ident: string;
  Strings: TStringList);
var
  I: Integer;
  SL: TStringList;
begin
  SL := TStringList.Create;
  try
    for I := 0 to Strings.Count - 1 do
    begin
      if Strings.Objects[I] <> nil then
        SL.Add('1|' + Strings[I])
      else
        SL.Add('0|' + Strings[I]);
    end;
    WriteString(Section, Ident, LinesToStr(SL.Text));
  finally
    SL.Free;
  end;
end;

procedure TCnIniFile.WriteStrings(const Section: string; Strings: TStrings);
begin
  WriteStringsToIni(Self, Section, Strings);
end;

function TCnIniFile.IsColorType(PInfo: PTypeInfo): Boolean;
begin
  Result := PInfo = TypeInfo(TColor);
end;

function TCnIniFile.IsBoolType(PInfo: PTypeInfo): Boolean;
begin
  Result := (PInfo^.Kind = tkEnumeration) and
    (GetTypeData(PInfo)^.MinValue < 0); // Longbool/wordbool/bytebool
end;

function TCnIniFile.IsBooleanType(PInfo: PTypeInfo): Boolean;
begin
{$IFDEF FPC}
  Result := (PInfo.Kind = tkEnumeration) and
    (GetTypeData(PInfo)^.BaseType = TypeInfo(Boolean));
{$ELSE}
  Result := (PInfo.Kind = tkEnumeration) and
    (GetTypeData(PInfo)^.BaseType^ = TypeInfo(Boolean));
{$ENDIF}
end;

function TCnIniFile.IsDateTimeType(PInfo: PTypeInfo): Boolean;
begin
  Result := PInfo = TypeInfo(TDateTime);
end;

procedure TCnIniFile.ReadObject(const Section: string; AObject: TObject);
var
  S: string;
  WS: WideString;
  Count: Integer;
  PropIdx: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
  TpInfo: PTypeInfo;
  Obj: TObject;
begin
  Count := GetPropList(AObject.ClassInfo, tkProperties - [tkArray, tkRecord,
    tkInterface], nil);
  GetMem(PropList, Count * SizeOf(Pointer));
  try
    GetPropList(AObject.ClassInfo, tkProperties - [tkArray, tkDynArray, tkRecord,
      tkVariant, tkMethod, tkInterface], @PropList^[0]);
    for PropIdx := 0 to Count - 1 do
    begin
      PropInfo := PropList^[PropIdx];
      try
        if ValueExists(Section, PropInfoName(PropInfo)) then
        begin
{$IFDEF FPC}
          TpInfo := PropInfo^.PropType;
{$ELSE}
          TpInfo := PropInfo^.PropType^;
{$ENDIF}
          if IsColorType(TpInfo) then
            SetOrdProp(AObject, PropInfo, ReadColor(Section, PropInfoName(PropInfo),
              GetOrdProp(AObject, PropInfo)))
          else if IsBooleanType(TpInfo) then
          begin
            if ReadBool(Section, PropInfoName(PropInfo), GetOrdProp(AObject, PropInfo) <> 0) then
              SetEnumProp(AObject, PropInfo, BoolToStr(True, True))
            else
              SetEnumProp(AObject, PropInfo, BoolToStr(False, True));
          end
          else if IsBoolType(TpInfo) then
          begin
            if ReadBool(Section, PropInfoName(PropInfo), GetOrdProp(AObject, PropInfo) <> 0) then
              SetOrdProp(AObject, PropInfo, -1)
            else
              SetOrdProp(AObject, PropInfo, 0);
          end
          else if IsDateTimeType(TpInfo) then
            SetFloatProp(AObject, PropInfo, ReadDateTime(Section, PropInfoName(PropInfo),
              GetFloatProp(AObject, PropInfo)))
          else
          begin
            case TpInfo^.Kind of
              tkInteger:
                SetOrdProp(AObject, PropInfo, ReadInteger(Section, PropInfoName(PropInfo),
                  GetOrdProp(AObject, PropInfo)));
              tkChar:
                begin
                  S := ReadString(Section, PropInfoName(PropInfo), Char(GetOrdProp(AObject, PropInfo)));
                  if S <> '' then
                    SetOrdProp(AObject, PropInfo, Ord(S[1]));
                end;
              tkWChar:
                begin
                  WS := ReadString(Section, PropInfoName(PropInfo), WideChar(GetOrdProp(AObject, PropInfo)));
                  if WS <> '' then
                    SetOrdProp(AObject, PropInfo, Ord(WS[1]));
                end;
              tkString, tkLString, tkWString{$IFDEF UNICODE}, tkUString{$ENDIF}:
                SetStrProp(AObject, PropInfo, ReadString(Section, PropInfoName(PropInfo),
                  GetStrProp(AObject, PropInfo)));
              tkFloat:
                SetFloatProp(AObject, PropInfo, ReadFloat(Section, PropInfoName(PropInfo),
                  GetFloatProp(AObject, PropInfo)));
              tkInt64:
                SetInt64Prop(AObject, PropInfo, StrToInt64(ReadString(Section,
                  PropInfoName(PropInfo), IntToStr(GetInt64Prop(AObject, PropInfo)))));
              tkEnumeration:
                SetEnumProp(AObject, PropInfo, ReadString(Section, PropInfoName(PropInfo),
                  GetEnumProp(AObject, PropInfo)));
              tkSet:
                SetSetProp(AObject, PropInfo, ReadString(Section, PropInfoName(PropInfo),
                  GetSetProp(AObject, PropInfo, True)));
              tkClass:
                begin
                  Obj := TObject(GetOrdProp(AObject, PropInfo));
                  if Obj <> nil then
                  begin
                    if Obj is TFont then
                      ReadFont(Section, PropInfoName(PropInfo), TFont(Obj))
                    else if Obj is TStrings then
                      ReadStrings(Section, PropInfoName(PropInfo), TStrings(Obj));
                  end;
                end;
            end;
          end;            
        end;
      except
        ;
      end;
    end;
  finally
    FreeMem(PropList);
  end;
end;

procedure TCnIniFile.WriteObject(const Section: string; AObject: TObject;
  NoDef: Boolean);
var
  Count: Integer;
  PropIdx: Integer;
  PropList: PPropList;
  PropInfo: PPropInfo;
  TpInfo: PTypeInfo;
  Obj: TObject;
begin
  Count := GetPropList(AObject.ClassInfo, tkProperties - [tkArray, tkRecord,
    tkInterface], nil);
  GetMem(PropList, Count * SizeOf(Pointer));
  try
    GetPropList(AObject.ClassInfo, tkProperties - [tkArray, tkDynArray, tkRecord,
      tkVariant, tkMethod, tkInterface], @PropList^[0]);
    for PropIdx := 0 to Count - 1 do
    begin
      PropInfo := PropList^[PropIdx];
      try
        if not NoDef or IsStoredProp(AObject, PropInfo) and
          not IsDefaultPropertyValue(AObject, PropInfo) then
        begin

{$IFDEF FPC}
          TpInfo := PropInfo^.PropType;
{$ELSE}
          TpInfo := PropInfo^.PropType^;
{$ENDIF}
          if IsColorType(TpInfo) then
            WriteColor(Section, PropInfoName(PropInfo), GetOrdProp(AObject, PropInfo))
          else if IsBooleanType(TpInfo) or IsBoolType(TpInfo) then
            WriteBool(Section, PropInfoName(PropInfo), GetOrdProp(AObject, PropInfo) <> 0)
          else if IsDateTimeType(TpInfo) then
            WriteDateTime(Section, PropInfoName(PropInfo), GetFloatProp(AObject, PropInfo))
          else
          begin
            case TpInfo^.Kind of
              tkInteger:
                WriteInteger(Section, PropInfoName(PropInfo), GetOrdProp(AObject, PropInfo));
              tkChar:
                WriteString(Section, PropInfoName(PropInfo), Char(GetOrdProp(AObject, PropInfo)));
              tkWChar:
                WriteString(Section, PropInfoName(PropInfo), WideChar(GetOrdProp(AObject, PropInfo)));
              tkString, tkLString, tkWString{$IFDEF UNICODE}, tkUString{$ENDIF}:
                WriteString(Section, PropInfoName(PropInfo), GetStrProp(AObject, PropInfo));
              tkFloat:
                WriteFloat(Section, PropInfoName(PropInfo), GetFloatProp(AObject, PropInfo));
              tkInt64:
                WriteString(Section, PropInfoName(PropInfo), IntToStr(GetInt64Prop(AObject, PropInfo)));
              tkEnumeration:
                WriteString(Section, PropInfoName(PropInfo), GetEnumProp(AObject, PropInfo));
              tkSet:
                WriteString(Section, PropInfoName(PropInfo), GetSetProp(AObject, PropInfo, True));
              tkClass:
                begin
                  Obj := TObject(GetOrdProp(AObject, PropInfo));
                  if Obj <> nil then
                  begin
                    if Obj is TFont then
                      WriteFont(Section, PropInfoName(PropInfo), TFont(Obj))
                    else if Obj is TStrings then
                      WriteStrings(Section, PropInfoName(PropInfo), TStrings(Obj));
                  end;
                end;
            end;
          end;            
        end
        else
        begin
          DeleteKey(Section, PropInfoName(PropInfo));
        end;
      except
        ;
      end;                      
    end;
  finally
    FreeMem(PropList);
  end;
end;

//------------------------------------------------------------------------------
// 调用被包装的 INI 访问方法
//------------------------------------------------------------------------------

procedure TCnIniFile.DeleteKey(const Section, Ident: String);
begin
  Ini.DeleteKey(Section, Ident);
end;

procedure TCnIniFile.EraseSection(const Section: string);
begin
  Ini.EraseSection(Section);
end;

function TCnIniFile.GetFileName: string;
begin
  Result := Ini.FileName;
end;

procedure TCnIniFile.ReadSection(const Section: string; Strings: TStrings);
begin
  Ini.ReadSection(Section, Strings);
end;

procedure TCnIniFile.ReadSections(Strings: TStrings);
begin
  Ini.ReadSections(Strings);
end;

procedure TCnIniFile.ReadSectionValues(const Section: string;
  Strings: TStrings);
begin
  Ini.ReadSectionValues(Section, Strings);
end;

function TCnIniFile.ReadString(const Section, Ident,
  Default: string): string;
begin
  Result := Ini.ReadString(Section, Ident, Default);
end;

procedure TCnIniFile.UpdateFile;
begin
  Ini.UpdateFile;
end;

procedure TCnIniFile.WriteString(const Section, Ident, Value: String);
begin
  Ini.WriteString(Section, Ident, Value);
end;

function TCnIniFile.ReadBool(const Section, Ident: string;
  Default: Boolean): Boolean;
begin
  Result := Ini.ReadBool(Section, Ident, Default);
end;

function TCnIniFile.ReadDate(const Section, Name: string;
  Default: TDateTime): TDateTime;
begin
  Result := Ini.ReadDate(Section, Name, Default);
end;

function TCnIniFile.ReadDateTime(const Section, Name: string;
  Default: TDateTime): TDateTime;
begin
  Result := Ini.ReadDateTime(Section, Name, Default);
end;

function TCnIniFile.ReadFloat(const Section, Name: string;
  Default: Double): Double;
begin
  Result := Ini.ReadFloat(Section, Name, Default);
end;

{$IFDEF INIFILE_READWRITE_INTEGER}

function TCnIniFile.ReadInteger(const Section, Ident: string;
  Default: Integer): Integer;
begin
  Result := Ini.ReadInteger(Section, Ident, Default);
end;

{$ELSE}

function TCnIniFile.ReadInteger(const Section, Ident: string;
  Default: LongInt): LongInt;
begin
  Result := Ini.ReadInteger(Section, Ident, Default);
end;

{$ENDIF}

function TCnIniFile.ReadTime(const Section, Name: string;
  Default: TDateTime): TDateTime;
begin
  Result := Ini.ReadTime(Section, Name, Default);
end;

procedure TCnIniFile.WriteBool(const Section, Ident: string;
  Value: Boolean);
begin
  Ini.WriteBool(Section, Ident, Value);
end;

procedure TCnIniFile.WriteDate(const Section, Name: string;
  Value: TDateTime);
begin
  Ini.WriteDate(Section, Name, Value);
end;

procedure TCnIniFile.WriteDateTime(const Section, Name: string;
  Value: TDateTime);
begin
  Ini.WriteDateTime(Section, Name, Value);
end;

procedure TCnIniFile.WriteFloat(const Section, Name: string;
  Value: Double);
begin
  Ini.WriteFloat(Section, Name, Value);
end;

{$IFDEF INIFILE_READWRITE_INTEGER}

procedure TCnIniFile.WriteInteger(const Section, Ident: string;
  Value: Integer);
begin
  Ini.WriteInteger(Section, Ident, Value);
end;

{$ELSE}

procedure TCnIniFile.WriteInteger(const Section, Ident: string;
  Value: LongInt);
begin
  Ini.WriteInteger(Section, Ident, Value);
end;

{$ENDIF}

procedure TCnIniFile.WriteTime(const Section, Name: string;
  Value: TDateTime);
begin
  Ini.WriteTime(Section, Name, Value);
end;

//==============================================================================
// 支持流操作的 IniFile 类
//==============================================================================

{ TCnStreamIniFile }

constructor TCnStreamIniFile.Create(const AFileName: string);
var
  Strings: TStrings;
begin
  inherited Create('');
  FFileName := AFileName;
  if FileExists(FFileName) then
    LoadFromFile(FFileName);

  if FFileName <> '' then
  begin
    Strings := TStringList.Create;
    try
      GetStrings(Strings);
      FInitData := Strings.Text;
    finally
      Strings.Free;
    end;
  end;    
end;

destructor TCnStreamIniFile.Destroy;
var
  Strings: TStrings;
begin
  if FFileName <> '' then
  begin
    // 有变更时才保存
    Strings := TStringList.Create;
    try
      GetStrings(Strings);
      if CompareStr(Strings.Text, FInitData) <> 0 then
        UpdateFile;
    finally
      Strings.Free;
    end;
  end;
  inherited;
end;

function TCnStreamIniFile.LoadFromFile(const AFileName: string): Boolean;
var
  Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      Result := LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;

function TCnStreamIniFile.LoadFromStream(AStream: TStream): Boolean;
var
  Strings: TStrings;
begin
  try
    Strings := TStringList.Create;
    try
      Strings.LoadFromStream(AStream);
      SetStrings(Strings);
    finally
      Strings.Free;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

function TCnStreamIniFile.SaveToFile(const AFileName: string): Boolean;
var
  Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(AFileName, fmCreate);
    try
      Stream.Size := 0;
      Result := SaveToStream(Stream);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;

function TCnStreamIniFile.SaveToStream(AStream: TStream): Boolean;
var
  Strings: TStrings;
begin
  try
    Strings := TStringList.Create;
    try
      GetStrings(Strings);
      Strings.SaveToStream(AStream);
    finally
      Strings.Free;
    end;
    Result := True;
  except
    Result := False;
  end;
end;

procedure TCnStreamIniFile.UpdateFile;
begin
  if FFileName <> '' then
    SaveToFile(FFileName);
end;

constructor TCnWideIniSection.Create(const AName: TCnWideString);
begin
  inherited Create;
  Name := AName;
  Lines := TCnWideStringList.Create;
end;

function CnWideIsSpace(C: WideChar): Boolean;
begin
  Result := Ord(C) <= Ord(' ');
end;

function CnWideTrimLeft(const S: TCnWideString): TCnWideString;
var
  I, L: Integer;
begin
  L := Length(S);
  I := 1;
  while (I <= L) and CnWideIsSpace(S[I]) do
    Inc(I);
  Result := Copy(S, I, L - I + 1);
end;

function CnWideTrimRight(const S: TCnWideString): TCnWideString;
var
  I: Integer;
begin
  I := Length(S);
  while (I > 0) and CnWideIsSpace(S[I]) do
    Dec(I);
  Result := Copy(S, 1, I);
end;

function CnWideTrim(const S: TCnWideString): TCnWideString;
begin
  Result := CnWideTrimRight(CnWideTrimLeft(S));
end;

function CnWidePosChar(C: WideChar; const S: TCnWideString): Integer;
var
  I, L: Integer;
begin
  Result := 0;
  L := Length(S);
  for I := 1 to L do
  begin
    if S[I] = C then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

destructor TCnWideIniSection.Destroy;
begin
  Lines.Free;
  inherited Destroy;
end;

constructor TCnWideIniFile.Create(const AFileName: string);
begin
  inherited Create(AFileName);
  FFileName := AFileName;
  FSections := TList.Create;
  FWriteBOM := True;
  if (FFileName <> '') and FileExists(FFileName) then
    LoadFromFile(FFileName);
end;

destructor TCnWideIniFile.Destroy;
begin
  ClearSections;
  FSections.Free;
  inherited Destroy;
end;

procedure TCnWideIniFile.ClearSections;
var
  I: Integer;
begin
  for I := FSections.Count - 1 downto 0 do
    TObject(FSections[I]).Free;
  FSections.Clear;
end;

function TCnWideIniFile.GetSectionIndex(const Section: TCnWideString): Integer;
var
  I: Integer;
  S: TCnWideIniSection;
begin
  Result := -1;
  for I := 0 to FSections.Count - 1 do
  begin
    S := TCnWideIniSection(FSections[I]);
    if WideCompareText(S.Name, Section) = 0 then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

function TCnWideIniFile.GetSection(const Section: TCnWideString;
  CreateIfMissing: Boolean): TCnWideIniSection;
var
  Index: Integer;
begin
  Index := GetSectionIndex(Section);
  if Index >= 0 then
    Result := TCnWideIniSection(FSections[Index])
  else if CreateIfMissing then
  begin
    Result := TCnWideIniSection.Create(Section);
    FSections.Add(Result);
  end
  else
    Result := nil;
end;

function TCnWideIniFile.FindKeyIndex(Lines: TCnWideStringList;
  const Ident: TCnWideString): Integer;
var
  I, P: Integer;
  Key: TCnWideString;
begin
  Result := -1;
  for I := 0 to Lines.Count - 1 do
  begin
    P := CnWidePosChar('=', Lines[I]);
    if P > 0 then
      Key := Copy(Lines[I], 1, P - 1)
    else
      Key := Lines[I];
    Key := CnWideTrim(Key);
    if WideCompareText(Key, Ident) = 0 then
    begin
      Result := I;
      Exit;
    end;
  end;
end;

procedure TCnWideIniFile.ParseLines(Lines: TCnWideStringList);
var
  I, P: Integer;
  Line, Trimmed, SecName, Key, Value: TCnWideString;
  Sec: TCnWideIniSection;
begin
  ClearSections;
  Sec := GetSection('', True);
  for I := 0 to Lines.Count - 1 do
  begin
    Line := Lines[I];
    Trimmed := CnWideTrim(Line);
    if Trimmed = '' then
      Continue;
    if (Trimmed[1] = ';') or (Trimmed[1] = '#') then
      Continue;
    if (Trimmed[1] = '[') and (Trimmed[Length(Trimmed)] = ']') then
    begin
      SecName := Copy(Trimmed, 2, Length(Trimmed) - 2);
      Sec := GetSection(SecName, True);
      Continue;
    end;
    P := CnWidePosChar('=', Line);
    if P > 0 then
    begin
      Key := CnWideTrim(Copy(Line, 1, P - 1));
      Value := CnWideTrim(Copy(Line, P + 1, MaxInt));
      Sec.Lines.Add(Key + '=' + Value);
    end
    else
    begin
      Key := CnWideTrim(Line);
      Sec.Lines.Add(Key);
    end;
  end;
end;

function TCnWideIniFile.LoadFromFile(const AFileName: string): Boolean;
var
  Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
    try
      Result := LoadFromStream(Stream);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;

function TCnWideIniFile.LoadFromStream(AStream: TStream): Boolean;
var
  Lines: TCnWideStringList;
  Data: AnsiString;
  Size, Len: Integer;
  WideText: TCnWideString;
  P: PAnsiChar;
begin
  Lines := TCnWideStringList.Create;
  try
    try
      Size := AStream.Size - AStream.Position;
      SetLength(Data, Size);
      if Size > 0 then
        AStream.ReadBuffer(Pointer(Data)^, Size);
      if (Size >= 3) and (Data[1] = #$EF) and (Data[2] = #$BB) and (Data[3] = #$BF) then
      begin
        WideText := CnUtf8DecodeToWideString(Copy(Data, 4, MaxInt));
      end
      else if (Size >= 2) and (Data[1] = #$FF) and (Data[2] = #$FE) then
      begin
        Len := (Size - 2) div SizeOf(WideChar);
        SetLength(WideText, Len);
        if Len > 0 then
        begin
          P := PAnsiChar(Data);
          Move(P[2], PWideChar(WideText)^, Len * SizeOf(WideChar));
        end;
      end
      else
        WideText := WideString(Data);
      Lines.Text := WideText;
      ParseLines(Lines);
      Result := True;
    except
      Result := False;
    end;
  finally
    Lines.Free;
  end;
end;

function TCnWideIniFile.SaveToFile(const AFileName: string): Boolean;
var
  Stream: TFileStream;
begin
  try
    Stream := TFileStream.Create(AFileName, fmCreate);
    try
      Stream.Size := 0;
      Result := SaveToStream(Stream);
    finally
      Stream.Free;
    end;
  except
    Result := False;
  end;
end;

function TCnWideIniFile.SaveToStream(AStream: TStream): Boolean;
var
  Lines: TCnWideStringList;
  I, J: Integer;
  Sec: TCnWideIniSection;
  WideText: TCnWideString;
  Data: AnsiString;
  Bom: AnsiString;
begin
  Lines := TCnWideStringList.Create;
  try
    try
      for I := 0 to FSections.Count - 1 do
      begin
        Sec := TCnWideIniSection(FSections[I]);
        if Sec.Name <> '' then
          Lines.Add('[' + Sec.Name + ']');
        for J := 0 to Sec.Lines.Count - 1 do
          Lines.Add(Sec.Lines[J]);
        if (I < FSections.Count - 1) and (Sec.Lines.Count > 0) then
          Lines.Add('');
      end;
      WideText := Lines.Text;
      Data := CnUtf8EncodeWideString(WideText);
      if FWriteBOM then
      begin
        Bom := #$EF#$BB#$BF;
        if Length(Bom) > 0 then
          AStream.WriteBuffer(Pointer(Bom)^, Length(Bom));
      end;
      if Length(Data) > 0 then
        AStream.WriteBuffer(Pointer(Data)^, Length(Data));
      Result := True;
    except
      Result := False;
    end;
  finally
    Lines.Free;
  end;
end;

procedure TCnWideIniFile.UpdateFile;
begin
  if FFileName <> '' then
    SaveToFile(FFileName);
end;

function TCnWideIniFile.ReadString(const Section, Ident, Default: string): string;
var
  Sec: TCnWideIniSection;
  Idx, P: Integer;
  Line: TCnWideString;
  Value: TCnWideString;
  SecName: TCnWideString;
  KeyName: TCnWideString;
begin
  SecName := {$IFDEF UNICODE}Section{$ELSE}CnUtf8DecodeToWideString(AnsiString(Section)){$ENDIF};
  KeyName := {$IFDEF UNICODE}Ident{$ELSE}CnUtf8DecodeToWideString(AnsiString(Ident)){$ENDIF};
  Sec := GetSection(SecName, False);
  if Sec = nil then
  begin
    Result := Default;
    Exit;
  end;
  Idx := FindKeyIndex(Sec.Lines, KeyName);
  if Idx < 0 then
  begin
    Result := Default;
    Exit;
  end;
  Line := Sec.Lines[Idx];
  P := CnWidePosChar('=', Line);
  if P > 0 then
    Value := Copy(Line, P + 1, MaxInt)
  else
    Value := '';
{$IFDEF UNICODE}
  Result := Value;
{$ELSE}
  Result := string(CnUtf8EncodeWideString(Value));
{$ENDIF}
end;

procedure TCnWideIniFile.WriteString(const Section, Ident, Value: String);
var
  Sec: TCnWideIniSection;
  Idx: Integer;
  SecName: TCnWideString;
  KeyName: TCnWideString;
  Val: TCnWideString;
begin
  SecName := {$IFDEF UNICODE}Section{$ELSE}CnUtf8DecodeToWideString(AnsiString(Section)){$ENDIF};
  KeyName := {$IFDEF UNICODE}Ident{$ELSE}CnUtf8DecodeToWideString(AnsiString(Ident)){$ENDIF};
  Val := {$IFDEF UNICODE}Value{$ELSE}CnUtf8DecodeToWideString(AnsiString(Value)){$ENDIF};
  Sec := GetSection(SecName, True);
  Idx := FindKeyIndex(Sec.Lines, KeyName);
  if Idx >= 0 then
    Sec.Lines[Idx] := KeyName + '=' + Val
  else
    Sec.Lines.Add(KeyName + '=' + Val);
end;

procedure TCnWideIniFile.ReadSection(const Section: string; Strings: TStrings);
var
  Sec: TCnWideIniSection;
  I, P: Integer;
  Line, Key: TCnWideString;
  SecName: TCnWideString;
begin
  if Strings = nil then
    Exit;
  Strings.Clear;
  SecName := {$IFDEF UNICODE}Section{$ELSE}CnUtf8DecodeToWideString(AnsiString(Section)){$ENDIF};
  Sec := GetSection(SecName, False);
  if Sec = nil then
    Exit;
  for I := 0 to Sec.Lines.Count - 1 do
  begin
    Line := Sec.Lines[I];
    P := CnWidePosChar('=', Line);
    if P > 0 then
      Key := Copy(Line, 1, P - 1)
    else
      Key := Line;
{$IFDEF UNICODE}
    Strings.Add(Key);
{$ELSE}
    Strings.Add(string(CnUtf8EncodeWideString(Key)));
{$ENDIF}
  end;
end;

procedure TCnWideIniFile.ReadSections(Strings: TStrings);
var
  I: Integer;
  Sec: TCnWideIniSection;
begin
  if Strings = nil then
    Exit;
  Strings.Clear;
  for I := 0 to FSections.Count - 1 do
  begin
    Sec := TCnWideIniSection(FSections[I]);
    if Sec.Name <> '' then
    begin
{$IFDEF UNICODE}
      Strings.Add(Sec.Name);
{$ELSE}
      Strings.Add(string(CnUtf8EncodeWideString(Sec.Name)));
{$ENDIF}
    end;
  end;
end;

procedure TCnWideIniFile.ReadSectionValues(const Section: string; Strings: TStrings);
var
  Sec: TCnWideIniSection;
  I: Integer;
  Line: TCnWideString;
  SecName: TCnWideString;
begin
  if Strings = nil then
    Exit;
  Strings.Clear;
  SecName := {$IFDEF UNICODE}Section{$ELSE}CnUtf8DecodeToWideString(AnsiString(Section)){$ENDIF};
  Sec := GetSection(SecName, False);
  if Sec = nil then
    Exit;
  for I := 0 to Sec.Lines.Count - 1 do
  begin
    Line := Sec.Lines[I];
{$IFDEF UNICODE}
    Strings.Add(Line);
{$ELSE}
    Strings.Add(string(CnUtf8EncodeWideString(Line)));
{$ENDIF}
  end;
end;

procedure TCnWideIniFile.EraseSection(const Section: string);
var
  Index: Integer;
  SecName: TCnWideString;
begin
  SecName := {$IFDEF UNICODE}Section{$ELSE}CnUtf8DecodeToWideString(AnsiString(Section)){$ENDIF};
  Index := GetSectionIndex(SecName);
  if Index >= 0 then
  begin
    TObject(FSections[Index]).Free;
    FSections.Delete(Index);
  end;
end;

procedure TCnWideIniFile.DeleteKey(const Section, Ident: String);
var
  Sec: TCnWideIniSection;
  Idx: Integer;
  SecName: TCnWideString;
  KeyName: TCnWideString;
begin
  SecName := {$IFDEF UNICODE}Section{$ELSE}CnUtf8DecodeToWideString(AnsiString(Section)){$ENDIF};
  KeyName := {$IFDEF UNICODE}Ident{$ELSE}CnUtf8DecodeToWideString(AnsiString(Ident)){$ENDIF};
  Sec := GetSection(SecName, False);
  if Sec = nil then
    Exit;
  Idx := FindKeyIndex(Sec.Lines, KeyName);
  if Idx >= 0 then
    Sec.Lines.Delete(Idx);
end;

//==============================================================================
// 支持内容加密及流操作的 IniFile 基类
//==============================================================================

{ TCnBaseEncryptIniFile }

constructor TCnBaseEncryptIniFile.Create(const AFileName: string
  {$IFDEF SUPPORT_ZLIB}; AUseZLib: Boolean = False{$ENDIF});
begin
{$IFDEF SUPPORT_ZLIB}
  FUseZLib := AUseZLib;
{$ENDIF}
  inherited Create(AFileName);
end;

function TCnBaseEncryptIniFile.LoadFromStream(AStream: TStream): Boolean;
var
  EncryptStream: TCnEncryptStream;
{$IFDEF SUPPORT_ZLIB}
  DecompStream: TDecompressionStream;
  MemStream: TMemoryStream;
{$ENDIF}
begin
  EncryptStream := nil;
{$IFDEF SUPPORT_ZLIB}
  DecompStream := nil;
  MemStream := nil;
{$ENDIF}
  try
  {$IFDEF SUPPORT_ZLIB}
    if FUseZLib then
    begin
      EncryptStream := CreateEncryptStream(AStream);
      MemStream := TMemoryStream.Create;
      MemStream.LoadFromStream(EncryptStream);
      DecompStream := TDecompressionStream.Create(MemStream);
      Result := inherited LoadFromStream(DecompStream);
    end
    else
  {$ENDIF}
    begin
      EncryptStream := CreateEncryptStream(AStream);
      Result := inherited LoadFromStream(EncryptStream);
    end;
  finally
    EncryptStream.Free;
  {$IFDEF SUPPORT_ZLIB}
    DecompStream.Free;
    MemStream.Free;
  {$ENDIF}
  end;
end;

function TCnBaseEncryptIniFile.SaveToStream(AStream: TStream): Boolean;
var
  EncryptStream: TCnEncryptStream;
{$IFDEF SUPPORT_ZLIB}
  MemStream: TMemoryStream;
  CompStream: TCompressionStream;
{$ENDIF}
begin
  EncryptStream := nil;
{$IFDEF SUPPORT_ZLIB}
  CompStream := nil;
  MemStream := nil;
{$ENDIF}
  try
  {$IFDEF SUPPORT_ZLIB}
    if FUseZLib then
    begin
      MemStream := TMemoryStream.Create;
    {$IFNDEF DELPHI2009_UP}
      CompStream := TCompressionStream.Create(clMax, MemStream);
    {$ELSE}
      {$IFDEF DELPHIXE2_UP}
      CompStream := TCompressionStream.Create(MemStream, zcMax, 15);
      {$ELSE}
      CompStream := TCompressionStream.Create(MemStream, zcMax);
      {$ENDIF}
    {$ENDIF}
      Result := inherited SaveToStream(CompStream);
      FreeAndNil(CompStream); // 释放时才会完成压缩输出
      EncryptStream := CreateEncryptStream(AStream);
      MemStream.SaveToStream(EncryptStream);
    end
    else
  {$ENDIF}
    begin
      EncryptStream := CreateEncryptStream(AStream);
      Result := inherited SaveToStream(EncryptStream);
    end;
  finally
    EncryptStream.Free;
  {$IFDEF SUPPORT_ZLIB}
    MemStream.Free;
    CompStream.Free;
  {$ENDIF}
  end;
end;

//==============================================================================
// 支持文本 Xor 加密及流操作的 IniFile 类
//==============================================================================

{ TCnXorIniFile }

constructor TCnXorIniFile.Create(const AFileName, XorStr: string
  {$IFDEF SUPPORT_ZLIB}; AUseZLib: Boolean{$ENDIF});
begin
  FXorStr := XorStr;
  inherited Create(AFileName{$IFDEF SUPPORT_ZLIB}, AUseZLib{$ENDIF});
end;

function TCnXorIniFile.CreateEncryptStream(AStream: TStream): TCnEncryptStream;
begin
  Result := TCnXorStream.Create(AStream, AnsiString(FXorStr));
end;

//==============================================================================
// 支持内容加密及流操作的 IniFile 类
//==============================================================================

{ TCnEncryptIniFile }

constructor TCnEncryptIniFile.Create(const AFileName: string; const SeedStr: string
  {$IFDEF SUPPORT_ZLIB}; AUseZLib: Boolean = False{$ENDIF});
begin
  FSeedStr := SeedStr;
  inherited Create(AFileName{$IFDEF SUPPORT_ZLIB}, AUseZLib{$ENDIF});
end;

function TCnEncryptIniFile.CreateEncryptStream(AStream: TStream): TCnEncryptStream;
begin
  Result := TCnCodeMapStream.Create(AStream, AnsiString(FSeedStr));
end;

end.
