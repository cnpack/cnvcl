{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2022 CnPack 开发组                       }
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
* 兼容测试：PWin9X/2000/XP + Delphi 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2002.10.20 V1.0
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
  Classes, SysUtils, TypInfo, IniFiles, Graphics,
  CnIniStrUtils, CnStream {$IFDEF SUPPORT_ZLIB}, ZLib{$ENDIF};

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
    {* 读取字体}
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
    {* 从一行文本中读取字符串列表}
    function ReadStrings(const Section: string; Strings: TStrings): TStrings; overload;
    {* 从单独的节中读取字符串列表}
    procedure WriteStrings(const Section, Ident: string; Strings: TStrings); overload;
    {* 写入字符串列表到一行文本中}
    procedure WriteStrings(const Section: string; Strings: TStrings); overload;
    {* 写入字符串列表到单独的节中}
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

procedure TCnIniFile.WriteStrings(const Section, Ident: string; Strings: TStrings);
begin
  WriteString(Section, Ident, LinesToStr(Strings.Text));
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
  Result := (PInfo.Kind = tkEnumeration) and
    (GetTypeData(PInfo)^.BaseType^ = TypeInfo(Boolean));
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
          if IsColorType(PropInfo^.PropType^) then
            SetOrdProp(AObject, PropInfo, ReadColor(Section, PropInfoName(PropInfo),
              GetOrdProp(AObject, PropInfo)))
          else if IsBooleanType(PropInfo^.PropType^) then
          begin
            if ReadBool(Section, PropInfoName(PropInfo), GetOrdProp(AObject, PropInfo) <> 0) then
              SetEnumProp(AObject, PropInfo, BoolToStr(True, True))
            else
              SetEnumProp(AObject, PropInfo, BoolToStr(False, True));
          end
          else if IsBoolType(PropInfo^.PropType^) then
          begin
            if ReadBool(Section, PropInfoName(PropInfo), GetOrdProp(AObject, PropInfo) <> 0) then
              SetOrdProp(AObject, PropInfo, -1)
            else
              SetOrdProp(AObject, PropInfo, 0);
          end
          else if IsDateTimeType(PropInfo^.PropType^) then
            SetFloatProp(AObject, PropInfo, ReadDateTime(Section, PropInfoName(PropInfo),
              GetFloatProp(AObject, PropInfo)))
          else
          begin
            case PropInfo^.PropType^^.Kind of
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
          if IsColorType(PropInfo^.PropType^) then
            WriteColor(Section, PropInfoName(PropInfo), GetOrdProp(AObject, PropInfo))
          else if IsBooleanType(PropInfo^.PropType^) or IsBoolType(PropInfo^.PropType^) then
            WriteBool(Section, PropInfoName(PropInfo), GetOrdProp(AObject, PropInfo) <> 0)
          else if IsDateTimeType(PropInfo^.PropType^) then
            WriteDateTime(Section, PropInfoName(PropInfo), GetFloatProp(AObject, PropInfo))
          else
          begin
            case PropInfo^.PropType^^.Kind of
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
