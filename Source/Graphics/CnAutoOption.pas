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

unit CnAutoOption;
{* |<PRE>
================================================================================
* 软件名称：不可视工具组件包
* 单元名称：自动参数设置类定义单元
* 单元作者：周劲羽 (zjy@cnpack.org)
* 开发平台：PWin2000 SP4 + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 备    注：该单元定义了自动参数设置信息类
*           用于在运行时使用树状结构根据设置信息对象显示通用的设置界面。
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Contnrs,
  {$IFDEF COMPILER6_UP} Variants, {$ENDIF COMPILER6_UP}
  TypInfo, Forms;

type

{ Exception }

  EInvalidPropItem = Exception;
  {* 无效的属性信息项异常 }

{ TCnBaseOption }

  TCnOptionGroup = class;

  TCnOptionClass = class of TCnBaseOption;
  {* 设置信息类类型 }

  TCnBaseOption = class(TPersistent)
  {* 设置信息基础类 }
  private
    FIsStored: Boolean;
    FOwner: TCnOptionGroup;
    FText: string;
    FVisible: Boolean;
  public
    constructor Create(AOwner: TCnOptionGroup); virtual;
    {* 类构造器，参数为所有者 }
    procedure Assign(Source: TPersistent); override;
    {* 对象赋值方法，使用 RTTI 来自动对发布属性赋值，子类只需要处理非发布属性
       即可，子类如果 override 该方法，需要 inherited }
    property Owner: TCnOptionGroup read FOwner;
    {* 设置项的所有者 }
  published
    property IsStored: Boolean read FIsStored write FIsStored;
    {* 标识该项设置是否保存到设置文件中 }
    property Text: string read FText write FText;
    {* 设置项显示的标题 }
  end;

{ TCnOptionCustom }

  TCnOptionCustom = class(TCnBaseOption)
  {* 使用一个按钮进行设置的参数项类 }
  private
    FOnClick: TNotifyEvent;
    FCaption: string;
  published
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    {* 设置按钮点击事件 }
    property Caption: string read FCaption write FCaption;
    {* 设置按钮标题 }
  end;

{ TCnOptionItem }

  TCnOptionItem = class(TCnBaseOption)
  {* 属性设置项类，该类实例与一个运行期需要配置的对象属性关联 }
  private
    FDefaultValue: Variant;
    FInstance: TObject;
    FList: TStrings;
    FMaxValue: Variant;
    FMinValue: Variant;
    FPropName: string;
    FValue: Variant;
    function GetDefaultValue: Variant;
    procedure SetList(Value: TStrings);
    function GetPropInfo: PPropInfo;
    function GetPropKind: TTypeKind;
    function GetPropValue: Variant;
    procedure SetPropValue(const Value: Variant);
    function GetMaxValue: Variant;
    function GetMinValue: Variant;
    procedure SetDefaultValue(const Value: Variant);
    procedure SetMaxValue(const Value: Variant);
    procedure SetMinValue(const Value: Variant);
  protected
    procedure ClearValue;
  public
    constructor Create(AOwner: TCnOptionGroup); override;
    {* 类构造器，参数为所有者 }
    destructor Destroy; override;
    {* 类析构器 }
    procedure ApplyOption;
    {* 应用当前的设置到对象属性 }
    procedure DefaultOption;
    {* 恢复设置为原对象属性的默认值 }
    procedure ResetOption;
    {* 恢复设置为原对象属性的当前值 }
    property PropInfo: PPropInfo read GetPropInfo;
    {* 关联对象的属性类型信息，如果属性不正确，会弹出 EInvalidPropItem 异常 }
    property PropKind: TTypeKind read GetPropKind;
    {* 关联对象属性的类型，如果属性不正确，会弹出 EInvalidPropItem 异常 }
    property PropValue: Variant read GetPropValue write SetPropValue;
    {* 关联对象属性的值，可读取和修改。如果属性不正确，会弹出 EInvalidPropItem 异常 }
    property Value: Variant read FValue write FValue;
    {* 可用来临时存放当前设置值 }
  published
    property DefaultValue: Variant read GetDefaultValue write SetDefaultValue;
    {* 关联对象属性的默认值，如果没有指定，会自动从属性定义中查找。
       如果没有设置默认值且属性不正确，会弹出 EInvalidPropItem 异常 }
    property Instance: TObject read FInstance write FInstance;
    {* 关联的对象实例 }
    property List: TStrings read FList write SetList;
    {* 附加显示信息，如果关联对象属性为枚举或集合，该字符串列表用来保存每一个
       枚举项的描述信息。如果关联对象属性为字符串类型，该列表可用来保存可选择
       的下拉列表值。如果关联对象属性为整数类型，该列表可用来保存下拉列表值，
       属性整数对应列表索引 }
    property MaxValue: Variant read GetMaxValue write SetMaxValue;
    {* 关联对象属性允许的最大值 }
    property MinValue: Variant read GetMinValue write SetMinValue;
    {* 关联对象属性允许的最小值 }
    property PropName: string read FPropName write FPropName;
    {* 关联对象属性的名称 }
  end;

{ TCnOptionGroup }

  TCnOptionGroup = class(TCnBaseOption)
  private
    FList: TObjectList;
    function GetCount: Integer;
    function GetItems(Index: Integer): TCnBaseOption;
    procedure SetItems(Index: Integer; Value: TCnBaseOption);
  protected
    property List: TObjectList read FList;
  public
    constructor Create(AOwner: TCnOptionGroup); override;
    {* 类构造器，参数为所有者 }
    destructor Destroy; override;
    {* 类析构器 }
    function Add(Item: TCnBaseOption): Integer;
    {* 增加一个子设置项 }
    function AddGroup(const AText: string): TCnOptionGroup;
    {* 增加一个新的子设置组，参数为标题 }
    function AddCustom(const AText: string; OnClick: TNotifyEvent; const ACaption:
      string = ''): TCnOptionCustom;
    {* 增加一个新的按钮设置项 }
    function AddItem(AInstance: TObject; const APropName: string; const AText:
      string = ''; const AList: string = ''): TCnOptionItem; overload;
    {* 增加一个子属性设置项，参数见 TCnOptionItem 定义，其中 AList 为多行文本字符串 }
    function AddItem(AInstance: TObject; const APropName: string; const AText:
      string; const AList: string; ADefaultValue, AMinValue, AMaxValue: Variant):
      TCnOptionItem; overload;
    {* 增加一个子属性设置项，参数见 TCnOptionItem 定义，其中 AList 为多行文本字符串 }
    procedure Assign(Source: TPersistent); override;
    {* 对象赋值方法 }
    procedure Clear;
    {* 清空所有子项 }
    procedure Delete(Index: Integer);
    {* 删除并释放一个子项 }
    function IndexOf(Item: TCnBaseOption): Integer;
    {* 查找子项，返回索引号 }
    procedure Insert(Index: Integer; Item: TCnBaseOption);
    {* 在指定位置插入一个子项 }
    procedure Move(CurIndex, NewIndex: Integer);
    {* 移动子项到新的位置 }
    property Count: Integer read GetCount;
    {* 子项的总数 }
    property Items[Index: Integer]: TCnBaseOption read GetItems write SetItems;
      default;
    {* 设置参数子项数组 }
  end;

function IsBooleanType(PInfo: PTypeInfo): Boolean;
function IsBoolType(PInfo: PTypeInfo): Boolean;

implementation

{$IFDEF DEBUG}
uses
  CnDebug;
{$ENDIF}

type
  TPersistentHack = class(TPersistent);

function VarToIntVar(const Value: Variant): Variant;
var
  R, E: Integer;
  S: string;
begin
  Result := Null;
  try
    if VarIsNull(Value) then
      Exit;

    S := Trim(VarToStr(Value));
    if S <> '' then
    begin
      Val(S, R, E);
      if E = 0 then
        Result := R;
    end;
  except
    ;
  end;
end;

function VarToFloatVar(const Value: Variant): Variant;
var
  R: Extended;
begin
  if TextToFloat(PChar(VarToStr(Value)), R, fvExtended) then
    Result := R
  else
    Result := Null;
end;

function IsBoolType(PInfo: PTypeInfo): Boolean;
begin
  Result := (PInfo^.Kind = tkEnumeration) and
    (GetTypeData(PInfo)^.MinValue < 0); // Longbool/wordbool/bytebool
end;

function IsBooleanType(PInfo: PTypeInfo): Boolean;
begin
  Result := (PInfo.Kind = tkEnumeration) and
    (GetTypeData(PInfo)^.BaseType^ = TypeInfo(Boolean));
end;

{ TCnBaseOption }

constructor TCnBaseOption.Create(AOwner: TCnOptionGroup);
begin
  inherited Create;
  FOwner := AOwner;
  FText := '';
  FVisible := True;
  FIsStored := True;
end;

procedure TCnBaseOption.Assign(Source: TPersistent);
var
  Stream: TMemoryStream;
  Reader: TReader;
  Writer: TWriter;
  Count: Integer;
  PropIdx: Integer;
  PropList: PPropList;
  PropName: string;
  PropInfo: PPropInfo;
begin
  if Source is ClassType then
  begin
    // 使用 RTTI 来保证赋值所有 published 属性（流不能传递值为 Default 的属性）
    Count := GetPropList(Self.ClassInfo, tkProperties - [tkArray, tkRecord,
      tkInterface], nil);
    GetMem(PropList, Count * SizeOf(Pointer));
    try
      GetPropList(ClassInfo, tkProperties - [tkArray, tkRecord,
        tkInterface], @PropList^[0]);
      for PropIdx := 0 to Count - 1 do
      begin
        PropInfo := PropList^[PropIdx];
        PropName := string(PropInfo^.Name);
        case PropInfo^.PropType^^.Kind of
          tkInteger, tkChar, tkWChar, tkClass, tkEnumeration, tkSet:
            SetOrdProp(Self, PropInfo, GetOrdProp(Source, PropInfo));
          tkFloat:
            SetFloatProp(Self, PropInfo, GetFloatProp(Source, PropInfo));
          tkString, tkLString, tkWString{$IFDEF UNICODE}, tkUString{$ENDIF}:
            SetStrProp(Self, PropInfo, GetStrProp(Source, PropInfo));
          tkVariant:
            SetVariantProp(Self, PropInfo, GetVariantProp(Source, PropInfo));
          tkInt64:
            SetInt64Prop(Self, PropInfo, GetInt64Prop(Source, PropInfo));
          tkMethod:
            SetMethodProp(Self, PropInfo, GetMethodProp(Source, PropInfo));
        end;
      end;
    finally
      FreeMem(PropList);
    end;

    // 使用流来传递自定义的属性
    Stream := nil;
    Reader := nil;
    Writer := nil;
    try
      Stream := TMemoryStream.Create;
      Writer := TWriter.Create(Stream, 4096);
      TPersistentHack(Source).DefineProperties(Writer);
      Writer.FlushBuffer;
      Stream.Position := 0;
      Reader := TReader.Create(Stream, 4096);
      TPersistentHack(Self).DefineProperties(Reader);
    finally
      FreeAndNil(Reader);
      FreeAndNil(Writer);
      FreeAndNil(Stream);
    end;
  end
  else
    inherited Assign(Source);
end;

{ TCnOptionItem }

constructor TCnOptionItem.Create(AOwner: TCnOptionGroup);
begin
  inherited;
  FValue := Null;
  FList := TStringList.Create;
end;

destructor TCnOptionItem.Destroy;
begin
  ClearValue;
  FList.Free;
  inherited;
end;

procedure TCnOptionItem.ApplyOption;
var
  SrcObj, DstObj: TPersistent;
begin
  if (PropKind = tkClass) and not VarIsNull(Value) then
  begin
{$IFDEF WIN64}
    SrcObj := TPersistent(Integer(Value));
    DstObj := TPersistent(Integer(PropValue));
{$ELSE}
    Integer(SrcObj) := Value;
    Integer(DstObj) := PropValue;
{$ENDIF}
    if (SrcObj is TPersistent) and (DstObj is TPersistent) and (SrcObj <> DstObj) then
      DstObj.Assign(SrcObj)
    else
      PropValue := Value;
  end
  else
  begin
    PropValue := Value;
  end;
end;

procedure TCnOptionItem.ClearValue;
var
  Obj: TObject;
begin
  if (PropKind = tkClass) and not VarIsNull(Value) and (Value <> PropValue) then
  begin
{$IFDEF WIN64}
    Obj := TObject(Integer(Value));
{$ELSE}
    Integer(Obj) := Value;
{$ENDIF}
    Obj.Free;
  end;
  Value := Null;
end;

procedure TCnOptionItem.DefaultOption;
var
  SrcObj, DstObj: TPersistent;
begin
  if (PropKind = tkClass) and not VarIsNull(DefaultValue) then
  begin
{$IFDEF WIN64}
    SrcObj := TPersistent(Integer(DefaultValue));
    DstObj := TPersistent(Integer(PropValue));
{$ELSE}
    Integer(SrcObj) := DefaultValue;
    Integer(DstObj) := PropValue;
{$ENDIF}
    if (SrcObj is TPersistent) and (DstObj is TPersistent) and (SrcObj <> DstObj) then
      DstObj.Assign(SrcObj)
    else
      PropValue := DefaultValue;
  end
  else
  begin
    PropValue := DefaultValue;
  end;
  ResetOption;
end;

procedure TCnOptionItem.ResetOption;
var
  SrcObj, DstObj: TPersistent;
begin
  ClearValue;
  if PropKind = tkClass then
  begin
{$IFDEF WIN64}
    SrcObj := TPersistent(Integer(PropValue));
{$ELSE}
    Integer(SrcObj) := PropValue;
{$ENDIF}
    if SrcObj is TFont then
      DstObj := TFont.Create
    else if SrcObj is TStrings then
      DstObj := TStringList.Create
    else
      DstObj := nil;
      
    if DstObj <> nil then
    begin
      DstObj.Assign(SrcObj);
      Value := Integer(DstObj);
    end
    else
      Value := PropValue;
  end
  else
  begin
    Value := PropValue;
  end;
end;

function TCnOptionItem.GetDefaultValue: Variant;
begin
  if not VarIsNull(FDefaultValue) then
    Result := FDefaultValue
  else if PropKind in [tkInteger, tkChar, tkEnumeration, tkSet, tkWChar] then
    Result := PropInfo^.Default
  else
    Result := PropValue;
end;

procedure TCnOptionItem.SetList(Value: TStrings);
begin
  FList.Assign(Value);
end;

function TCnOptionItem.GetMaxValue: Variant;
begin
  if not VarIsNull(FMaxValue) then
    Result := FMaxValue
  else
  begin
    if PropKind in [tkInteger, tkChar, tkEnumeration, tkWChar] then
      Result := GetTypeData(PropInfo^.PropType^)^.MaxValue
    else
      Result := Null;
  end;
end;

function TCnOptionItem.GetMinValue: Variant;
begin
  if not VarIsNull(FMinValue) then
    Result := FMinValue
  else
  begin
    if PropKind in [tkInteger, tkChar, tkEnumeration, tkWChar] then
      Result := GetTypeData(PropInfo^.PropType^)^.MinValue
    else
      Result := Null;
  end;
end;

function TCnOptionItem.GetPropInfo: PPropInfo;
begin
  Result := TypInfo.GetPropInfo(Instance, PropName);
  if Result = nil then
    raise EInvalidPropItem.CreateFmt('Invalid property define: %s.%s',
      [Instance.ClassName, PropName]);
end;

function TCnOptionItem.GetPropKind: TTypeKind;
begin
  Result := PropInfo^.PropType^^.Kind;
end;

function TCnOptionItem.GetPropValue: Variant;
begin
  Result := TypInfo.GetPropValue(Instance, PropName, False);
end;

procedure TCnOptionItem.SetPropValue(const Value: Variant);
var
  Obj: TObject;
begin
  case PropInfo.PropType^^.Kind of
    tkInteger, tkChar, tkWChar, tkEnumeration, tkSet, tkFloat,
    tkString, tkLString, tkWString, tkVariant, tkInt64{$IFDEF UNICODE}, tkUString{$ENDIF}:
      begin
        if IsBooleanType(PropInfo.PropType^) then
        begin
          if Value <> 0 then
            TypInfo.SetEnumProp(Instance, PropInfo, BooleanIdents[True])
          else
            TypInfo.SetEnumProp(Instance, PropInfo, BooleanIdents[False]);
        end
        else if IsBoolType(PropInfo.PropType^) then
        begin
          if Value <> 0 then
            TypInfo.SetOrdProp(Instance, PropName, -1)
          else
            TypInfo.SetOrdProp(Instance, PropName, 0)
        end
        else
          TypInfo.SetPropValue(Instance, PropName, Value);
      end;
    tkClass:
      begin
{$IFDEF WIN64}
        Obj := TObject(Integer(Value));
{$ELSE}
        Integer(Obj) := Value;
{$ENDIF}
        SetObjectProp(Instance, PropInfo, Obj);
      end;
  else
    ;
  end;
end;

procedure TCnOptionItem.SetDefaultValue(const Value: Variant);
begin
  FDefaultValue := Null;
  if not VarIsNull(Value) then
    case PropInfo.PropType^^.Kind of
      tkInteger, tkEnumeration, tkSet, tkInt64:
        FDefaultValue := VarToIntVar(Value);
      tkFloat:
        FDefaultValue := VarToFloatVar(Value);
      tkChar, tkWChar, tkString, tkLString, tkWString{$IFDEF UNICODE}, tkUString{$ENDIF}:
        FDefaultValue := VarToStr(Value);
    else
      FDefaultValue := Value;
    end;
end;

procedure TCnOptionItem.SetMaxValue(const Value: Variant);
begin
  FMaxValue := VarToIntVar(Value);
end;

procedure TCnOptionItem.SetMinValue(const Value: Variant);
begin
  FMinValue := VarToIntVar(Value);
end;

{ TCnOptionGroup }

constructor TCnOptionGroup.Create(AOwner: TCnOptionGroup);
begin
  inherited;
  FList := TObjectList.Create;
end;

destructor TCnOptionGroup.Destroy;
begin
  FList.Free;
  inherited;
end;

function TCnOptionGroup.Add(Item: TCnBaseOption): Integer;
begin
  if Item.FOwner <> nil then
    Item.FOwner.FList.Extract(Item);
  Result := FList.Add(Item);
  Item.FOwner := Self;
end;

function TCnOptionGroup.AddGroup(const AText: string): TCnOptionGroup;
begin
  Result := TCnOptionGroup.Create(Self);
  Result.FText := AText;
  Add(Result);
end;

function TCnOptionGroup.AddCustom(const AText: string;
  OnClick: TNotifyEvent; const ACaption: string): TCnOptionCustom;
begin
  Result := TCnOptionCustom.Create(Self);
  Result.FText := AText;
  Result.FCaption := ACaption;
  Result.FOnClick := OnClick;
  Add(Result);
end;

function TCnOptionGroup.AddItem(AInstance: TObject; const APropName: string;
  const AText: string = ''; const AList: string = ''): TCnOptionItem;
begin
  Result := AddItem(AInstance, APropName, AText, AList, Null, Null, Null);
end;

function TCnOptionGroup.AddItem(AInstance: TObject; const APropName: string;
  const AText: string; const AList: string; ADefaultValue, AMinValue,
  AMaxValue: Variant): TCnOptionItem;
begin
  Result := TCnOptionItem.Create(Self);
  Result.Instance := AInstance;
  Result.PropName := APropName;
  if AText <> '' then
    Result.Text := AText
  else
    Result.Text := APropName;
  Result.List.Text := AList;
  Result.DefaultValue := ADefaultValue;
  Result.MinValue := AMinValue;
  Result.MaxValue := AMaxValue;
  Add(Result);
end;

procedure TCnOptionGroup.Assign(Source: TPersistent);
var
  i: Integer;
  Item: TCnBaseOption;
begin
  inherited Assign(Source);
  if Source is TCnOptionGroup then
  begin
    Clear;
    for i := 0 to TCnOptionGroup(Source).Count - 1 do
    begin
      Item := TCnOptionClass(TCnOptionGroup(Source).Items[i].ClassType).Create(Self);
      Item.Assign(TCnOptionGroup(Source).Items[i]);
      Add(Item);
    end;
  end;
end;

procedure TCnOptionGroup.Clear;
begin
  FList.Clear;
end;

procedure TCnOptionGroup.Delete(Index: Integer);
begin
  FList.Delete(Index);
end;

function TCnOptionGroup.IndexOf(Item: TCnBaseOption): Integer;
begin
  Result := FList.IndexOf(Item);
end;

procedure TCnOptionGroup.Insert(Index: Integer; Item: TCnBaseOption);
begin
  FList.Insert(Index, Item);
end;

procedure TCnOptionGroup.Move(CurIndex, NewIndex: Integer);
begin
  FList.Move(CurIndex, NewIndex);
end;

function TCnOptionGroup.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TCnOptionGroup.GetItems(Index: Integer): TCnBaseOption;
begin
  Result := TCnOptionItem(FList[Index]);
end;

procedure TCnOptionGroup.SetItems(Index: Integer; Value: TCnBaseOption);
begin
  TCnBaseOption(FList[Index]).Assign(Value);
end;

end.
