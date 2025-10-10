{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     �й����Լ��Ŀ���Դ�������������                         }
{                   (C)Copyright 2001-2025 CnPack ������                       }
{                   ------------------------------------                       }
{                                                                              }
{            ���������ǿ�Դ��������������������� CnPack �ķ���Э������        }
{        �ĺ����·�����һ����                                                }
{                                                                              }
{            ������һ��������Ŀ����ϣ�������ã���û���κε���������û��        }
{        �ʺ��ض�Ŀ�Ķ������ĵ���������ϸ���������� CnPack ����Э�顣        }
{                                                                              }
{            ��Ӧ���Ѿ��Ϳ�����һ���յ�һ�� CnPack ����Э��ĸ��������        }
{        ��û�У��ɷ������ǵ���վ��                                            }
{                                                                              }
{            ��վ��ַ��https://www.cnpack.org                                  }
{            �����ʼ���master@cnpack.org                                       }
{                                                                              }
{******************************************************************************}

unit CnAutoOption;
{* |<PRE>
================================================================================
* ������ƣ������ӹ��������
* ��Ԫ���ƣ��Զ����������ඨ�嵥Ԫ
* ��Ԫ���ߣ��ܾ��� (zjy@cnpack.org)
* ����ƽ̨��PWin2000 SP4 + Delphi 5.01
* ���ݲ��ԣ�PWin9X/2000/XP + Delphi 5/6/7
* �� �� �����õ�Ԫ�е��ַ��������ϱ��ػ�����ʽ
* ��    ע���õ�Ԫ�������Զ�����������Ϣ��
*           ����������ʱʹ����״�ṹ����������Ϣ������ʾͨ�õ����ý��档
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
  {* ��Ч��������Ϣ���쳣 }

{ TCnBaseOption }

  TCnOptionGroup = class;

  TCnOptionClass = class of TCnBaseOption;
  {* ������Ϣ������ }

  TCnBaseOption = class(TPersistent)
  {* ������Ϣ������ }
  private
    FIsStored: Boolean;
    FOwner: TCnOptionGroup;
    FText: string;
    FVisible: Boolean;
  public
    constructor Create(AOwner: TCnOptionGroup); virtual;
    {* �๹����������Ϊ������ }
    procedure Assign(Source: TPersistent); override;
    {* ����ֵ������ʹ�� RTTI ���Զ��Է������Ը�ֵ������ֻ��Ҫ����Ƿ�������
       ���ɣ�������� override �÷�������Ҫ inherited }
    property Owner: TCnOptionGroup read FOwner;
    {* ������������� }
  published
    property IsStored: Boolean read FIsStored write FIsStored;
    {* ��ʶ���������Ƿ񱣴浽�����ļ��� }
    property Text: string read FText write FText;
    {* ��������ʾ�ı��� }
  end;

{ TCnOptionCustom }

  TCnOptionCustom = class(TCnBaseOption)
  {* ʹ��һ����ť�������õĲ������� }
  private
    FOnClick: TNotifyEvent;
    FCaption: string;
  published
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    {* ���ð�ť����¼� }
    property Caption: string read FCaption write FCaption;
    {* ���ð�ť���� }
  end;

{ TCnOptionItem }

  TCnOptionItem = class(TCnBaseOption)
  {* �����������࣬����ʵ����һ����������Ҫ���õĶ������Թ��� }
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
    {* �๹����������Ϊ������ }
    destructor Destroy; override;
    {* �������� }
    procedure ApplyOption;
    {* Ӧ�õ�ǰ�����õ��������� }
    procedure DefaultOption;
    {* �ָ�����Ϊԭ�������Ե�Ĭ��ֵ }
    procedure ResetOption;
    {* �ָ�����Ϊԭ�������Եĵ�ǰֵ }
    property PropInfo: PPropInfo read GetPropInfo;
    {* �������������������Ϣ��������Բ���ȷ���ᵯ�� EInvalidPropItem �쳣 }
    property PropKind: TTypeKind read GetPropKind;
    {* �����������Ե����ͣ�������Բ���ȷ���ᵯ�� EInvalidPropItem �쳣 }
    property PropValue: Variant read GetPropValue write SetPropValue;
    {* �����������Ե�ֵ���ɶ�ȡ���޸ġ�������Բ���ȷ���ᵯ�� EInvalidPropItem �쳣 }
    property Value: Variant read FValue write FValue;
    {* ��������ʱ��ŵ�ǰ����ֵ }
  published
    property DefaultValue: Variant read GetDefaultValue write SetDefaultValue;
    {* �����������Ե�Ĭ��ֵ�����û��ָ�������Զ������Զ����в��ҡ�
       ���û������Ĭ��ֵ�����Բ���ȷ���ᵯ�� EInvalidPropItem �쳣 }
    property Instance: TObject read FInstance write FInstance;
    {* �����Ķ���ʵ�� }
    property List: TStrings read FList write SetList;
    {* ������ʾ��Ϣ�����������������Ϊö�ٻ򼯺ϣ����ַ����б���������ÿһ��
       ö�����������Ϣ�����������������Ϊ�ַ������ͣ����б�����������ѡ��
       �������б�ֵ�����������������Ϊ�������ͣ����б���������������б�ֵ��
       ����������Ӧ�б����� }
    property MaxValue: Variant read GetMaxValue write SetMaxValue;
    {* ��������������������ֵ }
    property MinValue: Variant read GetMinValue write SetMinValue;
    {* �������������������Сֵ }
    property PropName: string read FPropName write FPropName;
    {* �����������Ե����� }
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
    {* �๹����������Ϊ������ }
    destructor Destroy; override;
    {* �������� }
    function Add(Item: TCnBaseOption): Integer;
    {* ����һ���������� }
    function AddGroup(const AText: string): TCnOptionGroup;
    {* ����һ���µ��������飬����Ϊ���� }
    function AddCustom(const AText: string; OnClick: TNotifyEvent; const ACaption:
      string = ''): TCnOptionCustom;
    {* ����һ���µİ�ť������ }
    function AddItem(AInstance: TObject; const APropName: string; const AText:
      string = ''; const AList: string = ''): TCnOptionItem; overload;
    {* ����һ������������������� TCnOptionItem ���壬���� AList Ϊ�����ı��ַ��� }
    function AddItem(AInstance: TObject; const APropName: string; const AText:
      string; const AList: string; ADefaultValue, AMinValue, AMaxValue: Variant):
      TCnOptionItem; overload;
    {* ����һ������������������� TCnOptionItem ���壬���� AList Ϊ�����ı��ַ��� }
    procedure Assign(Source: TPersistent); override;
    {* ����ֵ���� }
    procedure Clear;
    {* ����������� }
    procedure Delete(Index: Integer);
    {* ɾ�����ͷ�һ������ }
    function IndexOf(Item: TCnBaseOption): Integer;
    {* ����������������� }
    procedure Insert(Index: Integer; Item: TCnBaseOption);
    {* ��ָ��λ�ò���һ������ }
    procedure Move(CurIndex, NewIndex: Integer);
    {* �ƶ�����µ�λ�� }
    property Count: Integer read GetCount;
    {* ��������� }
    property Items[Index: Integer]: TCnBaseOption read GetItems write SetItems;
      default;
    {* ���ò����������� }
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
    // ʹ�� RTTI ����֤��ֵ���� published ���ԣ������ܴ���ֵΪ Default �����ԣ�
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

    // ʹ�����������Զ��������
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
    SrcObj := TPersistent(NativeInt(Value));
    DstObj := TPersistent(NativeInt(PropValue));
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
    Obj := TObject(NativeInt(Value));
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
    SrcObj := TPersistent(NativeInt(DefaultValue));
    DstObj := TPersistent(NativeInt(PropValue));
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
    SrcObj := TPersistent(NativeInt(PropValue));
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
{$IFDEF WIN64}
      Value := NativeInt(DstObj);
{$ELSE}
      Value := Integer(DstObj);
{$ENDIF}
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
        Obj := TObject(NativeInt(Value));
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
