{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2025 CnPack 开发组                       }
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

unit CnJsonPersistent;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：Json 持久化对象单元
* 单元作者：周劲羽
* 备    注：需要 Delphi 2010 及以上版本支持
* 开发平台：Win10 + Delphi 2010
* 兼容测试：暂未进行
* 本 地 化：该单元无需本地化处理
* 修改记录：2019.07.26 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

{$IFDEF DELPHI2010_UP}

uses
{$IFDEF MSWINDOWS}
  Windows,
{$ENDIF}
  SysUtils, Classes, Generics.Collections, Generics.Defaults, StrUtils, Math,
  JsonDataObjects, TypInfo, Rtti, IOUtils;

type
  // 用于声明字段是否需要持久化的特性类
  CnJsonPersistentAttr = class sealed(TCustomAttribute)
  private
    FPersistent: Boolean;
    FFieldKey: string;
  public
    constructor Create(Persistent: Boolean = True; FieldKey: string = '');
  end;

  TIntegerList = class(TList<Integer>);

  // 保存到 json 时空字符串是否跳过导出：
  // seDefault：默认（根据 CnJsonPersistentSkipEmptyString）
  // seTrue：跳过不导出
  // seFalse：正常导出
  TSkipEmptyString = (seDefault, seTrue, seFalse);

  // 支持自动使用 Json 进行持久化/反持久化的类
  // 直接在 public 区定义字段即可，对应 Json 中的对象属性，支持以下类型
  // 1. 简单类型
  //    Integer、Int64、Single、Double、String、Boolean、Enum
  // 2. 子对象类型
  //    必须也是 TCnJsonPersistent 子类
  // 3. 简单类型的集合列表
  //    整数类必须用 TIntegerList 类型
  //    其它类必须用 TStringList 类型
  // 4. 对象类型的集合列表
  //    必须用 TCnJsonPersistentObjectList<T> 的泛型子类
  // 注：
  // 1. 子类中定义的字段都不需要在类构造器和析构器中创建和释放，基类会自动处理
  // 2. 如果字段是关键字，前面加下划线_，如 _type
  TCnJsonPersistent = class(TPersistent)
  private
    function CanExportEmptyString: Boolean;
  public
    [CnJsonPersistentAttr(False)]
    json: string;
    [CnJsonPersistentAttr(False)]
    skipEmptyString: TSkipEmptyString;
    procedure Log(const Msg: string);
    function LoadFromJsonObject(AJson: TJsonObject): Boolean; virtual;
    function LoadFromFile(const AFileName: string): Boolean; virtual;
    function LoadFromJsonString(const AJsonString: string): Boolean; virtual;
    function SaveToJsonObject(AJson: TJsonObject): Boolean; virtual;
    function SaveToFile(const AFileName: string; Compact: Boolean = True): Boolean; virtual;
    function SaveToJsonString(var AJsonString: string; Compact: Boolean = True): Boolean; virtual;
    procedure Assign(Source: TPersistent); override;
    constructor Create; virtual;
    destructor Destroy; override;
  end;

  // 支持自动使用 Json 进行持久化/反持久化的对象列表泛型基类
  TCnJsonPersistentObjectList<T: TCnJsonPersistent> = class(TObjectList<T>)
  public
    [CnJsonPersistentAttr(False)]
    json: string;
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Log(const Msg: string);
    function LoadFromJsonObject(AJson: TJsonArray): Boolean; virtual;
    function LoadFromFile(const AFileName: string): Boolean; virtual;
    function LoadFromJsonString(const AJsonString: string): Boolean; virtual;
    function SaveToJsonObject(AJson: TJsonArray): Boolean; virtual;
    function SaveToFile(const AFileName: string; Compact: Boolean = True): Boolean; virtual;
    function SaveToJsonString(var AJsonString: string; Compact: Boolean = True): Boolean; virtual;
  end;

  TCnJsonPersistentClass = class of TCnJsonPersistent;

var
  CnJsonPersistentEnableDbgLog: Boolean = False;
  CnJsonPersistentSkipEmptyString: Boolean = False;

procedure DbgLog(const Msg: string);

{$ENDIF}

implementation

{$IFDEF DELPHI2010_UP}

procedure DbgLog(const Msg: string);
begin
  if CnJsonPersistentEnableDbgLog then
  begin
{$IFDEF MSWINDOWS}
    OutputDebugString(PChar(Msg));
{$ENDIF}
  end;
end;

{ CnJsonPersistentAttr }

constructor CnJsonPersistentAttr.Create(Persistent: Boolean; FieldKey: string);
begin
  FPersistent := Persistent;
  FFieldKey := FieldKey;
end;

{ TCnJsonPersistent }

procedure TCnJsonPersistent.Assign(Source: TPersistent);
var
  JsObj: TJsonObject;
begin
  if Source is TCnJsonPersistent then
  begin
    JsObj := TJsonObject.Create;
    try
      if TCnJsonPersistent(Source).SaveToJsonObject(JsObj) then
        LoadFromJsonObject(JsObj);
    finally
      JsObj.Free;
    end;
  end
  else
    inherited;
end;

function TCnJsonPersistent.CanExportEmptyString: Boolean;
begin
  case skipEmptyString of
    seDefault: Result := not CnJsonPersistentSkipEmptyString;
    seTrue: Result := False;
    seFalse: Result := True;
  else
    Result := True;
  end;
end;

constructor TCnJsonPersistent.Create;
var
  RttiType: TRttiType;
  Obj: TObject;
  Field: TRttiField;
begin
  Log('Create');
  skipEmptyString := seDefault;
  RttiType := TRttiContext.Create.GetType(ClassType);
  for Field in RttiType.GetFields do
  begin
    try
      if (Field.FieldType.TypeKind = tkClass) and (Field.FieldType is TRttiInstanceType) then
      begin
        Log(Format('Create sub object -> %s: %s', [Field.Name, Field.FieldType.Name]));
        Obj := TRttiInstanceType(Field.FieldType).MetaclassType.Create;
        if Pos(LowerCase('TCnJsonPersistentObjectList<'), LowerCase(Field.FieldType.Name)) = 1 then
          TCnJsonPersistentObjectList<TCnJsonPersistent>(Obj).Create
        else if Obj is TIntegerList then
          TIntegerList(Obj).Create
        else if Obj is TStringList then
          TStringList(Obj).Create
        else if Obj is TCnJsonPersistent then
          TCnJsonPersistent(Obj).Create;
      {$IFDEF AUTOREFCOUNT}
        Obj.__ObjAddRef;
      {$ENDIF}
        Field.SetValue(Self, Obj);
      end;
    except
      on E: Exception do
        Log(Format('CreateObject for %s.%s error. %s: %s', [ClassName, Field.Name,
          E.ClassName, E.Message]));
    end;
  end;
end;

destructor TCnJsonPersistent.Destroy;
var
  RttiType: TRttiType;
  Obj: TObject;
  Field: TRttiField;
begin
  Log('Destroy');
  RttiType := TRttiContext.Create.GetType(ClassType);
  for Field in RttiType.GetFields do
  begin
    try
      if Field.FieldType.TypeKind = tkClass then
      begin
        Obj := Field.GetValue(Self).AsObject;
        if Obj <> nil then
        begin
          Log(Format('Free sub object -> %s: %s', [Field.Name, Field.FieldType.Name]));
{$IFDEF NEXTGEN}
          Obj.DisposeOf;
{$ENDIF}
          Field.SetValue(Self, nil);
        end;
      end;
    except
      on E: Exception do
        Log(Format('FreeObject for %s.%s error. %s: %s', [ClassName, Field.Name,
          E.ClassName, E.Message]));
    end;
  end;
  inherited;
end;

procedure TCnJsonPersistent.Log(const Msg: string);
begin
  DbgLog(Format('[%s]%s', [ClassName, Msg]));
end;

function TCnJsonPersistent.LoadFromFile(const AFileName: string): Boolean;
var
  JsonObj: TJsonBaseObject;
begin
  Log('LoadFromFile: ' + AFileName);
  Result := False;
  try
    if FileExists(AFileName) then
    begin
      JsonObj := TJsonObject.ParseFromFile(AFileName);
      try
        if JsonObj is TJsonObject then
        begin
          Result := LoadFromJsonObject(TJsonObject(JsonObj));
        end;
      finally
        JsonObj.Free;
      end;
    end;
  except
    on E: Exception do
      Log(Format('%s.LoadFromFile error. %s: %s', [ClassName,
        E.ClassName, E.Message]));
  end;
end;

type
  THackRttiField = class(TRttiField)
  public
    procedure SetEnumValue(Instance: TObject; Value: Cardinal);
  end;

{ THackTRttiField }

procedure THackRttiField.SetEnumValue(Instance: TObject; Value: Cardinal);
var
  P: Pointer;
begin
  P := Pointer(@PByteArray(Instance)[Offset]);
  case FieldType.TypeSize of
    1: PByte(P)^ := Value;
    2: PWORD(P)^ := Value;
    4: PCardinal(P)^ := Value;
  end;
end;

function TCnJsonPersistent.LoadFromJsonObject(AJson: TJsonObject): Boolean;
var
  RttiType: TRttiType;
  NewType: TRttiInstanceType;
  Field: TRttiField;
  Obj: TObject;
  FieldName: string;
  NewClassName: string;
  S: string;
  WS: WideString;
  AC: AnsiChar;
  WC: WideChar;
  Attr: TCustomAttribute;
  Ps: Boolean;
  i: Integer;
  V: TValue;
begin
  Log('LoadFromJsonObject');
  Result := False;
  try
    if AJson = nil then
      Exit;
    json := AJson.ToJSON;
    RttiType := TRttiContext.Create.GetType(ClassType);
    for Field in RttiType.GetFields do
    begin
      try
        FieldName := Field.Name;
        Ps := True;
        if Field.Visibility <> mvPublic then
        begin
          Continue;
        end;

        for Attr in Field.GetAttributes do
        begin
          if Attr is CnJsonPersistentAttr then
          begin
            Ps := CnJsonPersistentAttr(Attr).FPersistent;
            if CnJsonPersistentAttr(Attr).FFieldKey <> '' then
              FieldName := CnJsonPersistentAttr(Attr).FFieldKey;
            Break;
          end;
        end;

        if not Ps then
          Continue;

        if Pos('_', FieldName) = 1 then
          Delete(FieldName, 1, 1);
        if AJson.Contains(FieldName) then
        begin
          Log(Format('%s.%s Type: %s', [ClassName, FieldName,
            GetEnumName(TypeInfo(TTypeKind), Ord(Field.FieldType.TypeKind))]));
          case Field.FieldType.TypeKind of
            tkInteger:
              begin
                Log(Format('SetIntValue: %d', [AJson.I[FieldName]]));
                Field.SetValue(Self, AJson.I[FieldName]);
              end;
            tkInt64:
              begin
                Log(Format('SetInt64Value: %d', [AJson.L[FieldName]]));
                Field.SetValue(Self, AJson.L[FieldName]);
              end;
            tkFloat:
              begin
                Log(Format('SetFloatValue: %f', [AJson.F[FieldName]]));
                Field.SetValue(Self, AJson.F[FieldName]);
              end;
            tkChar, tkWChar:
              begin
                S := AJson.S[FieldName];
                Log(Format('SetCharValue: %s', [S]));
                if Field.FieldType.TypeKind = tkChar then
                begin
                  if S <> '' then
                    AC := AnsiChar(S[1])
                  else
                    AC := #0;
                  TValue.Make(@AC, TypeInfo(AnsiChar), V);
                end
                else
                begin
                  if S <> '' then
                    WC := WideChar(S[1])
                  else
                    WC := #0;
                  TValue.Make(@WC, TypeInfo(WideChar), V);
                end;
                Field.SetValue(Self, V);
              end;
            tkString, tkLString, tkUString:
              begin
                Log(Format('SetStringValue: %s', [AJson.S[FieldName]]));
                Field.SetValue(Self, AJson.S[FieldName]);
              end;
            tkWString:
              begin
                // Avoid a bug of Delphi2010 in ConvStr2Str() in Rtti.pas.
                Log(Format('SetWideStringValue: %s', [AJson.S[FieldName]]));
                WS := WideString(AJson.S[FieldName]);
                TValue.Make(@WS, TypeInfo(WideString), V);
                Field.SetValue(Self, V);
              end;
            tkEnumeration:
              begin
                if SameText(Field.FieldType.Name, 'Boolean') then
                begin
                  Field.SetValue(Self, AJson.B[FieldName]);
                end
                else if SameText(Field.FieldType.Name, 'ByteBool') or
                  SameText(Field.FieldType.Name, 'WordBool') or
                  SameText(Field.FieldType.Name, 'LongBool') then
                begin
                  Log(Format('SetBoolValue: %s', [BoolToStr(AJson.B[FieldName], True)]));
                  V := Field.GetValue(Self);
                  if AJson.B[FieldName] then
                  begin
                    if SameText(Field.FieldType.Name, 'ByteBool') then
                      TValueData(V).FAsUByte := $FF
                    else if SameText(Field.FieldType.Name, 'WordBool') then
                      TValueData(V).FAsUWord := $FFFF
                    else if SameText(Field.FieldType.Name, 'LongBool') then
                      TValueData(V).FAsULong := $FFFFFFFF;
                  end
                  else
                    TValueData(V).FAsULong := 0;
                  Field.SetValue(Self, V);
                end
                else if Field.FieldType is TRttiEnumerationType then
                begin
                  Log(Format('SetEnumValue: %s', [AJson.S[FieldName]]));
                  THackRttiField(Field).SetEnumValue(Self, GetEnumValue(Field.FieldType.Handle, AJson.S[FieldName]));
                end
                else
                  Log(Format('Warning: Enum %s is not supported.', [Field.FieldType.Name]));
              end;
            tkSet:
              begin
                Log(Format('Warning: Set %s is not supported.', [Field.FieldType.Name]));
              end;
            tkClass:
              begin
                if (AJson.Types[FieldName] = jdtObject) and Field.GetValue(Self).IsObject then
                begin
                  Log(Format('ObjectType', [Field.FieldType.Name]));
                  Obj := TCnJsonPersistent(Field.GetValue(Self).AsObject);
                  if Obj <> nil then
                  begin
                    if Obj is TCnJsonPersistent then
                    begin
                      TCnJsonPersistent(Obj).LoadFromJsonObject(AJson.O[FieldName]);
                    end
                    else
                      Log('Warning: Object type is not supported.');
                  end
                  else
                    Log('Warning: Object is nil.');
                end
                else if (AJson.Types[FieldName] = jdtArray) and
                  (Pos(LowerCase('TCnJsonPersistentObjectList<'), LowerCase(Field.FieldType.Name)) = 1) then
                begin
                  NewClassName := Field.FieldType.Name;
                  NewClassName := Copy(NewClassName, Pos('<', NewClassName) + 1,
                    Pos('>', NewClassName) - Pos('<', NewClassName) - 1);
                  Log('Is TCnJsonPersistentObjectList Type. Instance Class Type: ' + NewClassName);
                  NewType := TRttiInstanceType(TRttiContext.Create.FindType(NewClassName));
                  if NewType <> nil then
                  begin
                    TCnJsonPersistentObjectList<TCnJsonPersistent>
                      (Field.GetValue(Self).AsObject).json := AJson.A[FieldName].ToJSON;
                    TCnJsonPersistentObjectList<TCnJsonPersistent>
                      (Field.GetValue(Self).AsObject).Clear;
                    for i := 0 to AJson.A[FieldName].Count - 1 do
                    begin
                      Log(Format('Create New %s Object. Index: %d', [NewClassName, i]));
                      Obj := NewType.MetaclassType.Create;
                    {$IFDEF AUTOREFCOUNT}
                      Obj.__ObjAddRef;
                    {$ENDIF}
                      TCnJsonPersistent(Obj).Create;
                      TCnJsonPersistent(Obj).LoadFromJsonObject(AJson.A[FieldName].O[i]);
                      TCnJsonPersistentObjectList<TCnJsonPersistent>(Field.GetValue(Self).AsObject).
                        Add(TCnJsonPersistent(Obj));
                    end;
                  end
                  else
                    Log('Warning: Instance Class Type is nil.');
                end
                else if (AJson.Types[FieldName] = jdtArray) and Field.GetValue(Self).IsObject then
                begin
                  Obj := Field.GetValue(Self).AsObject;
                  Log('Is List Type. Object Type: ' + Field.FieldType.Name);
                  if Obj <> nil then
                  begin
                    if Obj is TStringList then
                    begin
                      Log('Is TStringList Type');
                      for i := 0 to AJson.A[FieldName].Count - 1 do
                      begin
                        Log('Add String Item: ' + AJson.A[FieldName].S[i]);
                        TStringList(Obj).Add(AJson.A[FieldName].S[i]);
                      end;
                    end
                    else if Obj is TIntegerList then
                    begin
                      Log('Is TIntegerList Type');
                      for i := 0 to AJson.A[FieldName].Count - 1 do
                      begin
                        Log('Add Integer Item: ' + IntToStr(AJson.A[FieldName].I[i]));
                        TIntegerList(Obj).Add(AJson.A[FieldName].I[i]);
                      end;
                    end
                    else
                      Log('Warning: Unknown List Type.');
                  end
                  else
                    Log('Warning: Instance Class Type is nil.');
                end;
              end
            else
              begin
                Log('Warning: not supported.');
              end;
          end;
        end
        else
          Log(Format('Warning: the field %s is not exists in Json', [FieldName]));
      except
        on E: Exception do
          Log(Format('Set %s.%s error. %s: %s', [ClassName, Field.Name,
            E.ClassName, E.Message]));
      end;
    end;
    Result := True;
  except
    on E: Exception do
      Log(Format('%s.LoadFromJsonObject error. %s: %s', [ClassName,
        E.ClassName, E.Message]));
  end;
end;

function TCnJsonPersistent.LoadFromJsonString(const AJsonString: string): Boolean;
var
  JsonObj: TJsonBaseObject;
begin
  Log('LoadFromString: ' + AJsonString);
  Result := False;
  try
    if AJsonString <> '' then
    begin
      JsonObj := TJsonObject.Parse(AJsonString);
      try
        if JsonObj is TJsonObject then
        begin
          Result := LoadFromJsonObject(TJsonObject(JsonObj));
        end;
      finally
        JsonObj.Free;
      end;
    end;
  except
    on E: Exception do
      Log(Format('%s.LoadFromJsonString error. %s: %s', [ClassName,
        E.ClassName, E.Message]));
  end;
end;

function TCnJsonPersistent.SaveToFile(const AFileName: string; Compact: Boolean): Boolean;
var
  JsonObj: TJsonObject;
begin
  Log('SaveToFile: ' + AFileName);
  Result := False;
  try
    JsonObj := TJsonObject.Create;
    try
      if SaveToJsonObject(JsonObj) then
      begin
        JsonObj.SaveToFile(AFileName, Compact);
        Result := True;
      end;
    finally
      JsonObj.Free;
    end;
  except
    on E: Exception do
      Log(Format('%s.SaveToFile error. %s: %s', [ClassName,
        E.ClassName, E.Message]));
  end;
end;

function TCnJsonPersistent.SaveToJsonObject(AJson: TJsonObject): Boolean;
var
  RttiType: TRttiType;
  Field: TRttiField;
  Obj: TObject;
  FieldName: string;
  Attr: TCustomAttribute;
  List: TCnJsonPersistentObjectList<TCnJsonPersistent>;
  JsObj: TJsonObject;
  Ps: Boolean;
  i: Integer;
begin
  Log('SaveToJsonObject');
  Result := False;
  try
    if AJson = nil then
      Exit;
    RttiType := TRttiContext.Create.GetType(ClassType);
    for Field in RttiType.GetFields do
    begin
      try
        FieldName := Field.Name;
        Ps := True;
        if Field.Visibility <> mvPublic then
        begin
          Continue;
        end;

        for Attr in Field.GetAttributes do
        begin
          if Attr is CnJsonPersistentAttr then
          begin
            Ps := CnJsonPersistentAttr(Attr).FPersistent;
            if CnJsonPersistentAttr(Attr).FFieldKey <> '' then
              FieldName := CnJsonPersistentAttr(Attr).FFieldKey;

            Break;
          end;
        end;

        if not Ps then
          Continue;

        if Pos('_', FieldName) = 1 then
          Delete(FieldName, 1, 1);
        Log(Format('%s.%s Type: %s', [ClassName, FieldName,
          GetEnumName(TypeInfo(TTypeKind), Ord(Field.FieldType.TypeKind))]));
        case Field.FieldType.TypeKind of
          tkInteger:
            begin
              AJson.I[FieldName] := Field.GetValue(Self).AsInteger;
            end;
          tkInt64:
            begin
              AJson.L[FieldName] := Field.GetValue(Self).AsInt64;
            end;
          tkFloat:
            begin
              AJson.F[FieldName] := Field.GetValue(Self).AsExtended;
            end;
          tkChar, tkWChar:
            begin
              AJson.S[FieldName] := Field.GetValue(Self).AsString;
            end;
          tkString, tkLString, tkWString, tkUString:
            begin
              if CanExportEmptyString or (Field.GetValue(Self).AsString <> '') then
                AJson.S[FieldName] := Field.GetValue(Self).AsString;
            end;
          tkEnumeration:
            begin
              if SameText(Field.FieldType.Name, 'Boolean') or
                SameText(Field.FieldType.Name, 'ByteBool') or
                SameText(Field.FieldType.Name, 'WordBool') or
                SameText(Field.FieldType.Name, 'LongBool') then
              begin
                AJson.B[FieldName] := TValueData(Field.GetValue(Self)).FAsUByte <> 0;
              end
              else if Field.FieldType is TRttiEnumerationType then
              begin
                AJson.S[FieldName] := GetEnumName(Field.FieldType.Handle, Field.GetValue(Self).AsOrdinal);
              end
              else
                Log(Format('Warning: Enum %s is not supported.', [Field.FieldType.Name]));
            end;
          tkSet:
            begin
              Log(Format('Warning: Set %s is not supported.', [Field.FieldType.Name]));
            end;
          tkClass:
            begin
              if (Pos(LowerCase('TCnJsonPersistentObjectList<'), LowerCase(Field.FieldType.Name)) = 1) and
                Field.GetValue(Self).IsObject then
              begin
                List := TCnJsonPersistentObjectList<TCnJsonPersistent>
                  (Field.GetValue(Self).AsObject);
                if List <> nil then
                begin
                  for i := 0 to List.Count - 1 do
                  begin
                    Obj := List[i];
                    if Obj is TCnJsonPersistent then
                    begin
                      JsObj := TJsonObject.Create;
                      TCnJsonPersistent(Obj).SaveToJsonObject(JsObj);
                      AJson.A[FieldName].Add(JsObj);
                    end;
                  end;
                end;
              end
              else if Field.GetValue(Self).IsObject then
              begin
                Obj := Field.GetValue(Self).AsObject;
                if Obj <> nil then
                begin
                  if Obj is TCnJsonPersistent then
                  begin
                    TCnJsonPersistent(Obj).SaveToJsonObject(AJson.O[FieldName]);
                  end
                  else if Obj is TStringList then
                  begin
                    Log('Is TStringList Type');
                    for i := 0 to TStringList(Obj).Count - 1 do
                    begin
                      AJson.A[FieldName].Add(TStringList(Obj)[i]);
                    end;
                  end
                  else if Obj is TIntegerList then
                  begin
                    Log('Is TIntegerList Type');
                    for i := 0 to TIntegerList(Obj).Count - 1 do
                    begin
                      AJson.A[FieldName].Add(TIntegerList(Obj)[i]);
                    end;
                  end
                  else
                    Log('Warning: Object type is not supported.');
                end
                else
                  Log('Warning: Object is nil.');
              end
           end
          else
            begin
              Log('Warning: not supported.');
            end;
        end;
      except
        on E: Exception do
          Log(Format('Set %s.%s error. %s: %s', [ClassName, Field.Name,
            E.ClassName, E.Message]));
      end;
    end;
    Result := True;
  except
    on E: Exception do
      Log(Format('%s.LoadFromJsonObject error. %s: %s', [ClassName,
        E.ClassName, E.Message]));
  end;
end;

function TCnJsonPersistent.SaveToJsonString(var AJsonString: string; Compact: Boolean): Boolean;
var
  JsonObj: TJsonObject;
begin
  Log('SaveToJsonString');
  Result := False;
  try
    JsonObj := TJsonObject.Create;
    try
      if SaveToJsonObject(JsonObj) then
      begin
        AJsonString := JsonObj.ToJSON(Compact);
        Result := True;
      end;
    finally
      JsonObj.Free;
    end;
  except
    on E: Exception do
      Log(Format('%s.SaveToJsonString error. %s: %s', [ClassName,
        E.ClassName, E.Message]));
  end;
end;

{ TCnJsonPersistentObjectList<T> }

constructor TCnJsonPersistentObjectList<T>.Create;
begin
  inherited Create(True);
  Log('Create');
end;

destructor TCnJsonPersistentObjectList<T>.Destroy;
begin
  Log('Destroy');
  try
    inherited;
  except
    on E: Exception do
      Log(E.ClassName + ': ' + E.Message);
  end;
end;

procedure TCnJsonPersistentObjectList<T>.Log(const Msg: string);
begin
  DbgLog(Format('[%s]%s', [ClassName, Msg]));
end;

function TCnJsonPersistentObjectList<T>.LoadFromFile(
  const AFileName: string): Boolean;
var
  JsonObj: TJsonBaseObject;
begin
  Log('LoadFromFile: ' + AFileName);
  Result := False;
  try
    if FileExists(AFileName) then
    begin
      JsonObj := TJsonObject.ParseFromFile(AFileName);
      try
        if JsonObj is TJsonArray then
        begin
          Result := LoadFromJsonObject(TJsonArray(JsonObj));
        end;
      finally
        JsonObj.Free;
      end;
    end;
  except
    on E: Exception do
      Log(Format('%s.LoadFromFile error. %s: %s', [ClassName,
        E.ClassName, E.Message]));
  end;
end;

function TCnJsonPersistentObjectList<T>.LoadFromJsonObject(
  AJson: TJsonArray): Boolean;
var
  Obj: TCnJsonPersistent;
  i: Integer;
begin
  Log('LoadFromJsonObject');
  Result := False;
  try
    Clear;
    if AJson = nil then
      Exit;
    json := AJson.ToJSON;
    for i := 0 to AJson.Count - 1 do
    begin
      Log(Format('Create New %s Object. Index: %d', [T.ClassName, i]));
      Obj := TCnJsonPersistentClass(T).Create;
      Obj.LoadFromJsonObject(AJson.O[i]);
      Add(Obj);
    end;
    Result := True;
  except
    on E: Exception do
      Log(Format('%s.LoadFromJsonObject error. %s: %s', [ClassName,
        E.ClassName, E.Message]));
  end;
end;

function TCnJsonPersistentObjectList<T>.LoadFromJsonString(
  const AJsonString: string): Boolean;
var
  JsonObj: TJsonBaseObject;
begin
  Log('LoadFromJsonString: ' + AJsonString);
  Result := False;
  try
    if AJsonString <> '' then
    begin
      JsonObj := TJsonObject.Parse(AJsonString);
      try
        if JsonObj is TJsonArray then
        begin
          Result := LoadFromJsonObject(TJsonArray(JsonObj));
        end;
      finally
        JsonObj.Free;
      end;
    end;
  except
    on E: Exception do
      Log(Format('%s.LoadFromJsonString error. %s: %s', [ClassName,
        E.ClassName, E.Message]));
  end;
end;

function TCnJsonPersistentObjectList<T>.SaveToFile(const AFileName: string;
  Compact: Boolean): Boolean;
var
  JsonObj: TJsonArray;
begin
  Log('SaveToFile: ' + AFileName);
  Result := False;
  try
    JsonObj := TJsonArray.Create;
    try
      if SaveToJsonObject(JsonObj) then
      begin
        JsonObj.SaveToFile(AFileName, Compact);
        Result := True;
      end;
    finally
      JsonObj.Free;
    end;
  except
    on E: Exception do
      Log(Format('%s.SaveToFile error. %s: %s', [ClassName,
        E.ClassName, E.Message]));
  end;
end;

function TCnJsonPersistentObjectList<T>.SaveToJsonObject(
  AJson: TJsonArray): Boolean;
var
  Obj: TCnJsonPersistent;
  JsObj: TJsonObject;
  i: Integer;
begin
  Log('SaveToJsonObject');
  Result := False;
  try
    if AJson = nil then
      Exit;
    for i := 0 to Count - 1 do
    begin
      JsObj := TJsonObject.Create;
      Items[i].SaveToJsonObject(JsObj);
      AJson.Add(JsObj);
    end;
    Result := True;
  except
    on E: Exception do
      Log(Format('%s.SaveToJsonObject error. %s: %s', [ClassName,
        E.ClassName, E.Message]));
  end;
end;

function TCnJsonPersistentObjectList<T>.SaveToJsonString(
  var AJsonString: string; Compact: Boolean): Boolean;
var
  JsonObj: TJsonArray;
begin
  Log('SaveToJsonString');
  Result := False;
  try
    JsonObj := TJsonArray.Create;
    try
      if SaveToJsonObject(JsonObj) then
      begin
        AJsonString := JsonObj.ToJSON(Compact);
        Result := True;
      end;
    finally
      JsonObj.Free;
    end;
  except
    on E: Exception do
      Log(Format('%s.SaveToJsonString error. %s: %s', [ClassName,
        E.ClassName, E.Message]));
  end;
end;

initialization
  JsonSerializationConfig.NullConvertsToValueTypes := True;
{$IFDEF Android}
  CnJsonPersistentEnableDbgLog := FileExists('/sdcard/jsdbg.db');
{$ENDIF}
{$IFDEF IOS}
  CnJsonPersistentEnableDbgLog := FileExists(TPath.GetDocumentsPath + '/jsdbg.db');
{$ENDIF}
{$IFDEF MSWINDOWS}
  CnJsonPersistentEnableDbgLog := FileExists(ExtractFilePath(ParamStr(0)) + '\jsdbg.db');
{$ENDIF}
{$IFDEF DEBUG}
  //CnJsonPersistentEnableDbgLog := True;
{$ENDIF}

{$ENDIF}

end.

