{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2017 CnPack 开发组                       }
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

unit CnVarList;
{* |<PRE>
================================================================================
* 软件名称：开发包基础库
* 单元名称：变体列表类
* 单元作者：早安空气（zzzl）
* 移    植：Chide Ng
*           Liu Xiao
* 备    注：该单元实现了一变体列表类（类似于CList），能将各种数据类型或对象实例
            添加入列表中，并能转换为字符串，反之也支持从字符串中还原列表内容，
            其中还原操作不支持对象类型。
* 开发平台：PWin2K SP3 + Delphi 7
* 兼容测试：PWin9X/2000/XP + Delphi 6/7 C++Builder 6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2008.05.16
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Windows, Classes, Math{$IFDEF COMPILER6_UP}, Variants, StrUtils{$ENDIF};

type
  TCnVars = array of Variant;

  TCnVarList = class(TObject)
  private
    FValues: TCnVars;
    FValType: TCnVars;
    function GetCount: Integer;
    function Get(Index: Integer): Variant;
  protected
    function GetString(cList : TCnVarList): WideString;
    procedure DynArrayDelete(var A; elSize: Longint; Index, Count: Integer); virtual;
    function GetList(Index: Integer): TCnVarList;
    {* 如果第 I 个是 CnVarList，则返回此封装的 List, 外部使用完毕后需释放。}
  public
    constructor Create; virtual;
    destructor Destroy; override;

    procedure SetValues(const AValues: array of Variant);
    procedure Clear;
    procedure Add(AValue: Variant); overload;
    {* 将一个变体类型添加到列表中，允许是另外一个 VarArray}
    procedure Add(AValue: TCnVarList); overload;
    {* 将一个变体列表添加到列表中，实质上是将另外一个变体列表中的 Values 添加进来}
    procedure Add(AValue: TObject); overload;
    {* 添加一普通对象，注意对象无法从字符串还原}
    procedure Remove(Index: Integer);
    {* 删除 Index 位置所指的值}
    function GetType(Index: Integer): string;
    {* 获得列表某位置的变体的对应类型的字符串}
    function GetObject(Index: Integer): TObject;
    {* 获得列表某位置的对象实例，如果不是对象则返回 nil}
    function ToString: WideString;
    {* 将变体列表转换成字符串，允许嵌套}
    function FromString(Text: WideString; var Error: string): Boolean;
    {* 从字符串中恢复变体列表，如格式不对则返回 False，出错信息在 Error 中}
    
    property Count: Integer read GetCount;
    {* 列表元素数量}
    property Items[Index: Integer]: Variant read Get;
    {* 根据 Index 获得变体值}
    property Values: TCnVars read FValues write FValues;
    {* 变体数组}
    property ValType: TCnVars read FValType write FValType;
    {* 变体数组类型名}
  end;

implementation

{$IFDEF DEBUG}
uses
  CnDebug;
{$ENDIF}

{$IFNDEF COMPILER6_UP}
type
  TVarType = Word;

function TryStrToInt(const S: string; out Value: Integer): Boolean;
var
  E: Integer;
begin
  Val(S, Value, E);
  Result := E = 0;
end;

function StrToBool(const S: string): Boolean;
begin
  if UpperCase(S) = 'TRUE' then
    Result := True
  else if UpperCase(S) = 'FALSE' then
    Result := False
  else
    raise EConvertError.CreateFmt('Invalid Boolean Value: %s', [S]);
end;

function AnsiDequotedStr(const S: string; AQuote: Char): string;
var
  LText: PChar;
begin
  LText := PChar(S);
  Result := AnsiExtractQuotedStr(LText, AQuote);
  if Result = '' then
    Result := S;
end;

const
  cSimpleBoolStrs: array [Boolean] of string = ('False', 'True');

function BoolToStr(B: Boolean; UseBoolStrs: Boolean): string;
begin
  Result := cSimpleBoolStrs[B];
end;

function VarTypeAsText(const AType: TVarType): string;
const
  CText: array [varEmpty..varByte] of string = ('Empty', 'Null', 'Smallint', //Do not localize
    'Integer', 'Single', 'Double', 'Currency', 'Date', 'OleStr', 'Dispatch', //Do not localize
    'Error', 'Boolean', 'Variant', 'Unknown', 'Decimal', '$0F', 'ShortInt', //Do not localize
    'Byte'); //Do not localize
begin
  if AType and varTypeMask <= varByte then
    Result := CText[AType and varTypeMask]
  else if AType = varString then
    Result := 'String' //Do not localize
{$IFDEF UNICODE}
  else if AType = varUString then
    Result := 'UString' //Do not localize
{$ENDIF}
  else if AType = varAny then
    Result := 'Any' //Do not localize
  else
    Result := HexDisplayPrefix + IntToHex(AType and varTypeMask, 4);

  if AType and varArray <> 0 then
    Result := 'Array ' + Result; //Do not localize
  if AType and varByRef <> 0 then
    Result := 'ByRef ' + Result; //Do not localize
end;

{$ENDIF}

var
  FVarList: TList = nil;

{ TCnVarList }

procedure TCnVarList.Add(AValue: TCnVarList);
begin
  if (AValue = nil) or (AValue = Self) then Exit;
  SetLength(FValues, Length(FValues) + 1);
  SetLength(FValType, Length(FValType) + 1);
  FValues[High(FValues)] := AValue.FValues;
  FValType[High(FValType)] := AValue.FValType;
end;

procedure TCnVarList.Add(AValue: Variant);
begin
  SetLength(FValues, Length(FValues)+ 1);
  SetLength(FValType, Length(FValType)+ 1);
  FValues[High(FValues)] := AValue;
  FValType[High(FValType)] := VarTypeAsText(VarType(AValue));
end;

procedure TCnVarList.Add(AValue: TObject);
begin
  if not Assigned(Avalue) then Exit;
  SetLength(FValues, Length(FValues)+ 1);
  SetLength(FValType, Length(FValType)+ 1);
  FValues[High(FValues)] := Integer(AValue);
  FValType[High(FValType)] := 'Object:' + AValue.ClassName;
end;

procedure TCnVarList.Clear;
begin
  SetLength(FValues, 0);
  SetLength(FValType, 0);
end;

{
  A 变量类型，elSize = SizeOf(A) Index 开始删除的位置索引 ，Count 删除的数量
}
constructor TCnVarList.Create;
begin
  FVarList.Add(Self);
end;

destructor TCnVarList.Destroy;
begin
  FVarList.Remove(Self);
  inherited;
end;

procedure TCnVarList.DynArrayDelete(var A; elSize, Index, Count: Integer);
var
  Len, MaxDelete: Integer;
  P : PLongint;
begin
  P := PLongint(A);
  if P = nil then
   Exit;
{ 下面这句完全等同于 Dec(P) ; Len := P^
  因为 Dec(P) = Pchar(P) – 4  同样是移动4 字节的偏移量，只不过后者按字节来移动    }
  Len := PLongint(PChar(P) - 4)^; // 变量的长度 ，偏移量 -4
  if Index >= Len then //要删除的位置超出范围，退出
   Exit;

  MaxDelete := Len - Index; // 最多删除的数量
  Count := Min(Count, MaxDelete); // 取得一个较小值
  if Count = 0 then  Exit;
  Dec(Len, Count);// 移动到要删除的位置
  MoveMemory(PChar(P)+ Index * elSize,
             PChar(P)+ (Index + Count) * elSize ,
             (Len- Index) * elSize); //移动内存
  Dec(P);  //移出 “数组长度”位置
  Dec(P);  //移出“引用计数” 位置
  //重新再分配调整内存,Len 新的长度. Sizeof(Longint) * 2 = 2*Dec(P)
  ReallocMem(P, Len * elSize + Sizeof(Longint) * 2);
  Inc(P); // 指向数组长度
  P^ := Len; // new length
  Inc(P); // 指向数组元素，开始的位置
  PLongint(A) := P;
end;

type
  TCharState = (csUnknown,
                csBracketLeft,
                csBracketRight,  //左右括号
                csComma,
                csSq,        //单引号
                csInteger, csString, csBoolean);

function TCnVarList.FromString(Text: WideString;
  var Error: string): Boolean;
  
  function ParseText(Text: WideString): TCnVars;
  var
    I: Integer;
    State: TCharState;
    Num: Integer;
    Element: WideString;
    eType: TVarType;
    C: WideChar;
    sqCount : Integer;
    //每个元素表示为一个层。第0个元素为self,第1个为self下的第一层子List
    List : TList;
    cvl : TCnVarList;

    procedure _PushElement(); //将Element装入当前List中，并清空Element
    var
      v: Variant;
    begin
      case eType of
        varString {$IFDEF UNICODE}, varUString {$ENDIF}:  v := AnsiDequotedStr(Element, '''');
        varInteger: v := StrToInt(Element);
        varDouble:  v := StrToFloat(Element);
        varBoolean: v := StrToBool(Element);
      end;

      if List.Count > 0 then
      begin
        TCnVarList(List[List.Count - 1]).Add(v);
{$IFDEF DEBUG}
        CnDebugger.LogFmt('Add Value %d VarList: %s.', [List.Count - 1,
          TCnVarList(List[List.Count - 1]).ToString]);
{$ENDIF}
      end;
      Element := '';
    end;

    procedure _NewLayer(); 
    begin
      cvl := TCnVarList.Create;
      List.Add(cvl);
    end;

    procedure _PopLayer();
    var
      L : SmallInt;
    begin
      L := List.Count - 1;
      if L <= 0 then Exit;
{$IFDEF DEBUG}
      CnDebugger.LogFmt('The %d VarList Before Add: %s.', [L - 1, TCnVarList(List[L - 1]).ToString]);
      CnDebugger.LogFmt('The %d VarList Before Add: %s.', [L, TCnVarList(List[L]).ToString]);
{$ENDIF}
      TCnVarList(List[L - 1]).Add(TCnVarList(List[L]));
{$IFDEF DEBUG}
      CnDebugger.LogFmt('The %d VarList After Add: %s.', [L - 1, TCnVarList(List[L - 1]).ToString]);
{$ENDIF}
      TCnVarList(List[L]).Free;
{$IFDEF DEBUG}
      CnDebugger.LogFmt('The %d VarList After Free: %s.', [L - 1, TCnVarList(List[L - 1]).ToString]);
{$ENDIF}
      List.Delete(L);
    end;

    procedure _WantNewElement(); //从Text的I位置开始，认为是一个新的元素开始处理
    begin
      if C = '''' then
      begin
        Element := '''';
        eType := varString;
        State := csSq;
      end
      else if TryStrToInt(C, Num) then
      begin
        Element := C;
        eType := varInteger;
        State := csInteger;
      end
      else if (C = 't') or (C = 'T') or (C = 'f') or (C = 'F') then
      begin
        Element := C;
        eType := varBoolean;
        State := csBoolean;
      end
      else if C = '(' then
        _NewLayer;
    end;
  begin
    State := csUnknown;
    sqCount := 0;
    for I := 1 to Length(Text) do
    begin
      C := Text[I];
      case State of
        csUnknown:
          begin
            if C = '(' then
            begin // 找到起始符
              State := csBracketLeft;
              List := TList.Create;
              List.Add(Self);
            end;
          end;
        csBracketLeft:
          begin
            if C = ')' then
            begin
              if Element <> '' then
                _PushElement;

              if List.Count > 0 then
                _PopLayer;

              State := csBracketRight;
              end
            else
              if C = '(' then
              begin
                _NewLayer;
              end
              else
                _WantNewElement;
          end;
        csBracketRight:
          begin
            if Element <> '' then
              _PushElement;
            if List.Count > 0 then
                _PopLayer;
            if C = ',' then
              State := csComma
            else
              _WantNewElement;
            end;
        csComma:
          begin
            if Element <> '' then
              _PushElement;
            _WantNewElement;
          end;
        csSq:
          begin
            Element := Element + C;
            State := csString;
          end;
        csString:
          begin
            if C = ')' then
            begin
              if List.Count > 0 then
                _PopLayer;
              State := csBracketRight;
            end
            else if C = ',' then
              if sqCount mod 2 = 0 then
              begin
                State := csComma;
                sqCount := 0;
              end
              else
                Element := Element + C
            else
              Element := Element + C
          end;
        csInteger:
          begin
            if C = ',' then
              State := csComma
            else if C = ')' then
            begin
              if Element <> '' then
                _PushElement;
              if List.Count > 0 then
                _PopLayer;
              State := csBracketRight;
            end
            else if C = '.' then
            begin
              Element := Element + '.';
              eType := varDouble;
              end
            else if TryStrToInt(C, Num) then
            begin
              Element := Element + C;
            end
          end;
        csBoolean:
          begin
            if C = ')' then
            begin
              _PushElement;
              if List.Count > 0 then
                _PopLayer;
              State := csBracketRight;
            end
            else if C = ',' then
              State := csComma
            else
              Element := Element + C;
          end;
        end;
      end;
  end;
begin
  Clear;
  try
    ParseText(Text);
    Result := True;
  except
    on E: Exception do
    begin
      Error := E.Message;
      Result := False;
    end;
  end;
end;

function TCnVarList.Get(Index: Integer): Variant;
begin
  Result := FValues[Index];
end;

function TCnVarList.GetCount: Integer;
begin
  Result := Length(FValues);
end;

function TCnVarList.GetList(Index: Integer): TCnVarList;
var
  I: Integer;
  vVal, vType: Variant;
begin
  vVal := FValues[Index];
  if VarIsArray(vVal) then
  begin
    Result := TCnVarList.Create;
    vType := FValType[Index];
    SetLength(Result.FValues, VarArrayHighBound(vVal, 1) + 1);
    SetLength(Result.FValType, VarArrayHighBound(vType, 1) + 1);
    for I := VarArrayLowBound(vVal, 1) to VarArrayHighBound(vVal, 1) do
    begin
      Result.FValues[I] := vVal[I];
      Result.FValType[I] := vType[I];
    end;
  end
  else
    Result := nil;
end;

function TCnVarList.GetObject(Index: Integer): TObject;
var
  v: Variant;
begin
  Result := nil;      
  v := FValues[Index];
  if VarIsArray(v) then
    Exit;
  if Pos('Object', FValType[Index]) = 1 then
    Result := TObject(Integer(v));
end;

function TCnVarList.GetString(cList: TCnVarList): WideString;
var
  I: Integer;
  v: Variant;
  aList: TCnVarList;
begin
  Result := '';
  if not Assigned(cList) then Exit;
  Result := '(';
  for I := 0 to High(cList.Values) do
  begin
    v := cList.Values[I];
    if VarArrayDimCount(v) > 0 then
    begin
      // 如果元素本身就是vararray，表明是CnVarList来的，则递归调用扩展之。
      aList := cList.GetList(I);
      Result := Result + GetString(aList);
      FreeAndNil(aList);
    end
    else
    begin
      if Pos('Object', cList.ValType[I]) = 1 then
        Result := Result + QuotedStr(cList.ValType[I])
      else
        case VarType(v) of
          varString, {$IFDEF UNICODE} varUString, {$ENDIF} varOleStr:
            Result := Result + QuotedStr(v);
          varByte, {$IFDEF COMPILER6_UP}varShortInt,{$ENDIF} varSmallint,
          varInteger, varSingle, varDouble,
          {$IFDEF COMPILER6_UP}varWord, varLongWord, varInt64, {$ENDIF}
          varCurrency:
            Result := Result + VarToStr(v);
          varDate:
            Result := Result + '''' + DateTimeToStr(VarToDateTime(v)) + '''';
          varBoolean:
            Result := Result + BoolToStr(v, True);
          varVariant:
            Result := Result + QuotedStr(v);
        else
          Result := Result + 'Unknown:' + cList.ValType[I];
        end;
    end;
    if I < High(cList.Values) then
      Result := Result + ',';
  end;
  Result := Result + ')';
end;

function TCnVarList.GetType(Index: Integer): string;
begin
  Result := FValType[Index];
end;

procedure TCnVarList.Remove(Index: Integer);
begin
  DynArrayDelete(FValues, Length(FValues), Index, 1);
  DynArrayDelete(FValType, Length(FValType), Index, 1);
end;

procedure TCnVarList.SetValues(const AValues: array of Variant);
var
  I: Integer;
begin
  SetLength(FValues, Length(AValues));
  SetLength(FValType, Length(AValues));
  for I := Low(AValues) to High(AValues) do
  begin
    FValues[I] := AValues[I];
    FValType[I] := VarTypeAsText(VarType(AValues[I]));
  end;
end;

function TCnVarList.ToString: WideString;
begin
  Result := GetString(Self);
end;

procedure CleanVarList;
var
  I: Integer;
begin
  if FVarList <> nil then
    for I := 0 to FVarList.Count - 1 do
      TCnVarList(FVarList[I]).Free;
end;

initialization
  FVarList := TList.Create;

finalization
  CleanVarList;
  FreeAndNil(FVarList);

end.
