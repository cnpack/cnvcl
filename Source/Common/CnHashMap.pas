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

unit CnHashMap;
{* |<PRE>
================================================================================
* 软件名称：CnPack
* 单元名称：CnHashMap 实现单元
* 单元作者：Pan Ying
* 备    注：该单元为 CnHashMap 的实现单元。
* 开发平台：PWin2000Pro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：v0.96   2004/2/7  by beta
*               Add new class TCnStrToPtrHashMap
*           v0.95   2002/8/3  by Pan Ying
*               Add support for custom defined hash code method
*               Add New Class TCnStrToStrHashMap
*           v0.91   2002/7/28 by Pan Ying
*               Add new hash code method interface
*               Add private member FLengthBit and some support method
*               Now change Incr Length Method.
*           v0.90   2002/7/14 by Pan Ying
*               Just write the TCnBaseHashMap.
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Classes, SysUtils;

type

  ECnHashException = class(Exception)
  end;

  {     When an EOutOfMemory exception throw,means there is not enough
  Memory to keep the map,just maybe need more memory.}


  {    This Record is used as the internal method to store data,
    it use variant so can support int,string,object and so on.}
  TCnHashMapRec = record
    Key: Variant;
    HashCode: Integer;
            //when -2 ,as nothing;when -1, as deleted;
    Value: Variant;
  end;

  {Some type used to calculate the int hash code}
  TCnHashCodeType = (CnHashMove, CnHashMod);

  {Your can define your own function to calculate hash code,
    but sure it need less than the list Length}
  TCnCustomHashCodeMethod = function(AKey: Variant; AListLength, ATotalRec: Integer): Integer;

  {    This is the loew level hash map ,all others' ancestor.
     Some method just abstract.'}
  TCnBaseHashMap = class(TPersistent)
  private
    FIncr: Integer;
            //how much space should be alloc when full
    FSize: Integer;
            //how many data stored.
    FCurPos: Integer;
            //enum point
    FLengthBit: Integer;

    FHashCodeMethod: TCnHashCodeType;
    FUseCustomHash: Boolean;
    FOnCustomHashCode: TCnCustomHashCodeMethod;
            //the type

    procedure SetIncr(Value: Integer);
    procedure CreateList(Length: Integer);

    function VariantHashCode(AKey: Variant): Integer; virtual;
            //ATTENTION: override in every inherit
    function IntHashCode(AKey: Integer): Integer;

    procedure DeleteValue(AValue: Variant); virtual;
            //this procedure used to delete no use value,may be needed override

    function Search(AKey: Variant): Integer;
    procedure SetHashCodeMethod(const Value: TCnHashCodeType);
    procedure SetUseCustomHash(const Value: Boolean);
    procedure SetOnCustomHashCode(const Value: TCnCustomHashCodeMethod);
  protected
    FList: array of TCnHashMapRec;
            //store the map
    procedure ReSizeList(NewLength: Integer);
            //use to change the internal list's size

    procedure AddInternal(AKey, AValue: Variant);
    function DeleteInternal(AKey: Variant): Boolean;
    function FindInternal(AKey: Variant; var AValue: Variant): Boolean;

    function GetNextInternal(var AKey, AValue: Variant): Boolean;

    function HasHashCode(AKey: Variant): Integer;
  public
    constructor Create(AListLength: Integer = 8; AIncr: Integer = 2);
    destructor Destroy; override;

    procedure Add(AKey, AValue: Variant); overload; virtual;
    function Delete(AKey: Variant): Boolean; overload; virtual;
    function Find(AKey: Variant; var AValue: Variant): Boolean; overload; virtual;

    procedure Refresh;
            //use if many data has been deleted,takes a long time

    //use to list all key and value
    procedure StartEnum;
    function GetNext(var AKey, AValue: Variant): Boolean; overload; virtual;
            //return false when all hava been listed

    property Incr: Integer read FIncr write SetIncr;
    property Size: Integer read FSize;
    property HashCodeMethod: TCnHashCodeType read FHashCodeMethod write SetHashCodeMethod;
    property UseCustomHash: Boolean read FUseCustomHash write SetUseCustomHash;
    property OnCustomHashCode: TCnCustomHashCodeMethod read FOnCustomHashCode write SetOnCustomHashCode;

  end;

  TCnStrToStrHashMap = class(TCnBaseHashMap)
  private
    function VariantHashCode(AKey: Variant): Integer; override;

  public
    procedure Add(const AKey, AValue: string); reintroduce; overload;
    function Delete(const AKey: string): Boolean; reintroduce; overload;
    function Find(const AKey: string; var AValue: string): Boolean; reintroduce; overload;

    function GetNext(var AKey, AValue: string): Boolean; reintroduce; overload;
  end;

  TCnWideStrToWideStrHashMap = class(TCnBaseHashMap)
  private
    function VariantHashCode(AKey: Variant): Integer; override;

  public
    procedure Add(const AKey, AValue: WideString); reintroduce; overload;
    function Delete(const AKey: WideString): Boolean; reintroduce; overload;
    function Find(const AKey: WideString; var AValue: WideString): Boolean; reintroduce; overload;

    function GetNext(var AKey, AValue: WideString): Boolean; reintroduce; overload;
  end;

  TCnStrToPtrHashMap = class(TCnBaseHashMap)
  private
    function VariantHashCode(AKey: Variant): Integer; override;
  public
    procedure Add(const AKey: string; AValue: Pointer); reintroduce; overload;
    function Delete(const AKey: string): Boolean; reintroduce; overload;
    function Find(const AKey: string; var AValue: Pointer): Boolean; reintroduce; overload;
    function GetNext(var AKey: string; var AValue: Pointer): Boolean; reintroduce; overload;
  end;

  TCnStrToVariantHashMap = class(TCnBaseHashMap)
  private
    function VariantHashCode(AKey: Variant): Integer; override;
  public
    procedure Add(const AKey: string; AValue: Variant); reintroduce; overload;
    function Delete(const AKey: string): Boolean; reintroduce; overload;
    function Find(const AKey: string; var AValue: Variant): Boolean; reintroduce; overload;
    function GetNext(var AKey: string; var AValue: Variant): Boolean; reintroduce; overload;
  end;

implementation

{ TCnBaseHashMap }

procedure TCnBaseHashMap.Add(AKey, AValue: Variant);
begin
  AddInternal(AKey, AValue);
end;

procedure TCnBaseHashMap.AddInternal(AKey, AValue: Variant);
var
  i, j: Integer;
  Pos, DeletedPos: Integer;
begin
  //if smaller,then enlarge the size
  if Size >= Length(FList) then
    ReSizeList(Size * Incr);

  //calculate hash code
  i := HasHashCode(AKey);
  DeletedPos := -1;
  Pos := i;

  for j := Low(FList) to High(FList) do
  begin
    Pos := (i + j) mod Length(FList);

    if FList[Pos].HashCode = -2 then
      Break
    else if (FList[Pos].HashCode = i) and (FList[Pos].Key = AKey) then
      Break;

    if (DeletedPos < 0) and (FList[Pos].HashCode = -1) then
      DeletedPos := Pos;
  end;

  if (FList[Pos].HashCode = -2) or
    ((FList[Pos].HashCode = i) and (FList[Pos].Key = AKey)) then //new record
  begin
    if (FList[Pos].HashCode = i) and (FList[Pos].Key = AKey) then
      DeleteValue(FList[Pos].Value)
    else
      Inc(FSize);

    FList[Pos].Key := AKey;
    FList[Pos].HashCode := i;

    FList[Pos].Value := AValue;
  end
  else if DeletedPos >= 0 then
  begin
    Pos := DeletedPos;

    FList[Pos].Key := AKey;
    FList[Pos].HashCode := i;
    FList[Pos].Value := AValue;

    Inc(FSize);
  end;
end;

constructor TCnBaseHashMap.Create(AListLength, AIncr: Integer);
begin
  inherited Create;

  // set All the Initial value first here.
  FIncr := 15;
  FSize := 0;

  Incr := AIncr;

  FHashCodeMethod := CnHashMod;

  FOnCustomHashCode := nil;
  FUseCustomHash := false;

  CreateList(AListLength);
end;

procedure TCnBaseHashMap.CreateList(Length: Integer);
var
  i: Integer;
  nTemp: Integer;
begin
  FSize := 0;
  SetLength(FList, Length);

  for i := Low(FList) to High(FList) do
    FList[i].HashCode := -2; //just think -2 is space

  FLengthBit := 1;
  nTemp := 2;

  while nTemp < Length do
  begin
    nTemp := nTemp * 2;

    Inc(FLengthBit);
  end;
end;

function TCnBaseHashMap.Delete(AKey: Variant): Boolean;
begin
  Result := DeleteInternal(AKey);
end;

function TCnBaseHashMap.DeleteInternal(AKey: Variant): Boolean;
var
  Pos: Integer;
begin
  Pos := Search(AKey);

  if Pos = -1 then
    Result := false
  else
  begin
    FList[Pos].HashCode := -1; //deleted
    DeleteValue(FList[Pos].Value);

    dec(FSize);

    Result := true;
  end;
end;

procedure TCnBaseHashMap.DeleteValue(AValue: Variant);
begin
  //just donothing here;
end;

destructor TCnBaseHashMap.Destroy;
var
  i: Integer;
begin
  for i := Low(FList) to High(FList) do
    if FList[i].HashCode >= 0 then
      DeleteValue(FList[i].Value);

  SetLength(FList, 0);

  inherited;
end;

function TCnBaseHashMap.Find(AKey: Variant; var AValue: Variant): Boolean;
begin
  Result := FindInternal(AKey, AValue);
end;

function TCnBaseHashMap.FindInternal(AKey: Variant; var AValue: Variant): Boolean;
var
  Pos: Integer;
begin
  Pos := Search(AKey);

  if Pos = -1 then
    Result := false
  else
  begin
    AValue := FList[Pos].Value;

    Result := true;
  end;
end;

function TCnBaseHashMap.GetNext(var AKey, AValue: Variant): Boolean;
begin
  Result := GetNextInternal(AKey, AValue);
end;

function TCnBaseHashMap.GetNextInternal(var AKey,
  AValue: Variant): Boolean;
var
  i: Integer;
begin
  i := FCurPos + 1;

  while (i < Length(FList)) and (FList[i].HashCode < 0) do
    Inc(i);

  if i >= Length(FList) then
    Result := false
  else
  begin
    FCurPos := i;
    AKey := FList[i].Key;
    AValue := FList[i].Value;

    Result := true;
  end;
end;

function TCnBaseHashMap.HasHashCode(AKey: Variant): Integer;
begin
  if UseCustomHash then
    Result := OnCustomHashCode(AKey, Length(FList), Size)
  else
    Result := IntHashCode(VariantHashCode(AKey));
end;

function TCnBaseHashMap.IntHashCode(AKey: Integer): Integer;
var
  nTemp, nTemp2, nTemp3: Integer;
begin
  {ATTENTION: New Hash Code Method add here}

  case (HashCodeMethod) of
    CnHashMove:
      begin
        nTemp := Abs(AKey);
        nTemp2 := 0;
        nTemp3 := 1 shl FLengthBit;

        while (nTemp > 0) do
        begin
          Inc(nTemp2, nTemp mod nTemp3);

          nTemp := nTemp shr FLengthBit;
        end;

        Result := nTemp2;
      end;


    CnHashMod:
      Result := AKey mod Length(FList);

  else
    //we treat as the Mod Method
    Result := AKey mod Length(FList);
  end;

  Result := Abs(Result);
end;

procedure TCnBaseHashMap.Refresh;
var
  nNewLen: Integer;
begin
  nNewLen := Length(FList);

  while nNewLen > Size do nNewLen := nNewLen div Incr;

  if nNewLen <= 0 then
    nNewLen := Incr;

  while nNewLen <= Size do nNewLen := nNewLen * Incr;

  ReSizeList(nNewLen);
end;

procedure TCnBaseHashMap.ReSizeList(NewLength: Integer);
var
  TempList: array of TCnHashMapRec;
  i: Integer;
begin
  //this is a protected procedure,not directly called outside

  //first we check the NewLength is valid
  if (NewLength < Size) then
    raise ECnHashException.Create('New list size is not valid');

  //then we do the actual act,this will take a long time if list is long
  SetLength(TempList, Length(FList));

  try

    for i := Low(TempList) to High(TempList) do
      TempList[i] := FList[i];

    CreateList(NewLength);

    for i := Low(TempList) to High(TempList) do
      if TempList[i].HashCode >= 0 then
        AddInternal(TempList[i].Key, TempList[i].Value);

  finally
    SetLength(TempList, 0);
  end;

end;

function TCnBaseHashMap.Search(AKey: Variant): Integer;
var
  i, j: Integer;
  Pos: Integer;
begin
  Result := -1;

  //calculate hash code first
  i := HasHashCode(AKey);

  for j := Low(FList) to High(FList) do
  begin
    Pos := (i + j) mod Length(FList);

    if FList[Pos].HashCode = -2 then
      Break
    else if (FList[Pos].HashCode = i) and (FList[Pos].Key = AKey) then
    begin
      Result := Pos;
      Break;
    end;
  end;
end;

procedure TCnBaseHashMap.SetHashCodeMethod(const Value: TCnHashCodeType);
begin
  if (FHashCodeMethod <> Value) then
  begin
    //we should refresh this list,because hash code has been changed also
    FHashCodeMethod := Value;
    Refresh;
  end;
end;

procedure TCnBaseHashMap.SetIncr(Value: Integer);
begin
  if (Value <= 1) then
    raise ECnHashException.Create('Incr should be lagerer than 1')
  else
    if (Value <> FIncr) then
      FIncr := Value;
end;

procedure TCnBaseHashMap.SetOnCustomHashCode(
  const Value: TCnCustomHashCodeMethod);
begin
  if Assigned(Value) then
  begin
    FOnCustomHashCode := Value;
    if UseCustomHash then
      Refresh;
  end
  else
  //close  UseCustomHash
  begin
    FOnCustomHashCode := Value;
    UseCustomHash := false;
  end;
end;

procedure TCnBaseHashMap.SetUseCustomHash(const Value: Boolean);
begin
  if (Value <> FUseCustomHash) then
    if not (Value) then
    begin
      FUseCustomHash := Value;

      Refresh;
    end
    else
      if Assigned(OnCustomHashCode) then
      begin
        FUseCustomHash := Value;

        Refresh;
      end;
end;

procedure TCnBaseHashMap.StartEnum;
begin
  FCurPos := -1;
end;

function TCnBaseHashMap.VariantHashCode(AKey: Variant): Integer;
begin
  //here is just a example
  //u should change it when it's a string or an object
  Result := Integer(AKey);
end;

{ TCnStrToStrHashMap }

procedure TCnStrToStrHashMap.Add(const AKey, AValue: string);
begin
  AddInternal(AKey, AValue);
end;

function TCnStrToStrHashMap.Delete(const AKey: string): Boolean;
begin
  Result := DeleteInternal(AKey);
end;

function TCnStrToStrHashMap.Find(const AKey: string;
  var AValue: string): Boolean;
var
  myValue: Variant;
begin
  Result := FindInternal(Variant(AKey), myValue);

  if Result then
    AValue := myValue;
end;

function TCnStrToStrHashMap.GetNext(var AKey, AValue: string): Boolean;
var
  myKey, myValue: Variant;
begin
  Result := GetNextInternal(myKey, myValue);

  if Result then
  begin
    AKey := myKey;
    AValue := myValue;
  end;
end;

function TCnStrToStrHashMap.VariantHashCode(AKey: Variant): Integer;
var
  myHashCode, i: Integer;
  HashString: string;
begin
  myHashCode := 0;
  HashString := AKey;

  for i := 1 to Length(HashString) do
    myHashCode := myHashCode shl 5 + ord(HashString[i]) + myHashCode;

  Result := Abs(myHashCode);
end;

{ TCnWideStrToWideStrHashMap }

procedure TCnWideStrToWideStrHashMap.Add(const AKey, AValue: WideString);
begin
  AddInternal(AKey, AValue);
end;

function TCnWideStrToWideStrHashMap.Delete(const AKey: WideString): Boolean;
begin
  Result := DeleteInternal(AKey);
end;

function TCnWideStrToWideStrHashMap.Find(const AKey: WideString;
  var AValue: WideString): Boolean;
var
  myValue: Variant;
begin
  Result := FindInternal(Variant(AKey), myValue);

  if Result then
    AValue := myValue;
end;

function TCnWideStrToWideStrHashMap.GetNext(var AKey, AValue: WideString): Boolean;
var
  myKey, myValue: Variant;
begin
  Result := GetNextInternal(myKey, myValue);

  if Result then
  begin
    AKey := myKey;
    AValue := myValue;
  end;
end;

function TCnWideStrToWideStrHashMap.VariantHashCode(AKey: Variant): Integer;
var
  myHashCode, i: Integer;
  HashString: WideString;
begin
  myHashCode := 0;
  HashString := AKey;

  for i := 1 to Length(HashString) do
    myHashCode := myHashCode shl 5 + ord(HashString[i]) + myHashCode;

  Result := Abs(myHashCode);
end;

{ TCnShortStrToPtrHashMap }

function TCnStrToPtrHashMap.VariantHashCode(AKey: Variant): Integer;
var
  iHashCode, i: Integer;
  HashString: string;
begin
  iHashCode := 0;
  HashString := AKey;

  for i := 1 to Length(HashString) do
    iHashCode := iHashCode shl 5 + Ord(HashString[i]) + iHashCode;

  Result := Abs(iHashCode);
end;

procedure TCnStrToPtrHashMap.Add(const AKey: string; AValue: Pointer);
begin
  AddInternal(AKey, Integer(AValue));
end;

function TCnStrToPtrHashMap.Delete(const AKey: string): Boolean;
begin
  Result := DeleteInternal(AKey);
end;

function TCnStrToPtrHashMap.Find(const AKey: string; var AValue: Pointer): Boolean;
var
  vValue: Variant;
begin
  Result := FindInternal(Variant(AKey), vValue);

  if Result then
    AValue := Pointer(Integer(vValue));
end;

function TCnStrToPtrHashMap.GetNext(var AKey: string; var AValue: Pointer): Boolean;
var
  vKey, vValue: Variant;
begin
  Result := GetNextInternal(vKey, vValue);

  if Result then
  begin
    AKey := vKey;
    AValue := Pointer(Integer(vValue));
  end;
end;

{ TCnStrToVariantHashMap }

procedure TCnStrToVariantHashMap.Add(const AKey: string; AValue: Variant);
begin
  AddInternal(AKey, AValue);
end;

function TCnStrToVariantHashMap.Delete(const AKey: string): Boolean;
begin
  Result := DeleteInternal(AKey);
end;

function TCnStrToVariantHashMap.Find(const AKey: string;
  var AValue: Variant): Boolean;
begin
  Result := FindInternal(Variant(AKey), AValue);
end;

function TCnStrToVariantHashMap.GetNext(var AKey: string;
  var AValue: Variant): Boolean;
var
  vKey: Variant;
begin
  Result := GetNextInternal(vKey, AValue);

  if Result then
    AKey := vKey;
end;

function TCnStrToVariantHashMap.VariantHashCode(AKey: Variant): Integer;
var
  iHashCode, i: Integer;
  HashString: string;
begin
  iHashCode := 0;
  HashString := AKey;

  for i := 1 to Length(HashString) do
    iHashCode := iHashCode shl 5 + Ord(HashString[i]) + iHashCode;

  Result := Abs(iHashCode);
end;

end.
