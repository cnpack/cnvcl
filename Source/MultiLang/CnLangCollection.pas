{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2024 CnPack 开发组                       }
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

unit CnLangCollection;
{* |<PRE>
================================================================================
* 软件名称：CnPack 多语包
* 单元名称：语言条目描述及其列表类单元
* 单元作者：CnPack 开发组 (master@cnpack.org)
* 备    注：该单元实现了语言条目描述及其列表类
* 开发平台：PWin2000 + Delphi 5.0
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：2003.12.13 V1.1
*               将 DefaultFont 的属性来源移动到 LanguageItem 中，
*               每种语言可设置一单独的 DefaultFont，可流化到 Storage 中
*           2003.08.20 V1.0
*               创建单元，实现功能
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Windows, Graphics, CnIniStrUtils;

type
{$IFDEF UNICODE}
  TCnLangString = string;
{$ELSE}
  TCnLangString = WideString;
{$ENDIF}

  TCnLanguageItem = class (TCollectionItem)
  {* 语言条目描述类 }
  private
    FLanguageName: TCnLangString;
    FAuthor: TCnLangString;
    FAuthorEmail: TCnLangString;
    FAbbreviation: TCnLangString;
    FLanguageFileName: TCnLangString;
    FLanguageDirName: TCnLangString;
    FLanguageID: LongWord;
    FOnLanguageIDChanged: TNotifyEvent;
    FDefaultFont: TFont;
    function GetAbbreviation: TCnLangString;
    function GetLanguageName: TCnLangString;
    procedure SetAbbreviation(Value: TCnLangString);
    function GetLanguageFileName: TCnLangString;
    procedure SetLanguageFileName(const Value: TCnLangString);
    procedure SetDefaultFont(Value: TFont);
    function GetDefaultFontStr: TCnLangString;
    function GetLanguageDirName: TCnLangString;
    procedure SetLanguageDirName(const Value: TCnLangString);
  protected
    procedure SetAuthor(Value: TCnLangString);
    procedure SetAuthorEmail(Value: TCnLangString);
    procedure SetLanguageID(Value: LongWord);
    procedure SetLanguageName(Value: TCnLangString);
    procedure DoLanguageIDChanged; virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    {* 赋值过程}
    function IsValidLanguageID(ALanguageID: LongWord): Boolean;
    {* 判断一 ID 是否是合法的语言 ID }
    property OnLanguageIDChanged: TNotifyEvent read FOnLanguageIDChanged
      write FOnLanguageIDChanged;
    {* 当语言 ID 发生改变时触发 }
    property DefaultFontStr: TCnLangString read GetDefaultFontStr;
    {* 默认的语言 Font 的 Str}
  published
    property Abbreviation: TCnLangString read GetAbbreviation write SetAbbreviation;
    {* 该语言的缩写，在设置 LanguageID 时会自动设置 }
    property Author: TCnLangString read FAuthor write SetAuthor;
    {* 该语言的翻译条目的作者 }
    property AuthorEmail: TCnLangString read FAuthorEmail write SetAuthorEmail;
    {* 该语言的翻译条目的作者的电子邮件地址 }
    property LanguageID: LongWord read FLanguageID write SetLanguageID;
    {* 该语言的 ID 号 }
    property LanguageName: TCnLangString read GetLanguageName write SetLanguageName;
    {* 该语言的名称，在设置 LanguageID 时会自动设置 }
    property LanguageFileName: TCnLangString read GetLanguageFileName
      write SetLanguageFileName;
    {* 返回可用的保存文件名供多语言存储组件使用，以文件方式存储时其结果有效 }
    property LanguageDirName: TCnLangString read GetLanguageDirName
      write SetLanguageDirName;
    {* 返回可用的保存多语的目录名，以目录方式存储时其结果有效 }
    property DefaultFont: TFont read FDefaultFont write SetDefaultFont;
    {* 该语言的默认 Font，内部使用 FontStr 存储 }
  end;

  TCnLanguageCollection = class (TOwnedCollection)
  {* 管理语言列表 }
  private
    function GetItems(Index: Integer): TCnLanguageItem;
    procedure SetItems(Index: Integer; Value: TCnLanguageItem);
  public
    constructor Create(AOwner: TPersistent); reintroduce;
    function Add: TCnLanguageItem;
    {* 增加一新的语言条目 }
    function Find(ALanguageID: LongWord): Integer; overload;
    {* 根据语言 ID 查找语言条目 }
    function Find(ALanguageName: TCnLangString): Integer; overload;
    {* 根据语言名称查找语言条目 }
    property Items[Index: Integer]: TCnLanguageItem read GetItems write
      SetItems; default;
    {* 返回某条语言条目 }
  end;

implementation

uses
  CnLangConsts, CnLangUtils, CnLangStorage;

{***************************** TCnLanguageItem ******************************}

procedure TCnLanguageItem.DoLanguageIDChanged;
begin
  if Assigned(FOnLanguageIDChanged) then
    FOnLanguageIDChanged(Self);
end;

function TCnLanguageItem.GetAbbreviation: TCnLangString;
begin
  Result := FAbbreviation;
end;

function TCnLanguageItem.GetLanguageFileName: TCnLangString;
begin
  Result := FLanguageFileName;
end;

function TCnLanguageItem.GetLanguageName: TCnLangString;
begin
  Result := FLanguageName;
end;

function TCnLanguageItem.IsValidLanguageID(ALanguageID: LongWord): Boolean;
begin
  try
    Result := CnLanguages.IndexOf(ALanguageID) >= 0;
  except
    raise ELanguageStorageError.Create(SCnErrorInCheckingLanguage);
  end;
end;

procedure TCnLanguageItem.SetAbbreviation(Value: TCnLangString);
begin
  if FAbbreviation <> Value then
    FAbbreviation := Value;
end;

procedure TCnLanguageItem.SetAuthor(Value: TCnLangString);
begin
  if FAuthor <> Value then
  begin
    FAuthor := Value;
    
  end;
end;

procedure TCnLanguageItem.SetAuthorEmail(Value: TCnLangString);
begin
  if FAuthorEmail <> Value then
  begin
    FAuthorEmail := Value;

  end;
end;

procedure TCnLanguageItem.SetLanguageFileName(const Value: TCnLangString);
begin
  FLanguageFileName := Value;
end;

procedure TCnLanguageItem.SetLanguageID(Value: LongWord);
begin
  if FLanguageID <> Value then
  begin
    if Value = 0 then
    begin
      FLanguageID := Value;
      LanguageName := '';
      Abbreviation := '';
      LanguageFileName := '';

      LanguageDirName := '';
      DoLanguageIDChanged;
    end
    else if IsValidLanguageID(Value) then
    begin
      FLanguageID := Value;
      LanguageName := CnLanguages.NameFromLocaleID[Value];
      Abbreviation := CnLanguages.Ext[CnLanguages.IndexOf(Value)];
      if LanguageFileName = '' then
        LanguageFileName := Abbreviation;

      LanguageDirName := IntToStr(Value);
      DoLanguageIDChanged;
    end
    else
      raise ELanguageStorageError.CreateFmt(SCnInvalidLanguageIDError, [Value]);
  end;
end;

procedure TCnLanguageItem.SetLanguageName(Value: TCnLangString);
begin
  if FLanguageName <> Value then
  begin
    FLanguageName := Value;

  end;
end;

procedure TCnLanguageItem.SetDefaultFont(Value: TFont);
begin
  FDefaultFont.Assign(Value);
end;

constructor TCnLanguageItem.Create(Collection: TCollection);
begin
  inherited;
  FDefaultFont := TFont.Create;
end;

destructor TCnLanguageItem.Destroy;
begin
  FDefaultFont.Free;
  inherited;
end;

procedure TCnLanguageItem.Assign(Source: TPersistent);
begin
  if Source is TCnLanguageItem then
  begin
    FLanguageName := TCnLanguageItem(Source).LanguageName;
    FAuthor := TCnLanguageItem(Source).Author;
    FAuthorEmail := TCnLanguageItem(Source).AuthorEmail;
    FAbbreviation  := TCnLanguageItem(Source).Abbreviation;
    FLanguageFileName  := TCnLanguageItem(Source).LanguageFileName;
    FLanguageDirName  := TCnLanguageItem(Source).LanguageDirName;
    FLanguageID  := TCnLanguageItem(Source).LanguageID;    
  end
  else
    inherited;
end;

function TCnLanguageItem.GetDefaultFontStr: TCnLangString;
begin
  Result := FontToString(FDefaultFont);
end;

constructor TCnLanguageCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TCnLanguageItem);
end;

function TCnLanguageCollection.Add: TCnLanguageItem;
begin
  Result := TCnLanguageItem(inherited Add);
end;

function TCnLanguageCollection.Find(ALanguageID: LongWord): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Items[i].LanguageID = ALanguageID then
    begin
      Result := i;
      Exit;
    end;
end;

function TCnLanguageCollection.Find(ALanguageName: TCnLangString): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 0 to Count - 1 do
    if Items[i].LanguageName = ALanguageName then
    begin
      Result := i;
      Exit;
    end;
end;

function TCnLanguageCollection.GetItems(Index: Integer): TCnLanguageItem;
begin
  Result := TCnLanguageItem(inherited Items[Index]);
end;

procedure TCnLanguageCollection.SetItems(Index: Integer; Value: TCnLanguageItem);
begin
  inherited Items[Index] := Value;
end;

function TCnLanguageItem.GetLanguageDirName: TCnLangString;
begin
  Result := FLanguageDirName;
end;

procedure TCnLanguageItem.SetLanguageDirName(const Value: TCnLangString);
begin
  FLanguageDirName := Value;
end;

end.
