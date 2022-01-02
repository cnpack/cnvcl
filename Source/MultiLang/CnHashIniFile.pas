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

unit CnHashIniFile;
{* |<PRE>
================================================================================
* 软件名称：CnPack
* 单元名称：CnHashIniFile 实现单元
* 单元作者：Circus Monkey
* 备    注：该单元为 CnHashIniFile 的实现单元。
* 开发平台：EWinXPPro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 修改记录：v1.0   2004/1/19 by Circus Monkey
*              本控件 TCnHashIniFile 是基于哈希表工作的。它和 CnHashLangStorage
*              很相似。但是它支持多个 Sections。可以比较好的兼容 INIFile。根据
*              测试，处理大文件的速度比 CnHashLangStorage 稍慢，但远快于 IniFile
*              慢的原因是，所有 Sections 的信息没有保存在 HashMap 中。但事实上
*              差别非常的小。
* 已知问题：- 关闭时如果文件内容有改变，会更新整个文件。而非局部更新。
*           - 在保存时，会忽略所有的空格行，以及;---开头的注释行。
* 说    明：如果要实现 Unicode 兼容，只要替换所有的 string 成 WideString。然后
*           使用 TTntStringList 替换 TStringList, 并引用 TntClasses 即可。
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, IniFiles, CnHashMap;

type
  TCnCustomHashIniSection = class(TObject)
  private
    FSection: string;
    FHashMap: TCnStrToStrHashMap;
  protected
    procedure InitHashMap;
    procedure UpdateFile(AList: TStringList);
    function GetString(Name: string; var Value: string): Boolean;
    procedure SetString(Name, Value: string);
  public
    property Section: string read FSection;
    constructor Create(const Section: string);
    destructor Destroy; override;
  end;

  TCnCustomHashIniFile = class(TObject)
  private
    FFileName: string;
    FUpdated: Boolean;

  protected
    FMapList: TStringList;
    function InitFile: Boolean;
    procedure UpdateFile;
    function GetSection(const Section: string): TCnCustomHashIniSection;
  public
    property FileName: string read FFileName;
    constructor Create(const FileName: string);
    destructor Destroy; override;
  end;

  TCnHashIniFile = class(TCnCustomHashIniFile)
  public
    function ReadString(const Section, Ident, Default: string): string;
    {* 读取一个字符串, 和 TIniFile.ReadString 的是使用方法完全一致 }
    function ReadInteger(const Section, Ident: string; Default: LongInt): LongInt;
    {* 读取一个整型, 和 TIniFile.ReadInteger 的是使用方法完全一致 }
    function ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
    {* 读取一个布尔量, 和 TIniFile.ReadBool 的是使用方法完全一致 }
    procedure WriteString(const Section, Ident, Value: string);
    {* 保存一个字符串, 和 TIniFile.WriteString 的是使用方法完全一致 }
    procedure WriteInteger(const Section, Ident: string; Value: LongInt);
    {* 保存一个整型, 和 TIniFile.WriteInteger 的是使用方法完全一致 }
    procedure WriteBool(const Section, Ident: string; Value: Boolean);
    {* 保存一个布尔量, 和 TIniFile.WriteBool 的是使用方法完全一致 }
  end;

implementation

const
  _SCnCRLF = #13#10;
  _SCnBR = '<BR>';
  _DefDelimeter = '.';
  _DefEqual = '=';
  _ListLength = 1024;
  _IncSize = 2;

function TCnCustomHashIniSection.GetString(Name: string; var Value: string): Boolean;
begin
  Result := False;
  if Assigned(FHashMap) then
  begin
    Result := FHashMap.Find(Name, Value);
    if Result then
      Value := StringReplace(Value, _SCnBR, _SCnCRLF, [rfReplaceAll, rfIgnoreCase])
    else
      Value := '';
  end;
end;

procedure TCnCustomHashIniSection.SetString(Name, Value: string);
var
  myValue: string;
begin
  if Assigned(FHashMap) then
  begin
    if FHashMap.Find(Name, myValue) then
      FHashMap.Delete(Name);
    FHashMap.Add(Name, StringReplace(Value, _SCnCRLF, _SCnBR, [rfReplaceAll, rfIgnoreCase]));
  end;
end;

procedure TCnCustomHashIniSection.InitHashMap;
begin
  if Assigned(FHashMap) then
    FreeAndNil(FHashMap);
  FHashMap := TCnStrToStrHashMap.Create(_ListLength, _IncSize);
end;

procedure TCnCustomHashIniSection.UpdateFile(AList: TStringList);
var
  Key, Value: string;
  List: TStringList;
begin
  if Assigned(FHashMap) then
  begin
    List := TStringList.Create;
    try
      FHashMap.StartEnum;
      while FHashMap.GetNext(Key, Value) do
        if Key = '' then Continue else
        List.Add(Key + _DefEqual + Value);

      List.Sort;
      List.Insert(0, '[' + FSection + ']');
      AList.AddStrings(List);
    finally
      List.Free;
    end;
  end;
end;

// C: 2003-04-19  M: 2004-01-19
constructor TCnCustomHashIniSection.Create(const Section: string);
begin
  FSection := Section;
  InitHashMap;
end;

// C: 2003-04-19  M: 2004-01-19
destructor TCnCustomHashIniSection.Destroy;
begin
  // update file
  if Assigned(FHashMap) then FHashMap.Free;
  inherited;
end;

{**************************** TCnCustomHashIniFile ****************************}

constructor TCnCustomHashIniFile.Create(const FileName: string);
begin
  FFileName := FileName;
  FMapList := TStringList.Create;
  InitFile;
end;

destructor TCnCustomHashIniFile.Destroy;
begin
  UpdateFile;
//  if Assigned(FHashMap) then FHashMap.Free;
  FMapList.Free;
  inherited;
end;

{$WARNINGS OFF}
function TCnCustomHashIniFile.InitFile: Boolean;
var
  List: TStringList;
  Section: TCnCustomHashIniSection;
  i, EPos, Len: Integer;
  S, SectionN: string;
begin
  Result := True;
  //
  List := TStringList.Create;
  try
    List.LoadFromFile(FFileName);
  except
    Result := False;
    List.Free;
    Exit;
  end;

  for i := 0 to List.Count - 1 do
  begin
    S := Trim(List[i]);
    // Check, if this line contains a section tag.
    Len := Length(S);
    if (Len > 1) and (S[1] = '[') then
    begin
      SectionN := S;
      Delete(SectionN, 1, 1);         // remove '['
      EPos := Pos(']', SectionN);
      if EPos > 0 then
        Delete(SectionN, EPos, Len - EPos);
      SectionN := Trim(SectionN);

      Section := TCnCustomHashIniSection.Create(SectionN);
      FMapList.AddObject(SectionN, Section);
      Continue;
    end;

    if FMapList.Count = 0 then Continue;
    EPos := Pos(_DefEqual, S);

    if EPos > 0 then
      Section.SetString(Copy(S, 1, EPos - 1), Copy(S, EPos + 1, Length(S) - EPos)) else
//      FHashMap.Add(S, '');//Copy(S, 1, EPos - 1), '');

  end;
  List.Free;
end;
{$WARNINGS ON}

procedure TCnCustomHashIniFile.UpdateFile;
var
  Section: TCnCustomHashIniSection;
  List: TStringList;
begin
  if not FUpdated then Exit;

  List := TStringList.Create;
  try
    while (FMapList.Count > 0) do
    begin
      Section := TCnCustomHashIniSection(FMapList.Objects[0]);
      Section.UpdateFile(List);
      FMapList.Delete(0);
      Section.Free;
    end;
    List.SaveToFile(FFileName);
  finally
    List.Free;
  end;
end;

function TCnCustomHashIniFile.GetSection(const Section: string): TCnCustomHashIniSection;
var
  ID: Integer;
begin
  ID := FMapList.IndexOf(Section);
  if ID <> -1 then
    Result := TCnCustomHashIniSection(FMapList.Objects[ID])
  else
    Result := nil;
end;

function TCnHashIniFile.ReadString(const Section, Ident, Default: string): string;
var
  aSection: TCnCustomHashIniSection;
begin
  aSection := GetSection(Section);
  if aSection <> nil then
  begin
    if not aSection.GetString(Ident, Result) then
      Result := Default;
  end;
end;

function TCnHashIniFile.ReadInteger(const Section, Ident: string; Default: LongInt): LongInt;
var
  IntStrW: string;
begin
  IntStrW := ReadString(Section, Ident, '');
  if (Length(IntStrW) > 2) and (IntStrW[1] = '0') and
    ((IntStrW[2] = 'X') or (IntStrW[2] = 'x')) then
    IntStrW := '$' + Copy(IntStrW, 3, Maxint);
  Result := StrToIntDef(IntStrW, Default);
end;

function TCnHashIniFile.ReadBool(const Section, Ident: string; Default: Boolean): Boolean;
begin
  Result := ReadInteger(Section, Ident, Ord(Default)) <> 0;
end;

procedure TCnHashIniFile.WriteString(const Section, Ident, Value: string);
var
  aSection: TCnCustomHashIniSection;
begin
  aSection := GetSection(Section);
  if aSection <> nil then
  begin
    aSection.SetString(Ident, Value);
    FUpdated := True;  // if update = False then skip UpdateFile
  end;
end;

procedure TCnHashIniFile.WriteInteger(const Section, Ident: string; Value: LongInt);
begin
  WriteString(Section, Ident, IntToStr(Value));
end;

procedure TCnHashIniFile.WriteBool(const Section, Ident: string; Value: Boolean);
const
  Values: array[Boolean] of string = ('0', '1');
begin
  WriteString(Section, Ident, Values[Value]);
end;

end.
