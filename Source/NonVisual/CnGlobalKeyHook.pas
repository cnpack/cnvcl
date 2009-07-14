{******************************************************************************}
{                       CnPack For Delphi/C++Builder                           }
{                     中国人自己的开放源码第三方开发包                         }
{                   (C)Copyright 2001-2006 CnPack 开发组                       }
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

unit CnGlobalKeyHook;

{* |<PRE>
================================================================================
* 软件名称：系统功能组件包
* 单元名称：实现全局键盘勾子单元
* 单元作者：rarnu(rarnu@cnpack.org)
* 备    注：使用系统API实现的无dll勾子组件
* 开发平台：Windows2003 Server + Delphi2007 up2
* 兼容测试：Windows2000/XP/2003/Vista + Delphi 7/2006/2007/2009
* 本 地 化：该单元中的字符串均符合本地化处理方式
* 单元标识：$Id$
* 修改记录：2008.08.14 V1.0
*                创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  SysUtils, Classes, Messages, Windows, Menus, Forms;

type
  TCnHotKeyItem = class(TCollectionItem)
  private
    FHotKey             : TShortCut;
    FOnExecute          : TNotifyEvent;
    FApplicationToFront : Boolean;
    FID                 : Integer;
    procedure Changed;
    procedure SetHotKey(const Value: TShortCut);
    procedure SetOnExecute(const Value: TNotifyEvent);
    procedure SetApplicationToFront(const Value: Boolean);
    procedure DoExecute;
  protected
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
  published
    {* 是否将 Application 提到最前 }
    property ApplicationToFront: Boolean read FApplicationToFront
      write SetApplicationToFront default True;
    {* 热键键值定义 }
    property HotKey: TShortCut read FHotKey write SetHotKey default 0;
    {* 按下热键时执行的事件 }
    property OnExecute: TNotifyEvent read FOnExecute write SetOnExecute;
  end;
  
  TCnHotKeyCollection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TCnHotKeyItem;
    procedure SetItem(Index: Integer; const Value: TCnHotKeyItem);
  public
    constructor Create(AOwner: TPersistent);
    function Add: TCnHotKeyItem;
    function FindItemID(ID: Integer): TCnHotKeyItem;
    function Insert(Index: Integer): TCnHotKeyItem;
    {* 热键集合 }
    property Items[Index: Integer]: TCnHotKeyItem read GetItem write SetItem;
      default;
  end;

  TCnCustomGlobalKeyHook = class(TComponent)
  private
    FHotKeys  : TCnHotKeyCollection;
    FIDs      : array of Integer;
    FHandle   : THandle;
    FActive   : Boolean;
    procedure SetHotKeys(const Value: TCnHotKeyCollection);
    procedure SetActive(const Value: Boolean);
    procedure WndProc(var Message: TMessage);
  protected
    procedure Changed;
    property HotKeys: TCnHotKeyCollection read FHotKeys write SetHotKeys;
    property Active: Boolean read FActive write SetActive;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

  TCnGlobalKeyHook = class(TCnCustomGlobalKeyHook)
  published
    {* 热键集合 }
    property HotKeys;
    {* 是否允许使用热键激活 }
    property Active;
  end;

implementation

type
  TIDManager = class
  private
    FIDs  : array of Integer;
  public
    function GetAvailableID: Integer;
    procedure ReleaseID(const ID: Integer);
  end;

var
  IDManager : TIDManager;

function TIDManager.GetAvailableID: Integer;
var
  Ok    : Boolean;
  Index : Integer;
begin
  Result := $F000;

  repeat
    Ok := True;

    for Index := Low(FIDs) to High(FIDs) do
    begin
      if FIDs[Index] = Result then
      begin
        Inc(Result);
        Ok := False;
        Break;
      end;
    end;
  until Ok;

  SetLength(FIDs, Length(FIDs)+1);
  FIDs[High(FIDs)] := Result;
end;

procedure TIDManager.ReleaseID(const ID: Integer);
var
  Index : Integer;
begin
  for Index := Low(FIDs) to High(FIDs) do
  begin
    if FIDs[Index] = ID then
    begin
      if Index < High(FIDs) then
        FIDs[Index] := FIDs[High(FIDs)];

      SetLength(FIDs, Length(FIDs)-1);
      Break;
    end;
  end;
end;

procedure TCnHotKeyItem.Changed;
begin
  (TCnHotKeyCollection(Collection).GetOwner as TCnCustomGlobalKeyHook).Changed;
end;

constructor TCnHotKeyItem.Create(Collection: TCollection);
begin
  inherited;
  FID := IDManager.GetAvailableID;
  FApplicationToFront := True;
end;

destructor TCnHotKeyItem.Destroy;
begin
  IDManager.ReleaseID(FID);
  inherited;
end;

procedure TCnHotKeyItem.DoExecute;
begin
  if FApplicationToFront then
    SetForegroundWindow(Application.Handle);
  if Assigned(FOnExecute) then
    FOnExecute(TCnHotKeyCollection(Collection).GetOwner);
end;

function TCnHotKeyItem.GetDisplayName: string;
begin
  if FHotKey <> 0 then
    Result := ShortCutToText(FHotKey)
  else
    Result := inherited GetDisplayName;
end;

procedure TCnHotKeyItem.SetApplicationToFront(const Value: Boolean);
begin
  if Value <> FApplicationToFront then
  begin
    FApplicationToFront := Value;
    Changed;
  end;
end;

procedure TCnHotKeyItem.SetHotKey(const Value: TShortCut);
begin
  if Value <> FHotKey then
  begin
    FHotKey := Value;
    Changed;
  end;
end;

procedure TCnHotKeyItem.SetOnExecute(const Value: TNotifyEvent);
begin
  FOnExecute := Value;
  Changed;
end;

procedure TCnCustomGlobalKeyHook.Changed;
var
  Index     : Integer;
  ShortCut  : TShortCut;
  Modifiers : Cardinal;
begin
  for Index := Low(FIDs) to High(FIDs) do
    UnregisterHotKey(FHandle, FIDs[Index]);
  SetLength(FIDs, 0);

  if FActive and (not (csDesigning in ComponentState)) then
  begin
    for Index := 0 to FHotKeys.Count-1 do
    begin
      if (FHotKeys[Index].HotKey <> 0) and
        (Assigned(FHotKeys[Index].OnExecute) or
        FHotKeys[Index].ApplicationToFront) then
      begin
        SetLength(FIDs, Length(FIDs)+1);
        FIDs[High(FIDs)] := FHotKeys[Index].FID;
        ShortCut := FHotKeys[Index].HotKey;

        Modifiers := 0;
        if (ShortCut and scShift) <> 0 then
        begin
          Modifiers := Modifiers or MOD_SHIFT;
          ShortCut := ShortCut and (not scShift);
        end;
        if (ShortCut and scCtrl) <> 0 then
        begin
          Modifiers := Modifiers or MOD_CONTROL;
          ShortCut := ShortCut and (not scCtrl);
        end;
        if (ShortCut and scAlt) <> 0 then
        begin
          Modifiers := Modifiers or MOD_ALT;
          ShortCut := ShortCut and (not scAlt);
        end;

        if not RegisterHotKey(FHandle, FIDs[High(FIDs)], Modifiers, ShortCut) then
        begin
          SetLength(FIDs, Length(FIDs)-1);
          RaiseLastWin32Error;
        end;
      end;
    end;
  end;
end;

constructor TCnCustomGlobalKeyHook.Create(AOwner: TComponent);
begin
  inherited;

  if not (csDesigning in ComponentState) then
    FHandle := AllocateHWnd(WndProc);

  FActive := True;
  FHotKeys := TCnHotKeyCollection.Create(Self);
end;

destructor TCnCustomGlobalKeyHook.Destroy;
begin
  Active := False;
  FHotKeys.Free;

  if FHandle <> 0 then
    DeallocateHWnd(FHandle);

  inherited;
end;

procedure TCnCustomGlobalKeyHook.SetActive(const Value: Boolean);
begin
  if Value <> FActive then
  begin
    FActive := Value;
    Changed;
  end;
end;

procedure TCnCustomGlobalKeyHook.SetHotKeys(
  const Value: TCnHotKeyCollection);
begin
  FHotKeys.Assign(Value);
end;

procedure TCnCustomGlobalKeyHook.WndProc(var Message: TMessage);
var
  Index : Integer;
begin
  if Message.Msg = WM_HOTKEY then
  begin
    for Index := 0 to FHotKeys.Count-1 do
      if Message.WParam = FHotKeys[Index].FID then
      FHotKeys[Index].DoExecute;
  end else
    Message.Result := DefWindowProc(FHandle, Message.Msg, Message.WParam,
      Message.LParam);
end;

function TCnHotKeyCollection.Add: TCnHotKeyItem;
begin
  Result := inherited Add as TCnHotKeyItem;
end;

constructor TCnHotKeyCollection.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TCnHotKeyItem);
end;

function TCnHotKeyCollection.FindItemID(ID: Integer): TCnHotKeyItem;
begin
  Result := inherited FindItemID(ID) as TCnHotKeyItem;
end;

function TCnHotKeyCollection.GetItem(Index: Integer): TCnHotKeyItem;
begin
  Result := inherited Items[Index] as TCnHotKeyItem;
end;

function TCnHotKeyCollection.Insert(Index: Integer): TCnHotKeyItem;
begin
  Result := inherited Insert(Index) as TCnHotKeyItem;
end;

procedure TCnHotKeyCollection.SetItem(Index: Integer;
  const Value: TCnHotKeyItem);
begin
  inherited Items[Index] := Value;
end;

initialization
  IDManager := TIDManager.Create;

finalization
  IDManager.Free;
  
end.

