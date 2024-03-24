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

unit CnActionListHook;
{ |<PRE>
================================================================================
* 软件名称：CnWizards IDE 专家工具包
* 单元名称：ActionList 挂接服务单元
* 单元作者：刘啸（Passion） liuxiao@cnpack.org;
* 备    注：该单元用来实现对 IDE 内部 ActionList 的挂接操作，用户必须先挂接一个
            ActionList，才能对其内部的 Action 进行挂接。当挂接管理器的 Active 为
            False 的时候，所有挂接的 Action 的事件都会暂时恢复。当 ActionList 或
            Action 被释放的时候会自动取消挂接。
* 开发平台：PWin2000Pro + Delphi 5.01
* 兼容测试：PWin9X/2000/XP + Delphi 5/6/7 + C++Builder 5/6
* 本 地 化：该单元中的字符串支持本地化处理方式
* 修改记录：2024.03.15 V1.1
*               增加根据 Action 获取其旧事件的机制供调用
*           2003.07.15 V1.0
*               创建单元
================================================================================
|</PRE>}

interface

{$I CnPack.inc}

uses
  Windows, Messages, SysUtils, Classes, Forms, ActnList, Contnrs,
  CnConsts, CnClasses, CnCompConsts;

type
  TCnActionHookObj = class(TObject)
  {* 用来描述一被挂接的 Action}
  private
    FAction: TAction;

    FNewOnExecute: TNotifyEvent;
    FNewOnUpdate: TNotifyEvent;
    FOldOnExecute: TNotifyEvent;
    FOldOnUpdate: TNotifyEvent;
    procedure SetAction(const Value: TAction);
    procedure SetNewOnExecute(const Value: TNotifyEvent);
    procedure SetNewOnUpdate(const Value: TNotifyEvent);
    procedure SetOldOnExecute(const Value: TNotifyEvent);
    procedure SetOldOnUpdate(const Value: TNotifyEvent);
  protected
    procedure HookAction;
    {* 进行具体的 Action 事件替换操作}
    procedure RestoreAction;
    {* 恢复 Action 的原有事件}
  public
    constructor Create(AAction: TAction; NewOnExecute, NewOnUpdate: TNotifyEvent);
    destructor Destroy; override;

    property Action: TAction read FAction write SetAction;
    property OldOnUpdate: TNotifyEvent read FOldOnUpdate write SetOldOnUpdate;
    property OldOnExecute: TNotifyEvent read FOldOnExecute write SetOldOnExecute;
    property NewOnUpdate: TNotifyEvent read FNewOnUpdate write SetNewOnUpdate;
    property NewOnExecute: TNotifyEvent read FNewOnExecute write SetNewOnExecute;
  end;

//==============================================================================
// ActionList 挂接管理器
//==============================================================================

{ TCnActionListHook }

  THookActionListEvent = procedure(Sender: TObject; ActionList: TActionList) of object;

  TCnActionListHook = class(TCnComponent)
  private
    FActionListList: TList;
    FHookItemList: TObjectList;
    FActive: Boolean;
    FOnAddActionList: THookActionListEvent;
    FOnRemoveActionList: THookActionListEvent;
    procedure SetActive(const Value: Boolean);
    function GetHookedActionList(Index: Integer): TActionList;
    function GetHookedActionListCount: Integer;
    function GetHookedAction(Index: Integer): TAction;
    function GetHookedActionCount: Integer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetActionHookObj(AAction: TAction): TCnActionHookObj;

    procedure DoRemoveActionList(AActionList: TActionList);
    procedure DoAddActionList(AActionList: TActionList);
    procedure UpdateHookedActions;

    procedure GetComponentInfo(var AName, Author, Email, Comment: string); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function IsHooked(AActionList: TActionList): Boolean;
    {* 判断一 ActionList 是否被 Hook}
    function IsActionHooked(AAction: TAction): Boolean;
    {* 判断一 Action 是否被 Hook}
    procedure UnHookActionItems(ActionList: TActionList);
    {* 取消一 ActionList 中的所有 Action 的 Hook}

    procedure HookActionList(AActionList: TActionList);
    {* 供用户调用：挂接一个 ActionList}
    procedure UnHookActionList(AActionList: TActionList);
    {* 供用户调用：取消对一个 ActionList 的挂接}

    function AddActionNotifier(Action: TAction; NewOnExecute, NewOnUpdate:
      TNotifyEvent): Boolean;
    {* 供用户调用：挂接一 Action 的 OnExecute 和 OnUpdate 事件}

    procedure RemoveNotifiler(Action: TAction);
    {* 供用户调用：取消挂接一 Action，恢复其原有的 OnExecute 和 OnUpdate 事件}

    function GetActionOldExecute(Action: TAction): TNotifyEvent;
    {* 获取被挂接的 Action 的旧的执行事件}
    function GetActionOldUpdate(Action: TAction): TNotifyEvent;
    {* 获取被挂接的 Action 的旧的更新事件}

    property Active: Boolean read FActive write SetActive;
    {* 控制本挂接管理器是否有效 }
    property HookedActionListCount: Integer read GetHookedActionListCount;
    {* 返回被挂接的 ActionList 数目 }
    property HookedActionLists[Index: Integer]: TActionList
      read GetHookedActionList;
    {* 返回被挂接的 ActionList }
    property HookedActionCount: Integer read GetHookedActionCount;
    {* 返回被挂接的 Action 数目 }
    property HookedActions[Index: Integer]: TAction read GetHookedAction;
    {* 返回被挂接的 Action }

    property OnRemoveActionList: THookActionListEvent
      read FOnRemoveActionList write FOnRemoveActionList;
    property OnAddActionList: THookActionListEvent
      read FOnAddActionList write FOnAddActionList;
  end;

implementation

//==============================================================================
// ActionList 挂接管理器
//==============================================================================

{ TCnActionListHook }

function TCnActionListHook.AddActionNotifier(Action: TAction; NewOnExecute,
  NewOnUpdate: TNotifyEvent): Boolean;
var
  HookObj: TCnActionHookObj;
begin
  Result := False;
  if (Action <> nil) and (FHookItemList.IndexOf(Action) < 0) then
  begin
    if IsHooked(TActionList(Action.ActionList)) and not IsActionHooked(Action) then
    begin
      HookObj := TCnActionHookObj.Create(Action, NewOnExecute, NewOnUpdate);
      FHookItemList.Add(HookObj);
      if Active then
        HookObj.HookAction;
      Action.FreeNotification(Self);
      Result := True;
    end;
  end;
end;

constructor TCnActionListHook.Create(AOwner: TComponent);
begin
  inherited;
  FActionListList := TList.Create;

  // 不需要控制对 ActionList 的释放。
  FHookItemList := TObjectList.Create;
  FActive := True;
  FOnAddActionList := nil;
  FOnRemoveActionList := nil;
end;

destructor TCnActionListHook.Destroy;
begin
  FHookItemList.Free;
  FActionListList.Free;
  inherited;
end;

procedure TCnActionListHook.DoAddActionList(AActionList: TActionList);
begin
  if Assigned(FOnAddActionList) then
    FOnAddActionList(Self, AActionList);
end;

procedure TCnActionListHook.DoRemoveActionList(AActionList: TActionList);
begin
  if Assigned(FOnRemoveActionList) then
    FOnRemoveActionList(Self, AActionList);
end;

function TCnActionListHook.GetActionHookObj(AAction: TAction): TCnActionHookObj;
var
  I: Integer;
begin
  for I := 0 to FHookItemList.Count - 1 do
  begin
    if TCnActionHookObj(FHookItemList[I]).Action = AAction then
    begin
      Result := TCnActionHookObj(FHookItemList[I]);
      Exit;
    end;
  end;
  Result := nil;
end;

function TCnActionListHook.GetHookedActionList(Index: Integer): TActionList;
begin
  if (Index >= 0) and (Index < FActionListList.Count) then
    Result := TActionList(FActionListList[Index])
  else
    Result := nil;
end;

function TCnActionListHook.GetHookedActionListCount: Integer;
begin
  Result := FActionListList.Count;
end;

procedure TCnActionListHook.HookActionList(AActionList: TActionList);
begin
  if (AActionList <> nil) and not IsHooked(AActionList) then
  begin
    DoAddActionList(AActionList);
    FActionListList.Add(AActionList);
    AActionList.FreeNotification(Self);
  end
end;

function TCnActionListHook.IsHooked(AActionList: TActionList): Boolean;
begin
  Result := (FActionListList.IndexOf(AActionList) >= 0);
end;

function TCnActionListHook.IsActionHooked(AAction: TAction): Boolean;
begin
  Result := GetActionHookObj(AAction) <> nil;
end;

procedure TCnActionListHook.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent is TActionList) then
  begin
    UnHookActionItems(AComponent as TActionList);
    UnHookActionList(AComponent as TActionList);
  end
  else if (Operation = opRemove) and (AComponent is TAction) then
  begin
    RemoveNotifiler(AComponent as TAction);
  end;
end;

procedure TCnActionListHook.RemoveNotifiler(Action: TAction);
var
  HookObj: TCnActionHookObj;
begin
  if IsHooked(TActionList(Action.ActionList)) then
  begin
    if IsActionHooked(Action) then
    begin
      Action.RemoveFreeNotification(Self);
      HookObj := GetActionHookObj(Action);
      HookObj.RestoreAction;
      FHookItemList.Delete(FHookItemList.IndexOf(HookObj));
      HookObj.Free;
    end;
  end;
end;

procedure TCnActionListHook.SetActive(const Value: Boolean);
begin
  FActive := Value;
  UpdateHookedActions;
end;

procedure TCnActionListHook.UnHookActionItems(ActionList: TActionList);
var
  I: Integer;
begin
  for I := 0 to ActionList.ActionCount - 1 do
    if GetActionHookObj(ActionList.Actions[I] as TAction) <> nil then
      RemoveNotifiler(ActionList.Actions[I] as TAction);
end;

procedure TCnActionListHook.UnHookActionList(AActionList: TActionList);
begin
  if IsHooked(AActionList) then
  begin
    DoRemoveActionList(AActionList);
    AActionList.RemoveFreeNotification(Self);
    UnHookActionItems(AActionList);
    FActionListList.Remove(AActionList);
  end;
end;

procedure TCnActionListHook.UpdateHookedActions;
var
  I: Integer;
begin
  if Active then
    for I := 0 to FHookItemList.Count - 1 do
      TCnActionHookObj(FHookItemList[I]).HookAction
  else
    for I := 0 to FHookItemList.Count - 1 do
      TCnActionHookObj(FHookItemList[I]).RestoreAction;
end;

{function TCnActionListHook.AddActionNotifier(const ActionName: String;
  NewOnExecute, NewOnUpdate: TNotifyEvent): Boolean;
begin
  if (FindComponent(ActionName) <> nil) and
    (FindComponent(ActionName) is TAction) then
      Self.AddActionNotifier((FindComponent(ActionName) as TAction),
        NewOnUpdate, NewOnExecute);
end;

procedure TCnActionListHook.RemoveNotifiler(const ActionName: String);
begin
  if (FindComponent(ActionName) <> nil) and
    (FindComponent(ActionName) is TAction) then
      Self.RemoveNotifiler(FindComponent(ActionName) as TAction);
end; }

function TCnActionListHook.GetHookedAction(Index: Integer): TAction;
begin
  if (Index >= 0) and (Index < FHookItemList.Count) then
    Result := TCnActionHookObj(FHookItemList[Index]).Action
  else
    Result := nil;
end;

function TCnActionListHook.GetHookedActionCount: Integer;
begin
  Result := FHookItemList.Count;
end;

procedure TCnActionListHook.GetComponentInfo(var AName, Author, Email,
  Comment: string);
begin
  AName := SCnActionListHookName;
  Author := SCnPack_Zjy;
  Email := SCnPack_ZjyEmail;
  Comment := SCnActionListHookComment;
end;

function TCnActionListHook.GetActionOldExecute(Action: TAction): TNotifyEvent;
var
  Obj: TCnActionHookObj;
begin
  Obj := GetActionHookObj(Action);
  if Obj <> nil then
    Result := Obj.OldOnExecute
  else
    Result := nil;
end;

function TCnActionListHook.GetActionOldUpdate(Action: TAction): TNotifyEvent;
var
  Obj: TCnActionHookObj;
begin
  Obj := GetActionHookObj(Action);
  if Obj <> nil then
    Result := Obj.OldOnUpdate
  else
    Result := nil;
end;

{ TCnActionHookObj }

constructor TCnActionHookObj.Create(AAction: TAction; NewOnExecute,
  NewOnUpdate: TNotifyEvent);
begin
  FAction := AAction;
  FOldOnExecute := AAction.OnExecute;
  FOldOnUpdate := AAction.OnUpdate;
  FNewOnExecute := NewOnExecute;
  FNewOnUpdate := NewOnUpdate;
end;

destructor TCnActionHookObj.Destroy;
begin
  if Self.FAction <> nil then
    Self.RestoreAction;
  inherited;
end;

procedure TCnActionHookObj.HookAction;
begin
  if FAction <> nil then
  begin
    if Assigned(FNewOnExecute) then
      FAction.OnExecute := NewOnExecute;
    if Assigned(FNewOnUpdate) then
      FAction.OnUpdate := NewOnUpdate;
  end;
end;

procedure TCnActionHookObj.RestoreAction;
begin
  if FAction <> nil then
  begin
    FAction.OnExecute := OldOnExecute;
    FAction.OnUpdate := OldOnUpdate;
  end;
end;

procedure TCnActionHookObj.SetAction(const Value: TAction);
begin
  FAction := Value;
end;

procedure TCnActionHookObj.SetNewOnExecute(const Value: TNotifyEvent);
begin
  FNewOnExecute := Value;
end;

procedure TCnActionHookObj.SetNewOnUpdate(const Value: TNotifyEvent);
begin
  FNewOnUpdate := Value;
end;

procedure TCnActionHookObj.SetOldOnExecute(const Value: TNotifyEvent);
begin
  FOldOnExecute := Value;
end;

procedure TCnActionHookObj.SetOldOnUpdate(const Value: TNotifyEvent);
begin
  FOldOnUpdate := Value;
end;

end.
